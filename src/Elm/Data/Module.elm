module Elm.Data.Module exposing
    ( Module, ModuleType(..)
    , map
    , unalias, exposes, imports, findModuleOfVar
    )

{-| Module information (corresponds to a single .elm file).
Name, imports, contents, etc.

@docs Module, ModuleType
@docs map
@docs unalias, exposes, imports, findModuleOfVar

-}

import Dict exposing (Dict)
import Dict.Extra as Dict
import Elm.Compiler.Error exposing (DesugarError(..))
import Elm.Data.Declaration as Declaration exposing (Declaration)
import Elm.Data.Exposing exposing (ExposedItem(..), Exposing(..))
import Elm.Data.FilePath exposing (FilePath)
import Elm.Data.Import exposing (Import)
import Elm.Data.ModuleName exposing (ModuleName)
import Elm.Data.Qualifiedness exposing (PossiblyQualified(..))
import Elm.Data.VarName exposing (VarName)
import List.NonEmpty
import Maybe.Extra
import Result.Extra


{-| -}
type alias Module expr annotation qualifiedness =
    { -- TODO somewhere check that dependencies' exposing lists contain only what's in that module's exposing list
      imports : Dict ModuleName Import
    , name : ModuleName
    , filePath : FilePath
    , declarations : Dict VarName (Declaration expr annotation qualifiedness)
    , type_ : ModuleType
    , exposing_ : Exposing
    }


{-|

    module Main exposing ...
    --> PlainModule

    port module Main exposing ...
    --> PortModule

    effect module Main where ... exposing ...
    --> EffectModule

-}
type ModuleType
    = PlainModule
    | PortModule
    | EffectModule


{-| Does this module import this module name?
(This doesn't take the `as ...` part of the import line into consideration.)
-}
imports : ModuleName -> Module expr annotation qualifiedness -> Bool
imports moduleName module_ =
    Dict.member moduleName module_.imports


{-| Does this module expose this variable name?
-}
exposes : VarName -> Module expr annotation qualifiedness -> Bool
exposes varName module_ =
    let
        isInDeclarations =
            -- TODO this means constructors will have to be in the module_.declarations
            Dict.member varName module_.declarations
    in
    case module_.exposing_ of
        ExposingAll ->
            isInDeclarations

        ExposingSome items ->
            List.NonEmpty.any
                (\exposedItem ->
                    case exposedItem of
                        ExposedValue value ->
                            value == varName

                        ExposedType type_ ->
                            -- TODO check this code after we have custom types in Frontend.Expr.
                            type_ == varName

                        ExposedTypeAndAllConstructors _ ->
                            {- TODO when we have custom types in Frontend.Expr,
                               return if the varName is in the type constructors.
                            -}
                            False
                )
                items
                && isInDeclarations


{-| Reverses the aliasing in import statements for a single module name.

Given `import Foo as F`:

    unalias module_ "F"
    --> Just "Foo"

    unalias module_ "Foo"
    --> Nothing

    unalias module_ "Foox"
    --> Nothing

-}
unalias : Module expr annotation qualifiedness -> ModuleName -> Maybe ModuleName
unalias thisModule moduleName =
    thisModule.imports
        |> Dict.find (\_ dep -> dep.as_ == Just moduleName)
        |> Maybe.map (Tuple.second >> .moduleName)


{-| Apply a function to all the expressions inside the module.
-}
map :
    (exprA -> exprB)
    -> (Maybe annotationA -> Maybe annotationB)
    -> (qualifiednessA -> qualifiednessB)
    -> Module exprA annotationA qualifiednessA
    -> Module exprB annotationB qualifiednessB
map fnExpr fnAnnotation fnQualifiedness module_ =
    { imports = module_.imports
    , name = module_.name
    , filePath = module_.filePath
    , declarations =
        Dict.map
            (always <|
                Declaration.map
                    fnExpr
                    fnAnnotation
                    fnQualifiedness
            )
            module_.declarations
    , type_ = module_.type_
    , exposing_ = module_.exposing_
    }


{-| We have roughly these options:

  - bar = >baz< (baz being defined elsewhere in this module)
  - import Foo exposing (baz); bar = >baz<
  - import Foo; bar = >Foo.baz<
  - import Foo as F; bar = >F.baz<

In all these cases we need to find the full unaliased module name of the var.

There are two possible errors here:

  - VarNameNotFound: you used var name that can't be found
  - AmbiguousName: you used a name that is imported/defined more than once

TODO "module name not found" somewhere... maybe here, maybe parsing? dunno yet...

-}
findModuleOfVar :
    Dict ModuleName (Module expr annotation qualifiedness)
    -> Module expr annotation qualifiedness
    -> { qualifiedness : PossiblyQualified, name : VarName }
    -> Result DesugarError ModuleName
findModuleOfVar modules thisModule var =
    unqualifiedVarInThisModule thisModule var
        |> Maybe.Extra.orElseLazy (\() -> unqualifiedVarInImportedModule modules thisModule var)
        |> Maybe.Extra.orElseLazy (\() -> qualifiedVarInImportedModule modules var)
        |> Maybe.Extra.orElseLazy (\() -> qualifiedVarInAliasedModule modules thisModule var)
        |> Result.fromMaybe
            (VarNameNotFound
                { var = var
                , insideModule = thisModule.name
                }
            )
        |> Result.Extra.join


unqualifiedVarInThisModule :
    Module expr annotation qualifiedness
    -> { qualifiedness : PossiblyQualified, name : VarName }
    -> Maybe (Result DesugarError ModuleName)
unqualifiedVarInThisModule thisModule { qualifiedness, name } =
    if
        (qualifiedness == PossiblyQualified Nothing)
            && Dict.member name thisModule.declarations
    then
        Just (Ok thisModule.name)

    else
        Nothing


unqualifiedVarInImportedModule :
    Dict ModuleName (Module expr annotation qualifiedness)
    -> Module expr annotation qualifiedness
    -> { qualifiedness : PossiblyQualified, name : VarName }
    -> Maybe (Result DesugarError ModuleName)
unqualifiedVarInImportedModule modules thisModule { qualifiedness, name } =
    if qualifiedness == PossiblyQualified Nothing then
        -- find a module which exposes that var
        let
            acceptableImports =
                thisModule.imports
                    |> Dict.values
                    |> List.filter
                        (\import_ ->
                            Dict.get import_.moduleName modules
                                |> Maybe.map (exposes name)
                                |> Maybe.withDefault False
                        )
        in
        case acceptableImports of
            [] ->
                Nothing

            [ acceptableImport ] ->
                Just (Ok acceptableImport.moduleName)

            _ ->
                Just
                    (Err
                        (AmbiguousName
                            { name = name
                            , insideModule = thisModule.name
                            , possibleModules = List.map .moduleName acceptableImports
                            }
                        )
                    )

    else
        Nothing


{-| We don't think about module `as` aliasing here.
-}
qualifiedVarInImportedModule :
    Dict ModuleName (Module expr annotation qualifiedness)
    -> { qualifiedness : PossiblyQualified, name : VarName }
    -> Maybe (Result DesugarError ModuleName)
qualifiedVarInImportedModule modules { qualifiedness, name } =
    let
        (PossiblyQualified maybeModule) =
            qualifiedness
    in
    maybeModule
        |> Maybe.andThen (\m -> Dict.get m modules)
        |> Maybe.andThen
            (\module__ ->
                if Dict.member name module__.declarations then
                    Just (Ok module__.name)

                else
                    Nothing
            )


qualifiedVarInAliasedModule :
    Dict ModuleName (Module expr annotation qualifiedness)
    -> Module expr annotation qualifiedness
    -> { qualifiedness : PossiblyQualified, name : VarName }
    -> Maybe (Result DesugarError ModuleName)
qualifiedVarInAliasedModule modules thisModule { qualifiedness, name } =
    let
        (PossiblyQualified maybeModule) =
            qualifiedness

        unaliasedModuleName =
            maybeModule
                |> Maybe.andThen (unalias thisModule)
    in
    qualifiedVarInImportedModule
        modules
        { qualifiedness = PossiblyQualified unaliasedModuleName
        , name = name
        }
