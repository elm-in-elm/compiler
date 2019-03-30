module Stage.Desugar exposing (desugar)

import AST.Canonical as Canonical
import AST.Frontend as Frontend
import Basics.Extra exposing (flip)
import Common
import Common.Types
    exposing
        ( Dict_
        , Module
        , ModuleName
        , Modules
        , Project
        , TopLevelDeclaration
        , VarName
        )
import Dict.Any exposing (AnyDict)
import Error exposing (DesugarError(..), Error(..))
import Extra.Dict.Any
import Maybe.Extra


desugar : Project Frontend.ProjectFields -> Result Error (Project Canonical.ProjectFields)
desugar p =
    p.modules
        |> resultMapDict Common.moduleNameToString (desugarModule p.modules)
        |> Result.map
            (\modules ->
                { elmJson = p.elmJson
                , mainFilePath = p.mainFilePath
                , mainModuleName = p.mainModuleName
                , sourceDirectory = p.sourceDirectory
                , modules = modules
                }
            )
        |> Result.mapError DesugarError


desugarModule : Modules Frontend.Expr -> Module Frontend.Expr -> Result DesugarError (Module Canonical.Expr)
desugarModule modules thisModule =
    thisModule.topLevelDeclarations
        |> resultMapDict Common.varNameToString (desugarTopLevelDeclaration modules thisModule)
        |> Result.map
            (\topLevelDeclarations ->
                { dependencies = thisModule.dependencies
                , name = thisModule.name
                , filePath = thisModule.filePath
                , type_ = thisModule.type_
                , exposing_ = thisModule.exposing_
                , topLevelDeclarations = topLevelDeclarations
                }
            )


{-| Roughly: Dict.Any.map toResult >> Result.Extra.combine
We might need to make the function accept keys if there arises a need for it.
-}
resultMapDict : (k -> comparable) -> (v -> Result x v2) -> AnyDict comparable k v -> Result x (AnyDict comparable k v2)
resultMapDict toComparable fn dict =
    dict
        |> Dict.Any.toList
        -- This following line is a mouthful. It uses the Result-producing fn on the second part of the tuple,
        -- and in the same loop does what Result.Extra.combine would did with that second part of the tuple:
        -- List (a, Result x b) -> Result x (List (a,b))
        |> List.foldr (\( a, b ) acc -> Result.map2 (\b_ acc_ -> ( a, b_ ) :: acc_) (fn b) acc) (Ok [])
        |> Result.map (Dict.Any.fromList toComparable)


desugarTopLevelDeclaration : Modules Frontend.Expr -> Module Frontend.Expr -> TopLevelDeclaration Frontend.Expr -> Result DesugarError (TopLevelDeclaration Canonical.Expr)
desugarTopLevelDeclaration modules thisModule d =
    desugarExpr modules thisModule d.body
        |> Result.map
            (\body ->
                { name = d.name
                , module_ = d.module_
                , body = body
                }
            )


{-| TODO Is it possible to make this a pipeline of small passes, even though
they change the type? Ideally we wouldn't smush them together into one function
but keep them separate!

Possibly if each small pass has a "magic" function that deals with the rest
of cases so that the pass can only deal with the interesting Expr constructor...

    exampleSmallPass : (F.Expr -> Result err C.Expr) -> F.Expr -> Result err C.Expr
    exampleSmallPass recurse expr =
        case expr of
            F.Literal literal ->
                Ok (C.Literal literal)

            _ ->
                recurse expr

Maybe it will have to return Maybes so that the algorithm then knows when to
stop trying?

Also, when combining those, can we somehow use the Transform library?

---

Current passes done in this function:

    - Var VarName -> Var (ModuleName, VarName)

-}
desugarExpr : Modules Frontend.Expr -> Module Frontend.Expr -> Frontend.Expr -> Result DesugarError Canonical.Expr
desugarExpr modules thisModule expr =
    let
        desugarExpr_ =
            desugarExpr modules thisModule
    in
    case expr of
        Frontend.Literal literal ->
            Ok (Canonical.Literal literal)

        Frontend.Var ( maybeModuleName, varName ) ->
            findModuleOfVar modules thisModule maybeModuleName varName
                |> Result.fromMaybe (VarNotInEnvOfModule ( maybeModuleName, varName ) thisModule.name)
                |> Result.map (\moduleName -> Canonical.Var ( moduleName, varName ))

        Frontend.Argument varName ->
            Ok (Canonical.Argument varName)

        Frontend.Plus e1 e2 ->
            Result.map2 Canonical.Plus
                (desugarExpr_ e1)
                (desugarExpr_ e2)

        Frontend.Lambda { arguments, body } ->
            desugarExpr modules thisModule body
                |> Result.map (curryLambda arguments)


{-| Convert a multi-arg lambda into multiple single-arg lambdas.

    Frontend.Lambda [ arg1, arg2 ] body

    -->
    Canonical.Lambda arg1 (Canonical.Lambda arg2 body)

-}
curryLambda : List VarName -> Canonical.Expr -> Canonical.Expr
curryLambda arguments body =
    List.foldr
        (\argument body_ ->
            Canonical.Lambda
                { argument = argument
                , body = body_
                }
        )
        body
        arguments


{-| We have roughly these options:

  - bar = >baz< (baz being defined elsewhere in this module)
  - import Foo exposing (baz); bar = >baz<
  - import Foo; bar = >Foo.baz<
  - import Foo as F; bar = >F.baz<

In all these cases we need to find the full unaliased module name of the var.

-}
findModuleOfVar : Modules Frontend.Expr -> Module Frontend.Expr -> Maybe ModuleName -> VarName -> Maybe ModuleName
findModuleOfVar modules thisModule maybeModuleName varName =
    -- TODO test all these
    {- TODO does this allow for some collisions by "returning early"?
       Should we check that exactly one is Just and the others are Nothing?
    -}
    unqualifiedVarInThisModule thisModule maybeModuleName varName
        |> Maybe.Extra.orElseLazy (\() -> unqualifiedVarInImportedModule modules thisModule maybeModuleName varName)
        |> Maybe.Extra.orElseLazy (\() -> qualifiedVarInImportedModule modules maybeModuleName varName)
        |> Maybe.Extra.orElseLazy (\() -> qualifiedVarInAliasedModule modules thisModule maybeModuleName varName)


unqualifiedVarInThisModule : Module Frontend.Expr -> Maybe ModuleName -> VarName -> Maybe ModuleName
unqualifiedVarInThisModule thisModule maybeModuleName varName =
    if maybeModuleName == Nothing && Dict.Any.member varName thisModule.topLevelDeclarations then
        Just thisModule.name

    else
        Nothing


unqualifiedVarInImportedModule : Modules Frontend.Expr -> Module Frontend.Expr -> Maybe ModuleName -> VarName -> Maybe ModuleName
unqualifiedVarInImportedModule modules thisModule maybeModuleName varName =
    if maybeModuleName == Nothing then
        -- find a module which exposes that var
        thisModule.dependencies
            |> Extra.Dict.Any.find
                (\_ dependency ->
                    Dict.Any.get dependency.moduleName modules
                        |> Maybe.map (Common.exposes varName modules)
                        |> Maybe.withDefault False
                )
            |> Maybe.map (\( k, v ) -> v.moduleName)

    else
        Nothing


qualifiedVarInImportedModule : Modules Frontend.Expr -> Maybe ModuleName -> VarName -> Maybe ModuleName
qualifiedVarInImportedModule modules maybeModuleName varName =
    maybeModuleName
        |> Maybe.andThen (flip Dict.Any.get modules)
        |> Maybe.andThen
            (\module_ ->
                if Dict.Any.member varName module_.topLevelDeclarations then
                    Just maybeModuleName

                else
                    Nothing
            )
        |> Maybe.withDefault Nothing


qualifiedVarInAliasedModule : Modules Frontend.Expr -> Module Frontend.Expr -> Maybe ModuleName -> VarName -> Maybe ModuleName
qualifiedVarInAliasedModule modules thisModule maybeModuleName varName =
    let
        unaliasedModuleName =
            Maybe.andThen (Common.unalias thisModule) maybeModuleName
    in
    -- Reusing the existing functionality. TODO is this a good idea?
    qualifiedVarInImportedModule modules unaliasedModuleName varName
