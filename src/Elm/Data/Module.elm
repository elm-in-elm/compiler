module Elm.Data.Module exposing
    ( Module, ModuleType(..)
    , map
    , unalias, exposes, imports
    )

{-| Module information (corresponds to a single .elm file).
Name, imports, contents, etc.

@docs Module, ModuleType
@docs map
@docs unalias, exposes, imports

-}

import Dict exposing (Dict)
import Dict.Extra as Dict
import Elm.Data.Declaration as Declaration exposing (Declaration)
import Elm.Data.Exposing exposing (ExposedItem(..), Exposing(..))
import Elm.Data.FilePath exposing (FilePath)
import Elm.Data.Import exposing (Import)
import Elm.Data.ModuleName exposing (ModuleName)
import Elm.Data.VarName exposing (VarName)


{-| -}
type alias Module expr annotation =
    -- TODO comments? doc comments?
    { -- TODO somewhere check that dependencies' exposing lists contain only what's in that module's exposing list
      imports : Dict ModuleName Import
    , name : ModuleName
    , filePath : FilePath
    , declarations : Dict VarName (Declaration expr annotation)
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
imports : ModuleName -> Module expr annotation -> Bool
imports moduleName module_ =
    Dict.member moduleName module_.imports


{-| Does this module expose this variable name?
-}
exposes : VarName -> Module expr annotation -> Bool
exposes varName module_ =
    let
        isInDeclarations =
            Dict.member varName module_.declarations
    in
    case module_.exposing_ of
        ExposingAll ->
            isInDeclarations

        ExposingSome items ->
            List.any
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
unalias : Module expr annotation -> ModuleName -> Maybe ModuleName
unalias thisModule moduleName =
    thisModule.imports
        |> Dict.find (\_ dep -> dep.as_ == Just moduleName)
        |> Maybe.map (Tuple.second >> .moduleName)


{-| Apply a function to all the expressions inside the module.
-}
map : (exprA -> exprB) -> Module exprA annotation -> Module exprB annotation
map fn module_ =
    { imports = module_.imports
    , name = module_.name
    , filePath = module_.filePath
    , declarations = Dict.map (always <| Declaration.map fn) module_.declarations
    , type_ = module_.type_
    , exposing_ = module_.exposing_
    }
