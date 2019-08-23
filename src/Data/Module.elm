module Data.Module exposing
    ( EffectMetadata
    , Module
    , ModuleType(..)
    , Modules
    , exposes
    , unalias
    )

import AssocList as Dict exposing (Dict)
import AssocList.Extra as Dict
import Data.Declaration exposing (Declaration)
import Data.Exposing exposing (ExposedItem(..), Exposing(..))
import Data.FilePath exposing (FilePath)
import Data.Import exposing (Import)
import Data.ModuleName exposing (ModuleName)
import Data.VarName exposing (VarName)


type alias Module expr =
    -- TODO comments? doc comments?
    { -- TODO somewhere check that dependencies' exposing lists contain only what's in that module's exposing list
      imports : Dict ModuleName Import
    , name : ModuleName
    , filePath : FilePath
    , declarations : Dict VarName (Declaration expr)
    , type_ : ModuleType
    , exposing_ : Exposing
    }


type ModuleType
    = PlainModule
    | PortModule
    | EffectModule EffectMetadata


type alias EffectMetadata =
    {}


type alias Modules expr =
    Dict ModuleName (Module expr)


{-| Is this variable name exposed in this module?
-}
exposes : VarName -> Module a -> Bool
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


{-| Given `import Foo as F`, `unalias ... (ModuleName "F")` => `Just (ModuleName "Foo")`
-}
unalias : Module a -> ModuleName -> Maybe ModuleName
unalias thisModule moduleName =
    thisModule.imports
        |> Dict.find (\_ dep -> dep.as_ == Just moduleName)
        |> Maybe.map (Tuple.second >> .moduleName)
