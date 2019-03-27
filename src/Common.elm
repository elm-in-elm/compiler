module Common exposing
    ( expectedFilePath
    , expectedModuleName
    , exposes
    , filePathToString
    , moduleNameToString
    , moduleNames
    , topLevelDeclarationToString
    , unalias
    , varNameToString
    )

import Common.Types
    exposing
        ( Dict_
        , ExposedItem(..)
        , Exposing(..)
        , FilePath(..)
        , Module
        , ModuleName(..)
        , Modules
        , Set_
        , TopLevelDeclaration
        , VarName(..)
        )
import Dict.Any
import Error exposing (Error(..), GeneralError(..))
import Extra.Dict.Any
import Set.Any


filePathToString : FilePath -> String
filePathToString (FilePath filePath) =
    filePath


moduleNameToString : ModuleName -> String
moduleNameToString (ModuleName moduleName) =
    moduleName


moduleNames : Modules expr -> Set_ ModuleName
moduleNames program =
    let
        toSet : Modules expr -> Set_ ModuleName
        toSet dict =
            dict
                |> Dict.Any.keys
                |> Set.Any.fromList moduleNameToString
    in
    toSet program


{-| Expects the source directory filepaths to be normalized so that there's no `/` at the end.
-}
expectedFilePath : FilePath -> ModuleName -> FilePath
expectedFilePath (FilePath sourceDirectory) (ModuleName moduleName) =
    FilePath (sourceDirectory ++ "/" ++ String.replace "." "/" moduleName ++ ".elm")


expectedModuleName : FilePath -> FilePath -> Result Error ModuleName
expectedModuleName (FilePath sourceDirectory) (FilePath filePath) =
    if String.startsWith sourceDirectory filePath then
        let
            lengthToDrop : Int
            lengthToDrop =
                -- don't forget the `/` which isn't part of the sourceDirectory but is in the filePath
                String.length sourceDirectory + 1
        in
        filePath
            |> String.dropLeft lengthToDrop
            |> String.replace "/" "."
            |> String.dropRight 4
            |> ModuleName
            |> Ok

    else
        Err (GeneralError (FileNotInSourceDirectories (FilePath filePath)))


varNameToString : VarName -> String
varNameToString (VarName varName) =
    varName


topLevelDeclarationToString : TopLevelDeclaration a -> String
topLevelDeclarationToString { name, module_ } =
    moduleNameToString module_ ++ "." ++ varNameToString name


{-| Is this variable name exposed in this module?
-}
exposes : VarName -> Modules a -> Module a -> Bool
exposes ((VarName var) as varName) modules module_ =
    let
        isInTopLevelDeclarations =
            Dict.Any.member varName module_.topLevelDeclarations
    in
    case module_.exposing_ of
        ExposingAll ->
            isInTopLevelDeclarations

        ExposingSome items ->
            List.any
                (\exposedItem ->
                    case exposedItem of
                        ExposedValue value ->
                            value == var

                        ExposedType type_ ->
                            -- TODO check this code after we have custom types in Frontend.Expr.
                            type_ == var

                        ExposedTypeAndAllConstructors type_ ->
                            {- TODO when we have custom types in Frontend.Expr,
                               return if the varName is in the type constructors.
                            -}
                            False
                )
                items
                && isInTopLevelDeclarations


{-| Given `import Foo as F`, `unalias ... (ModuleName "F")` => `Just (ModuleName "Foo")`
-}
unalias : Module a -> ModuleName -> Maybe ModuleName
unalias thisModule moduleName =
    thisModule.dependencies
        |> Extra.Dict.Any.find (\_ dep -> dep.as_ == Just moduleName)
        |> Maybe.map (Tuple.second >> .moduleName)
