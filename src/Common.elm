module Common exposing
    ( expectedFilePath
    , expectedModuleName
    , filePathToString
    , moduleNameToString
    , moduleNames
    , topLevelDeclarationToString
    , varNameToString
    )

import Common.Types
    exposing
        ( Dict_
        , FilePath(..)
        , ModuleName(..)
        , Modules
        , Set_
        , TopLevelDeclaration
        , VarName(..)
        )
import Dict.Any as AnyDict
import Error exposing (Error(..), GeneralError(..))
import Set.Any as AnySet


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
                |> AnyDict.keys
                |> AnySet.fromList moduleNameToString
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
        FileNotInSourceDirectories (FilePath filePath)
            |> GeneralError
            |> Err


varNameToString : VarName -> String
varNameToString (VarName varName) =
    varName


topLevelDeclarationToString : TopLevelDeclaration a -> String
topLevelDeclarationToString { name, module_ } =
    moduleNameToString module_ ++ "." ++ varNameToString name
