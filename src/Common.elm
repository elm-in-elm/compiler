module Common exposing
    ( expectedFilePath
    , expectedModuleName
    , filePathToString
    , moduleNameToString
    , moduleNames
    )

import Common.Types
    exposing
        ( Dict_
        , ElmProgram(..)
        , FilePath(..)
        , ModuleName(..)
        , Set_
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


moduleNames : ElmProgram -> Set_ ModuleName
moduleNames program =
    let
        toSet : Dict_ ModuleName a -> Set_ ModuleName
        toSet dict =
            dict
                |> AnyDict.keys
                |> AnySet.fromList moduleNameToString
    in
    case program of
        Frontend { modules } ->
            toSet modules

        Canonical { modules } ->
            toSet modules

        Backend { modules } ->
            toSet modules


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
