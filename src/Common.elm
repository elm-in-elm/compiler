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


expectedFilePath : FilePath -> ModuleName -> FilePath
expectedFilePath (FilePath sourceDirectory) (ModuleName moduleName) =
    {- TODO somewhere normalize the / out of the source directories
       so that it's not there twice.

       Eg. we wouldn't want

           sourceDirectories = ["src/"]
           --> expectedFilePaths ... == "src//Foo.elm"
    -}
    FilePath (sourceDirectory ++ "/" ++ String.replace "." "/" moduleName ++ ".elm")


expectedModuleName : FilePath -> FilePath -> Result Error ModuleName
expectedModuleName (FilePath sourceDirectory) (FilePath filePath) =
    if String.startsWith sourceDirectory filePath then
        let
            lengthToDrop : Int
            lengthToDrop =
                String.length sourceDirectory
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
