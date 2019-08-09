module Data.FilePath exposing
    ( FilePath
    , expected
    , fromString
    , toString
    )

import Data.ModuleName as ModuleName exposing (ModuleName)


type FilePath
    = FilePath String


{-| TODO do we want some sort of validation here? eg. trailing slashes etc.
This currently happens elsewhere
-}
fromString : String -> FilePath
fromString string =
    FilePath string


toString : FilePath -> String
toString (FilePath filePath) =
    filePath


{-| Expects the source directory filepaths to be normalized so that there's no `/` at the end.
-}
expected : FilePath -> ModuleName -> FilePath
expected (FilePath sourceDirectory) moduleName =
    FilePath (sourceDirectory ++ "/" ++ String.replace "." "/" (ModuleName.toString moduleName) ++ ".elm")
