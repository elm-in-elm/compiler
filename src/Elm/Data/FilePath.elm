module Elm.Data.FilePath exposing
    ( FilePath
    , expectedFilePath
    )

import Elm.Data.ModuleName exposing (ModuleName)


type alias FilePath =
    String


{-| Expects the source directory filepaths to be normalized so that there's no `/` at the end.
-}
expectedFilePath : { sourceDirectory : FilePath, moduleName : ModuleName } -> FilePath
expectedFilePath { sourceDirectory, moduleName } =
    sourceDirectory
        ++ "/"
        ++ String.replace "." "/" moduleName
        ++ ".elm"
