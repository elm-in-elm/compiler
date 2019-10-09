module Elm.Data.FilePath exposing (FilePath, expectedFilePath, removeTrailingSlash)

{-| File path of an Elm (or other) file.

@docs FilePath, expectedFilePath, removeTrailingSlash

-}

import Elm.Data.ModuleName exposing (ModuleName)


{-| Just a `String` alias, instead of a `type` wrapper. We generally use records
with explanatory field names where two Strings would be next to each other,
to protect against swapping them accidentally.
-}
type alias FilePath =
    String


{-| Converts from module name to its expected file path.
Additionally needs the source directory from `elm.json`.

The source directory can be passed both with and without the trailing slash.

    expectedFilePath { sourceDirectory = "src", moduleName = "Foo" }
    --> "src/Foo.elm"

    expectedFilePath { sourceDirectory = "src/", filePath = "Foo" }
    --> "src/Foo.elm"

-}
expectedFilePath : { sourceDirectory : FilePath, moduleName : ModuleName } -> FilePath
expectedFilePath { sourceDirectory, moduleName } =
    removeTrailingSlash sourceDirectory
        ++ "/"
        ++ String.replace "." "/" moduleName
        ++ ".elm"


{-| Normalize the file path.

    removeTrailingSlash "src/foo/"
    --> "src/foo"

    removeTrailingSlash "src/foo"
    --> "src/foo"

    removeTrailingSlash "src/foo/Main.elm"
    --> "src/foo/Main.elm"

-}
removeTrailingSlash : FilePath -> FilePath
removeTrailingSlash filePath =
    if String.endsWith "/" filePath then
        String.dropRight 1 filePath

    else
        filePath
