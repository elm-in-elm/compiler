module Elm.Data.ModuleName exposing (ModuleName, expectedModuleName)

{-| Module name, eg. the `Foo.Bar` in

    something =
        Foo.Bar.x + 1

@docs ModuleName, expectedModuleName

-}

-- TODO maybe hold it as a list for as long as possible?


{-| Just a `String` alias, instead of a `type` wrapper. We generally use records
with explanatory field names where two Strings would be next to each other,
to protect against swapping them accidentally.
-}
type alias ModuleName =
    String


{-| Converts from file path to its expected module name.
Additionally needs the source directory from `elm.json`.

Returns `Nothing` if the source directory doesn't agree with the file path.

    expectedModuleName { sourceDirectory = "src", filePath = "src/Foo.elm" }
    --> Just "Foo"

    expectedModuleName { sourceDirectory = "src", filePath = "lib/Foo.elm" }
    --> Nothing

-}
expectedModuleName : { sourceDirectory : String, filePath : String } -> Maybe ModuleName
expectedModuleName { sourceDirectory, filePath } =
    if String.startsWith sourceDirectory filePath then
        let
            lengthToDrop : Int
            lengthToDrop =
                {- sourceDirectory has been normalized to not end with `/`.
                   filePath ends with a file, so there definitely is a `/`.

                   We need to drop everything before the filename, so that's
                   length of the source directory + 1 for the extra slash.

                      "src/Foo.elm"
                       ^^^^

                -}
                String.length sourceDirectory + 1
        in
        filePath
            |> String.dropLeft lengthToDrop
            |> String.replace "/" "."
            |> String.dropRight {- .elm -} 4
            |> Just

    else
        Nothing
