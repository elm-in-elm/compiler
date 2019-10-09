module Elm.Data.ModuleName exposing (ModuleName, expectedModuleName)

{-| Module name, eg. the `Foo.Bar` in

    something =
        Foo.Bar.x + 1

@docs ModuleName, expectedModuleName

-}


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
    {- TODO Doesn't use the FilePath type to avoid cycles.
       Maybe we can prevent it with a different module structure?
    -}
    -- TODO Doesn't work with multiple source directories!
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
            |> String.dropRight {- .elm -} 4
            |> Just

    else
        Nothing
