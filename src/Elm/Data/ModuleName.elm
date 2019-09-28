module Elm.Data.ModuleName exposing
    ( ModuleName
    , expectedModuleName
    )


type alias ModuleName =
    String


{-| Nothing if the source directory doesn't agree with the file path.

    expectedModuleName "src" "src/Foo.elm"
    --> Just "Foo"

    expectedModuleName "src" "lib/Foo.elm"
    --> Nothing

Doesn't use the FilePath type to avoid cycles. TODO maybe we can prevent it with a different module structure?

TODO doesn't work with multiple source directories!

-}
expectedModuleName : { sourceDirectory : String, filePath : String } -> Maybe ModuleName
expectedModuleName { sourceDirectory, filePath } =
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
