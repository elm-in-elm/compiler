module Data.ModuleName exposing
    ( ModuleName
    , expected
    , fromString
    , toString
    )


type ModuleName
    = ModuleName String


{-| Nothing if the source directory doesn't agree with the file path.

    expectedModuleName "src" "src/Foo.elm"
    --> Just "Foo"

    expectedModuleName "src" "lib/Foo.elm"
    --> Nothing

Doesn't use the FilePath type to avoid cycles. TODO maybe we can prevent it with a different module structure?

TODO doesn't work with multiple source directories!

-}
expected : { sourceDirectory : String, filePath : String } -> Maybe ModuleName
expected { sourceDirectory, filePath } =
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
            |> ModuleName
            |> Just

    else
        Nothing


toString : ModuleName -> String
toString (ModuleName moduleName) =
    moduleName


fromString : String -> ModuleName
fromString string =
    ModuleName string
