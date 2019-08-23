module Data.FileContents exposing
    ( FileContents
    , fromString
    , toString
    )


type FileContents
    = FileContents String


fromString : String -> FileContents
fromString string =
    FileContents string


toString : FileContents -> String
toString (FileContents fileContents) =
    fileContents
