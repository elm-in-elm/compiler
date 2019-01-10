port module Ports exposing
    ( print
    , printStderr
    , println
    , printlnStderr
    , readFile
    , waitForReadFile
    , writeToFile
    )

import Common exposing (FileContents(..), FilePath(..))


port stdout : String -> Cmd msg


port stderr : String -> Cmd msg


port read : String -> Cmd msg


port readSubscription : (( String, String ) -> msg) -> Sub msg


port write : { filePath : String, contents : String } -> Cmd msg


print : String -> Cmd msg
print string =
    stdout string


println : String -> Cmd msg
println string =
    stdout (string ++ "\n")


printStderr : String -> Cmd msg
printStderr string =
    stderr string


printlnStderr : String -> Cmd msg
printlnStderr string =
    stderr (string ++ "\n")


readFile : FilePath -> Cmd msg
readFile (FilePath filePath) =
    read filePath


waitForReadFile : (FilePath -> FileContents -> msg) -> Sub msg
waitForReadFile toMsg =
    readSubscription
        (\( filePath, fileContents ) ->
            toMsg (FilePath filePath) (FileContents fileContents)
        )


writeToFile : FilePath -> FileContents -> Cmd msg
writeToFile (FilePath filePath) (FileContents contents) =
    write
        { filePath = filePath
        , contents = contents
        }
