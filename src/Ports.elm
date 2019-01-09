port module Ports exposing
    ( print
    , printStderr
    , println
    , printlnStderr
    , readFile
    , waitForReadFile
    , writeToFile
    )

import Common exposing (FileContents(..))


port stdout : String -> Cmd msg


port stderr : String -> Cmd msg


port readFile : String -> Cmd msg


port readSubscription : (( String, String ) -> msg) -> Sub msg


port write : { filename : String, contents : String } -> Cmd msg


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


waitForReadFile : (String -> FileContents -> msg) -> Sub msg
waitForReadFile toMsg =
    readSubscription
        (\( fileName, fileContents ) ->
            toMsg fileName (FileContents fileContents)
        )


writeToFile : String -> FileContents -> Cmd msg
writeToFile filename (FileContents contents) =
    write
        { filename = filename
        , contents = contents
        }
