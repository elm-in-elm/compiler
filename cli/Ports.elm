port module Ports exposing
    ( print
    , printStderr
    , println
    , printlnStderr
    , readFile
    , waitForReadFile
    , writeToFile
    )

import Data.FileContents as FileContents exposing (FileContents)
import Data.FilePath as FilePath exposing (FilePath)
import Error exposing (ErrorCode)


port stdout : String -> Cmd msg


port stderr : String -> Cmd msg


port read : String -> Cmd msg


port readSubscription : (( String, String ) -> msg) -> Sub msg


port readErrorSubscription : (( String, String ) -> msg) -> Sub msg


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
readFile filePath =
    read <| FilePath.toString filePath


waitForReadFile : (ErrorCode -> msg) -> (FilePath -> FileContents -> msg) -> Sub msg
waitForReadFile toErrorMsg toMsg =
    Sub.batch
        [ readSubscription
            (\( filePath, fileContents ) ->
                toMsg (FilePath.fromString filePath) (FileContents.fromString fileContents)
            )
        , readErrorSubscription
            (\( filePath, errorCode ) ->
                let
                    filePath_ =
                        FilePath.fromString filePath
                in
                toErrorMsg (Error.parseErrorCode errorCode filePath_)
            )
        ]


writeToFile : FilePath -> FileContents -> Cmd msg
writeToFile filePath contents =
    write
        { filePath = FilePath.toString filePath
        , contents = FileContents.toString contents
        }
