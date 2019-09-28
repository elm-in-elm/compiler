port module Ports exposing
    ( print
    , printStderr
    , println
    , printlnStderr
    , readFile
    , waitForReadFile
    , writeToFile
    )

import Elm.Compiler.Error as Error exposing (ErrorCode)
import Elm.Data.FileContents exposing (FileContents)
import Elm.Data.FilePath exposing (FilePath)


port stdout : String -> Cmd msg


port stderr : String -> Cmd msg


port read : String -> Cmd msg


port readSubscription : ({ filePath : FilePath, fileContents : FileContents } -> msg) -> Sub msg


port readErrorSubscription : ({ filePath : FilePath, errorCode : String } -> msg) -> Sub msg


port writeToFile : { filePath : String, fileContents : String } -> Cmd msg


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
    read filePath


waitForReadFile : (ErrorCode -> msg) -> ({ filePath : FilePath, fileContents : FileContents } -> msg) -> Sub msg
waitForReadFile toErrorMsg toMsg =
    Sub.batch
        [ readSubscription toMsg
        , readErrorSubscription (toErrorMsg << Error.parseErrorCode)
        ]
