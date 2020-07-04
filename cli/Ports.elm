port module Ports exposing
    ( print
    , printStderr
    , println
    , printlnStderr
    , readFile
    , setExitCode
    , waitForReadFile
    , writeToFile
    )

import Elm.Data.FileContents exposing (FileContents)
import Elm.Data.FilePath exposing (FilePath)
import Elm.Data.ModuleName exposing (ModuleName)


port stdout : String -> Cmd msg


port stderr : String -> Cmd msg


port read :
    { moduleName : ModuleName
    , filePath : FilePath
    }
    -> Cmd msg


port readSubscription :
    ({ filePath : FilePath
     , fileContents : FileContents
     }
     -> msg
    )
    -> Sub msg


port readErrorSubscription :
    ({ moduleName : ModuleName
     , filePath : FilePath
     , errorCode : String
     }
     -> msg
    )
    -> Sub msg


port writeToFile :
    { filePath : FilePath
    , fileContents : FileContents
    }
    -> Cmd msg


port setExitCode : Int -> Cmd msg


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


readFile :
    { moduleName : ModuleName
    , filePath : FilePath
    }
    -> Cmd msg
readFile r =
    read r


waitForReadFile :
    ({ moduleName : ModuleName
     , filePath : FilePath
     , errorCode : String
     }
     -> msg
    )
    ->
        ({ filePath : FilePath
         , fileContents : FileContents
         }
         -> msg
        )
    -> Sub msg
waitForReadFile toErrorMsg toMsg =
    Sub.batch
        [ readSubscription toMsg
        , readErrorSubscription toErrorMsg
        ]
