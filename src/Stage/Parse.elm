module Stage.Parse exposing (parse)

import Common exposing (FileContents(..), FilePath(..), Module)
import Error exposing (Error, ParseError(..))



-- TODO check filename too


{-| I suspect in the future we'll have to add an argument of previously parsed
modules.
-}
parse : FilePath -> FileContents -> Result Error Module
parse filePath fileContents =
    Debug.todo "parse"
