module Stage.Emit exposing (emit)

import Common.Types
    exposing
        ( FileContents(..)
        , Project
        , ProjectToEmit
        )
import Error exposing (Error)


emit : Project -> Result Error ProjectToEmit
emit project =
    -- TODO do it later
    Ok { output = FileContents "// TODO" }
