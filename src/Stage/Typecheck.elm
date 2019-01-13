module Stage.Typecheck exposing (typecheck)

import Common.Types exposing (Project)
import Error exposing (Error)


typecheck : Project -> Result Error Project
typecheck project =
    -- TODO do later. not a MVP
    Ok project
