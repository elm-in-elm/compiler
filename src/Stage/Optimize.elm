module Stage.Optimize exposing (optimize)

import Common.Types exposing (Project)
import Error exposing (Error)


optimize : Project -> Result Error Project
optimize project =
    -- TODO optimizations aren't MVP, do it later :)
    Ok project
