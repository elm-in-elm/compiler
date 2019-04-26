module Stage.Optimize exposing (optimize)

import AST.Typed as Typed
import Common.Types exposing (Project)
import Error exposing (Error)


optimize : Project Typed.ProjectFields -> Result Error (Project Typed.ProjectFields)
optimize project =
    -- TODO do we need types when optimizing? If not we could swap the stages and make this a bit simpler
    -- TODO optimizations aren't MVP, do it later :)
    Ok project
