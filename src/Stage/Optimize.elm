module Stage.Optimize exposing (optimize)

import AST.Canonical as Canonical
import Common.Types exposing (Project)
import Error exposing (Error)


optimize : Project Canonical.ProjectFields -> Result Error (Project Canonical.ProjectFields)
optimize project =
    -- TODO optimizations aren't MVP, do it later :)
    Ok project
