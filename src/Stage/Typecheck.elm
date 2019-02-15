module Stage.Typecheck exposing (typecheck)

import AST.Canonical as Canonical
import Common.Types exposing (Project)
import Error exposing (Error)


typecheck : Project Canonical.ProjectFields -> Result Error (Project Canonical.ProjectFields)
typecheck project =
    -- TODO do later. not a MVP
    Ok project
