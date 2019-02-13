module Stage.Typecheck exposing (typecheck)

import AST.Canonical as Canonical
import Common.Types exposing (Project)
import Error exposing (Error)


typecheck : Project Canonical.Expr -> Result Error (Project Canonical.Expr)
typecheck project =
    -- TODO do later. not a MVP
    Ok project
