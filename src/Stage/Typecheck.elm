module Stage.Typecheck exposing (typecheck)

import AST.Canonical as Canonical
import AST.Typechecked as Typechecked
import Common.Types exposing (Project)
import Error exposing (Error)


{-|

    -- TODO TODO TODO TODO TODO --
    ------------------------------
    -------- START HERE ----------
    ------------------------------
    -- TODO TODO TODO TODO TODO --



-}
typecheck : Project Canonical.ProjectFields -> Result Error (Project Typechecked.ProjectFields)
typecheck project =
    Ok project
