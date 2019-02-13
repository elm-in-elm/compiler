module Stage.PrepareForBackend exposing (prepareForBackend)

import AST.Backend as Backend
import AST.Canonical as Canonical
import Common.Types exposing (Project)
import Error exposing (Error)


prepareForBackend : Project Canonical.Expr -> Result Error (Project Backend.Expr)
prepareForBackend project =
    -- TODO the types are the same right now
    Ok project
