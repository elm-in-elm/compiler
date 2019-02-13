module Stage.Emit exposing (emit)

import AST.Backend as Backend
import Common.Types
    exposing
        ( FileContents(..)
        , Project
        , ProjectToEmit
        )
import Error exposing (Error)


emit : Project Backend.Expr -> Result Error ProjectToEmit
emit project =
    -- TODO do it later
    Ok { output = FileContents "// TODO" }
