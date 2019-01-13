module Stage.Emit exposing (emit)

import AST.Backend as Backend
import Common
    exposing
        ( FileContents
        , Project
        , ProjectToEmit
        )
import Error exposing (EmitError(..), Error)


emit : Project -> Result Error ProjectToEmit
emit project =
    Debug.todo "emit"
