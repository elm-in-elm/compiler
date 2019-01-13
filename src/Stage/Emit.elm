module Stage.Emit exposing (emit)

import Common exposing (Project, ProjectToEmit)
import Error exposing (Error)


emit : Project -> Result Error ProjectToEmit
emit project =
    Debug.todo "emit"
