module Stage.Typecheck exposing (typecheck)

import Common exposing (Project)
import Error exposing (Error, TypeError(..))


typecheck : Project -> Result Error Project
typecheck sourceCode =
    Debug.todo "typecheck"
