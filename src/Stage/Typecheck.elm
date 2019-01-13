module Stage.Typecheck exposing (typecheck)

import AST.Canonical as Canonical
import Common exposing (Project)
import Error exposing (Error, TypeError(..))


typecheck : Project -> Result Error Project
typecheck sourceCode =
    Debug.todo "typecheck"
