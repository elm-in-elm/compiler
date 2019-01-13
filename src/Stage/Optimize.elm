module Stage.Optimize exposing (optimize)

import AST.Canonical as Canonical
import Common exposing (Project)
import Error exposing (Error, OptimizeError(..))


optimize : Project -> Result Error Project
optimize project =
    Debug.todo "optimize"
