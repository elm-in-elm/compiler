module Stage.Optimize exposing (optimize)

import Common exposing (Project)
import Error exposing (Error, OptimizeError(..))


optimize : Project -> Result Error Project
optimize project =
    Debug.todo "optimize"
