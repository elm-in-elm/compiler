module Stage.Emit exposing (emit)

import Common exposing (FileContents, Project)
import Error exposing (EmitError(..), Error)


emit : Project -> Result Error FileContents
emit project =
    -- TODO Bytes instead of String?
    Debug.todo "emit"
