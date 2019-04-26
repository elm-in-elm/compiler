module Stage.RemoveTypes exposing (removeTypes)

import AST.Canonical as Canonical
import AST.Typed as Typed
import Common.Types exposing (Project)
import Error exposing (Error)


removeTypes : Project Typed.ProjectFields -> Result Error (Project Canonical.ProjectFields)
removeTypes p =
    p.modules
        |> Debug.todo "removeTypes"
