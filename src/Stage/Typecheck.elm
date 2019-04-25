module Stage.Typecheck exposing (typecheck)

import AST.Canonical as Canonical
import AST.Typechecked as Typechecked
import Common.Types exposing (Project)
import Error exposing (Error)


typecheck : Project Canonical.ProjectFields -> Result Error (Project Typechecked.ProjectFields)
typecheck project =
    Ok project
