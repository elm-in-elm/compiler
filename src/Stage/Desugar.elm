module Stage.Desugar exposing (desugar)

import AST.Canonical as Canonical
import AST.Frontend as Frontend
import Common exposing (Project)
import Error exposing (DesugarError(..), Error)


desugar : Project -> Result Error Project
desugar project =
    Debug.todo "desugar"
