module Stage.Desugar exposing (desugar)

import Common.Types exposing (Project)
import Error exposing (Error)


desugar : Project -> Result Error Project
desugar project =
    -- TODO for now, canonical AST is the same as frontend AST, so do nothing
    Ok project
