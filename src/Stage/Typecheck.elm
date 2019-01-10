module Stage.Typecheck exposing (typecheck)

import Common exposing (AST, Dict_, Module, ModuleName)
import Error exposing (Error, TypeError(..))


typecheck : Dict_ ModuleName Module -> Result Error (Dict_ ModuleName Module)
typecheck sourceCode =
    Debug.todo "typecheck"
