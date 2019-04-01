module AST.Canonical exposing
    ( Expr(..)
    , ProjectFields
    )

import AST.Common exposing (Literal)
import Common.Types
    exposing
        ( ModuleName
        , Modules
        , VarName
        )


type alias ProjectFields =
    { modules : Modules Expr }


{-| Differs from Frontend.Expr by:

  - having fully qualified variables
  - having only single argument lambdas

TODO records are probably better for communicating the meaning of args.

-}
type Expr
    = Literal Literal
    | Var ModuleName VarName
    | Argument VarName
    | Plus Expr Expr
    | Lambda VarName Expr
