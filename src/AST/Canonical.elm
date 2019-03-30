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
  - (after fixing TODO about multi-arg lambda in Frontend.Expr) having only single-arg lambdas

-}
type Expr
    = Literal Literal
    | Var ( ModuleName, VarName )
    | Argument VarName
    | Plus Expr Expr
    | Lambda
        { argument : VarName
        , body : Expr
        }
