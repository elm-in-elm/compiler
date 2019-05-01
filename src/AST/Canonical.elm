module AST.Canonical exposing
    ( Expr(..)
    , ProjectFields
    , lambda
    , var
    )

import AST.Common.Literal exposing (Literal)
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

-}
type Expr
    = Literal Literal
    | Var { qualifier : ModuleName, name : VarName }
    | Argument VarName
    | Plus Expr Expr
    | Lambda { argument : VarName, body : Expr }
    | Call { fn : Expr, argument : Expr }
    | If { test : Expr, then_ : Expr, else_ : Expr }


var : ModuleName -> VarName -> Expr
var qualifier name =
    Var
        { qualifier = qualifier
        , name = name
        }


lambda : VarName -> Expr -> Expr
lambda argument body =
    Lambda
        { argument = argument
        , body = body
        }
