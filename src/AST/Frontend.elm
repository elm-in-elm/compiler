module AST.Frontend exposing (Expr(..))

import AST.Common exposing (Literal(..), VarName(..))


type Expr
    = Var VarName
    | Application
        { fn : Expr
        , arg : Expr
        }
    | Lambda
        { argName : VarName
        , body : Expr
        }
    | Let
        { varName : VarName
        , varBody : Expr
        , body : Expr
        }
    | Literal Literal
    | If
        { test : Expr
        , then_ : Expr
        , else_ : Expr
        }
    | Fixpoint Expr
    | Operator
        { opName : VarName
        , left : Expr
        , right : Expr
        }
