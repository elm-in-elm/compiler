module AST.Frontend exposing (Expr(..), Literal(..))

import AST.Common exposing (VarName(..))


type Expr
    = Literal Literal


type Literal
    = LInt Int



{- Let's not get ahead of ourselves

   | Var VarName
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
-}
