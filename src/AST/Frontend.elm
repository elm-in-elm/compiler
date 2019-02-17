module AST.Frontend exposing
    ( Expr(..)
    , ProjectFields
    )

import AST.Common exposing (Literal)
import Common.Types
    exposing
        ( Modules
        , VarName(..)
        )


type alias ProjectFields =
    { modules : Modules Expr }


type Expr
    = Literal Literal
    | Var VarName
    | Plus Expr Expr



{- Let's not get ahead of ourselves

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
