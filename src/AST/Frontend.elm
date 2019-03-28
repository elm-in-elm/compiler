module AST.Frontend exposing
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


type Expr
    = Literal Literal
    | Var ( Maybe ModuleName, VarName ) -- the ModuleName here is name of the alias
    | Plus Expr Expr
    | Lambda
        -- TODO multi-arg lambda
        { argName : VarName
        , body : Expr
        }



{- Let's not get ahead of ourselves

   | Application
       { fn : Expr
       , arg : Expr
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
