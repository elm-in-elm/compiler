module AST.Frontend exposing
    ( Expr(..)
    , ProjectFields
    , transformOne
    )

import AST.Common exposing (Literal(..))
import Common.Types
    exposing
        ( ModuleName
        , Modules
        , VarName
        )
import Transform


type alias ProjectFields =
    { modules : Modules Expr }


type Expr
    = Literal Literal
    | Var ( Maybe ModuleName, VarName ) -- the ModuleName here is name of the alias
    | Argument VarName
    | Plus Expr Expr
    | Lambda
        -- TODO multi-arg lambda
        { argument : VarName
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


{-| A helper for the Transform library.
-}
recurse : (Expr -> Expr) -> Expr -> Expr
recurse fn expr =
    case expr of
        Literal (LInt _) ->
            expr

        Var _ ->
            expr

        Argument _ ->
            expr

        Plus e1 e2 ->
            Plus (fn e1) (fn e2)

        Lambda ({ body } as rec) ->
            Lambda { rec | body = fn body }


{-| TODO Maybe find a better name? The "one" means one transformation.

(If we do more than one at the same time, we should use the Transform library
a bit differently, see `Transform.orList`.)

-}
transformOne : (Expr -> Expr) -> Expr -> Expr
transformOne transform expr =
    Transform.transformAll
        recurse
        (Transform.toMaybe transform)
        expr
