module AST.Frontend exposing
    ( Expr(..)
    , ProjectFields
    , call
    , lambda
    , transform
    , var
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
    | Var { qualifier : Maybe ModuleName, name : VarName }
    | Argument VarName
    | Plus Expr Expr
    | Lambda { arguments : List VarName, body : Expr }
    | Call { fn : Expr, argument : Expr }


var : Maybe ModuleName -> VarName -> Expr
var qualifier name =
    Var
        { qualifier = qualifier
        , name = name
        }


lambda : List VarName -> Expr -> Expr
lambda arguments body =
    Lambda
        { arguments = arguments
        , body = body
        }


call : Expr -> Expr -> Expr
call fn argument =
    Call
        { fn = fn
        , argument = argument
        }



{- Let's not get ahead of ourselves

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
recurse f expr =
    case expr of
        Literal (Int _) ->
            expr

        Literal (Char _) ->
            expr

        Literal (String _) ->
            expr

        Var _ ->
            expr

        Argument _ ->
            expr

        Plus e1 e2 ->
            Plus (f e1) (f e2)

        Lambda ({ body } as lambda_) ->
            Lambda { lambda_ | body = f body }

        Call { fn, argument } ->
            Call
                { fn = f fn
                , argument = f argument
                }


transform : (Expr -> Expr) -> Expr -> Expr
transform pass expr =
    {- If we do more than one at the same time, we should use the Transform
       library a bit differently, see `Transform.orList`.
    -}
    Transform.transformAll
        recurse
        (Transform.toMaybe pass)
        expr
