module AST.Frontend exposing
    ( Expr(..)
    , ProjectFields
    , lambda
    , transform
    , var
    )

import AST.Common.Literal exposing (Literal)
import Common
import Common.Types
    exposing
        ( Binding
        , ModuleName
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
    | If { test : Expr, then_ : Expr, else_ : Expr }
    | Let { bindings : List (Binding Expr), body : Expr }
    | List (List Expr)
    | Unit
    | Tuple Expr Expr
    | Tuple3 Expr Expr Expr


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



{- Let's not get ahead of ourselves

   | Let
       { varName : VarName
       , varBody : Expr
       , body : Expr
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
        Literal _ ->
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

        If { test, then_, else_ } ->
            If
                { test = f test
                , then_ = f then_
                , else_ = f else_
                }

        Let { bindings, body } ->
            Let
                { bindings = List.map (Common.mapBinding f) bindings
                , body = f body
                }

        List items ->
            List (List.map f items)

        Unit ->
            expr
        
        Tuple e1 e2 ->
            Tuple (f e1) (f e2)

        Tuple3 e1 e2 e3 ->
            Tuple3 (f e1) (f e2) (f e3)


transform : (Expr -> Expr) -> Expr -> Expr
transform pass expr =
    {- If we do more than one at the same time, we should use the Transform
       library a bit differently, see `Transform.orList`.
    -}
    Transform.transformAll
        recurse
        (Transform.toMaybe pass)
        expr
