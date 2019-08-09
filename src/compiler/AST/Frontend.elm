module AST.Frontend exposing
    ( Expr(..)
    , LocatedExpr
    , ProjectFields
    , lambda
    , transform
    , unwrap
    , var
    )

import AST.Common.Literal exposing (Literal)
import AST.Common.Located as Located exposing (Located)
import AST.Frontend.Unwrapped as Unwrapped
import Data.Binding as Binding exposing (Binding)
import Data.Module exposing (Modules)
import Data.ModuleName exposing (ModuleName)
import Data.VarName exposing (VarName)
import Transform


type alias ProjectFields =
    { modules : Modules LocatedExpr }


type alias LocatedExpr =
    Located Expr


type Expr
    = Literal Literal
    | Var { qualifier : Maybe ModuleName, name : VarName }
    | Argument VarName
    | Plus LocatedExpr LocatedExpr
    | Cons LocatedExpr LocatedExpr
    | ListConcat LocatedExpr LocatedExpr
    | Lambda { arguments : List VarName, body : LocatedExpr }
    | Call { fn : LocatedExpr, argument : LocatedExpr }
    | If { test : LocatedExpr, then_ : LocatedExpr, else_ : LocatedExpr }
    | Let { bindings : List (Binding LocatedExpr), body : LocatedExpr }
    | List (List LocatedExpr)
    | Unit
    | Tuple LocatedExpr LocatedExpr
    | Tuple3 LocatedExpr LocatedExpr LocatedExpr


var : Maybe ModuleName -> VarName -> Expr
var qualifier name =
    Var
        { qualifier = qualifier
        , name = name
        }


lambda : List VarName -> LocatedExpr -> Expr
lambda arguments body =
    Lambda
        { arguments = arguments
        , body = body
        }


{-| A helper for the Transform library.
-}
recurse : (Expr -> Expr) -> Expr -> Expr
recurse f expr =
    let
        f_ =
            Located.map f
    in
    case expr of
        Literal _ ->
            expr

        Var _ ->
            expr

        Argument _ ->
            expr

        Plus e1 e2 ->
            Plus
                (f_ e1)
                (f_ e2)

        Cons e1 e2 ->
            Cons
                (f_ e1)
                (f_ e2)

        ListConcat e1 e2 ->
            ListConcat (f_ e1) (f_ e2)

        Lambda ({ body } as lambda_) ->
            Lambda { lambda_ | body = f_ body }

        Call { fn, argument } ->
            Call
                { fn = f_ fn
                , argument = f_ argument
                }

        If { test, then_, else_ } ->
            If
                { test = f_ test
                , then_ = f_ then_
                , else_ = f_ else_
                }

        Let { bindings, body } ->
            Let
                { bindings = List.map (Binding.map f_) bindings
                , body = f_ body
                }

        List items ->
            List (List.map f_ items)

        Unit ->
            expr

        Tuple e1 e2 ->
            Tuple (f_ e1) (f_ e2)

        Tuple3 e1 e2 e3 ->
            Tuple3 (f_ e1) (f_ e2) (f_ e3)


transform : (Expr -> Expr) -> Expr -> Expr
transform pass expr =
    {- If we do more than one at the same time, we should use the Transform
       library a bit differently, see `Transform.orList`.
    -}
    Transform.transformAll
        recurse
        (Transform.toMaybe pass)
        expr


unwrap : LocatedExpr -> Unwrapped.Expr
unwrap expr =
    case Located.unwrap expr of
        Literal literal ->
            Unwrapped.Literal literal

        Var var_ ->
            Unwrapped.Var var_

        Argument name ->
            Unwrapped.Argument name

        Plus e1 e2 ->
            Unwrapped.Plus
                (unwrap e1)
                (unwrap e2)

        Cons e1 e2 ->
            Unwrapped.Cons
                (unwrap e1)
                (unwrap e2)

        ListConcat e1 e2 ->
            Unwrapped.ListConcat
                (unwrap e1)
                (unwrap e2)

        Lambda { arguments, body } ->
            Unwrapped.Lambda
                { arguments = arguments
                , body = unwrap body
                }

        Call { fn, argument } ->
            Unwrapped.Call
                { fn = unwrap fn
                , argument = unwrap argument
                }

        If { test, then_, else_ } ->
            Unwrapped.If
                { test = unwrap test
                , then_ = unwrap then_
                , else_ = unwrap else_
                }

        Let { bindings, body } ->
            Unwrapped.Let
                { bindings = List.map (Binding.map unwrap) bindings
                , body = unwrap body
                }

        List list ->
            Unwrapped.List
                (List.map unwrap list)

        Unit ->
            Unwrapped.Unit

        Tuple e1 e2 ->
            Unwrapped.Tuple
                (unwrap e1)
                (unwrap e2)

        Tuple3 e1 e2 e3 ->
            Unwrapped.Tuple3
                (unwrap e1)
                (unwrap e2)
                (unwrap e3)
