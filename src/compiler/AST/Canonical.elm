module AST.Canonical exposing
    ( Expr(..)
    , LocatedExpr
    , ProjectFields
    , lambda
    , unwrap
    , var
    )

import AST.Canonical.Unwrapped as Unwrapped
import AST.Common.Literal exposing (Literal)
import AST.Common.Located as Located exposing (Located)
import Common
import Common.Types
    exposing
        ( Binding
        , ModuleName
        , Modules
        , VarName
        )
import Dict.Any exposing (AnyDict)


type alias ProjectFields =
    { modules : Modules LocatedExpr }


type alias LocatedExpr =
    Located Expr


{-| Differs from Frontend.Expr by:

  - having fully qualified variables
  - having only single argument lambdas

-}
type Expr
    = Literal Literal
    | Var { qualifier : ModuleName, name : VarName }
    | Argument VarName
    | Plus LocatedExpr LocatedExpr
    | Lambda { argument : VarName, body : LocatedExpr }
    | Call { fn : LocatedExpr, argument : LocatedExpr }
    | If { test : LocatedExpr, then_ : LocatedExpr, else_ : LocatedExpr }
    | Let { bindings : AnyDict String VarName (Binding LocatedExpr), body : LocatedExpr }
    | List (List LocatedExpr)
    | Unit
    | Tuple LocatedExpr LocatedExpr
    | Tuple3 LocatedExpr LocatedExpr LocatedExpr


var : ModuleName -> VarName -> Expr
var qualifier name =
    Var
        { qualifier = qualifier
        , name = name
        }


lambda : VarName -> LocatedExpr -> Expr
lambda argument body =
    Lambda
        { argument = argument
        , body = body
        }


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

        Lambda { argument, body } ->
            Unwrapped.Lambda
                { argument = argument
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
                { bindings =
                    Dict.Any.map
                        (always (Common.mapBinding unwrap))
                        bindings
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
