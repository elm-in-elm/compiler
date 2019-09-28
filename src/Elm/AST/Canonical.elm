module Elm.AST.Canonical exposing
    ( Expr(..)
    , LocatedExpr
    , ProjectFields
    , fromUnwrapped
    , lambda
    , unwrap
    )

import Dict exposing (Dict)
import Elm.AST.Canonical.Unwrapped as Unwrapped
import Elm.AST.Common.Located as Located exposing (Located)
import Elm.Data.Binding as Binding exposing (Binding)
import Elm.Data.Module exposing (Module)
import Elm.Data.ModuleName exposing (ModuleName)
import Elm.Data.VarName exposing (VarName)


type alias ProjectFields =
    { modules : Dict ModuleName (Module LocatedExpr) }


type alias LocatedExpr =
    Located Expr


{-| Differs from Frontend.Expr by:

  - having fully qualified variables
  - having only single argument lambdas

-}
type Expr
    = Int Int
    | Float Float
    | Char Char
    | String String
    | Bool Bool
    | Var { module_ : ModuleName, name : VarName }
    | Argument VarName
    | Plus LocatedExpr LocatedExpr
    | Cons LocatedExpr LocatedExpr
    | Lambda { argument : VarName, body : LocatedExpr }
    | Call { fn : LocatedExpr, argument : LocatedExpr }
    | If { test : LocatedExpr, then_ : LocatedExpr, else_ : LocatedExpr }
    | Let { bindings : Dict VarName (Binding LocatedExpr), body : LocatedExpr }
    | List (List LocatedExpr)
    | Unit
    | Tuple LocatedExpr LocatedExpr
    | Tuple3 LocatedExpr LocatedExpr LocatedExpr


lambda : VarName -> LocatedExpr -> Expr
lambda argument body =
    Lambda
        { argument = argument
        , body = body
        }


unwrap : LocatedExpr -> Unwrapped.Expr
unwrap expr =
    case Located.unwrap expr of
        Int int ->
            Unwrapped.Int int

        Float float ->
            Unwrapped.Float float

        Char char ->
            Unwrapped.Char char

        String string ->
            Unwrapped.String string

        Bool bool ->
            Unwrapped.Bool bool

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
                    Dict.map
                        (always (Binding.map unwrap))
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


{-| Adds **dummy** locations to the Unwrapped.Expr.
-}
fromUnwrapped : Unwrapped.Expr -> LocatedExpr
fromUnwrapped expr =
    Located.located Located.dummyRegion <|
        case expr of
            Unwrapped.Int int ->
                Int int

            Unwrapped.Float float ->
                Float float

            Unwrapped.Char char ->
                Char char

            Unwrapped.String string ->
                String string

            Unwrapped.Bool bool ->
                Bool bool

            Unwrapped.Var var_ ->
                Var var_

            Unwrapped.Argument name ->
                Argument name

            Unwrapped.Plus e1 e2 ->
                Plus
                    (fromUnwrapped e1)
                    (fromUnwrapped e2)

            Unwrapped.Cons e1 e2 ->
                Cons
                    (fromUnwrapped e1)
                    (fromUnwrapped e2)

            Unwrapped.Lambda { argument, body } ->
                Lambda
                    { argument = argument
                    , body = fromUnwrapped body
                    }

            Unwrapped.Call { fn, argument } ->
                Call
                    { fn = fromUnwrapped fn
                    , argument = fromUnwrapped argument
                    }

            Unwrapped.If { test, then_, else_ } ->
                If
                    { test = fromUnwrapped test
                    , then_ = fromUnwrapped then_
                    , else_ = fromUnwrapped else_
                    }

            Unwrapped.Let { bindings, body } ->
                Let
                    { bindings =
                        Dict.map
                            (always (Binding.map fromUnwrapped))
                            bindings
                    , body = fromUnwrapped body
                    }

            Unwrapped.List list ->
                List
                    (List.map fromUnwrapped list)

            Unwrapped.Unit ->
                Unit

            Unwrapped.Tuple e1 e2 ->
                Tuple
                    (fromUnwrapped e1)
                    (fromUnwrapped e2)

            Unwrapped.Tuple3 e1 e2 e3 ->
                Tuple3
                    (fromUnwrapped e1)
                    (fromUnwrapped e2)
                    (fromUnwrapped e3)
