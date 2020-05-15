module Elm.AST.Frontend exposing
    ( ProjectFields
    , LocatedExpr, Expr(..), unwrap, transform, recurse
    )

{-| Frontend AST is the first stage after parsing from source code, and has
the closest resemblance to the Elm source code. Eg. all the comments etc. are
still there.

@docs ProjectFields
@docs LocatedExpr, Expr, unwrap, transform, recurse

-}

import Dict exposing (Dict)
import Elm.AST.Frontend.Unwrapped as Unwrapped
import Elm.Data.Binding as Binding exposing (Binding)
import Elm.Data.Located as Located exposing (Located)
import Elm.Data.Module exposing (Module)
import Elm.Data.ModuleName exposing (ModuleName)
import Elm.Data.VarName exposing (VarName)
import Transform


{-| "What does this compiler stage need to store abotut the whole project?

(See [`Elm.Data.Project`](Elm.Data.Project).)

In this case, a dict of all the compiled Elm [modules](Elm.Data.Module#Module)
that hold [frontend AST expressions](#LocatedExpr).

-}
type alias ProjectFields =
    { modules : Dict ModuleName (Module LocatedExpr) }


{-| The main type of this module. Expression with [location metadata](Elm.Data.Located).

Note the underlying [`Expr`](#Expr) custom type recurses on this [`LocatedExpr`](#LocatedExpr) type,
so that the children also each have their location metadata.

If you want expressions without location metadata, look at [`unwrap`](#unwrap).

-}
type alias LocatedExpr =
    Located Expr


{-| -}
type Expr
    = Int Int
    | Float Float
    | Char Char
    | String String
    | Bool Bool
    | Var { module_ : Maybe ModuleName, name : VarName }
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
    | Record (List (Binding LocatedExpr))
    | RecordAccessor VarName


{-| A helper for the [Transform](/packages/Janiczek/transform/latest/) library.
Runs the given function recursively on all children.
-}
recurse : (Expr -> Expr) -> Expr -> Expr
recurse f expr =
    let
        f_ =
            Located.map f
    in
    case expr of
        Int _ ->
            expr

        Float _ ->
            expr

        Char _ ->
            expr

        String _ ->
            expr

        Bool _ ->
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

        Record bindings ->
            Record <| List.map (Binding.map f_) bindings

        RecordAccessor _ ->
            expr


{-| [Transform](/packages/Janiczek/transform/latest/Transform#transformAll)
the expression using the provided function.

Start at the children, repeatedly apply on them until they stop changing,
then go up.

-}
transform : (Expr -> Expr) -> Expr -> Expr
transform pass expr =
    {- If we do more than one at the same time, we should use the Transform
       library a bit differently, see `Transform.orList`.
    -}
    Transform.transformAll
        recurse
        (Transform.toMaybe pass)
        expr


{-| Discard the [location metadata](Elm.Data.Located#Located).
-}
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

        Record bindings ->
            Unwrapped.Record <|
                List.map (Binding.map unwrap) bindings

        RecordAccessor name ->
            Unwrapped.RecordAccessor name
