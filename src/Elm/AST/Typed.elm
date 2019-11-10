module Elm.AST.Typed exposing
    ( ProjectFields
    , LocatedExpr, Expr, Expr_(..), getExpr, getType, unwrap, dropTypes, transformAll, transformOnce, recursiveChildren, setExpr
    )

{-| Typed AST holds the inferred [types](Elm.Data.Type) for every expression.

@docs ProjectFields
@docs LocatedExpr, Expr, Expr_, getExpr, getType, unwrap, dropTypes, transformAll, transformOnce, recursiveChildren, setExpr

-}

import Dict exposing (Dict)
import Elm.AST.Canonical as Canonical
import Elm.AST.Typed.Unwrapped as Unwrapped
import Elm.Data.Binding as Binding exposing (Binding)
import Elm.Data.Located as Located exposing (Located)
import Elm.Data.Module exposing (Module)
import Elm.Data.ModuleName exposing (ModuleName)
import Elm.Data.Type exposing (Type)
import Elm.Data.VarName exposing (VarName)
import Transform


{-| "What does this compiler stage need to store abotut the whole project?

(See [`Elm.Data.Project`](Elm.Data.Project).)

In this case, a dict of all the compiled Elm [modules](Elm.Data.Module#Module)
that hold [typed AST expressions](#LocatedExpr).

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


{-| Differs from [Canonical.Expr](Elm.AST.Canonical#Expr) by:

  - being a tuple of the underlying Expr\_ and its inferred type

-}
type alias Expr =
    ( Expr_, Type )


{-| -}
type Expr_
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
    | Record (Dict VarName (Binding LocatedExpr))


{-| A helper for the [Transform](/packages/Janiczek/transform/latest/) library.
Runs the given function recursively on all children.
-}
recurse : (LocatedExpr -> LocatedExpr) -> LocatedExpr -> LocatedExpr
recurse fn locatedExpr =
    locatedExpr
        |> mapExpr
            (\expr ->
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
                            (fn e1)
                            (fn e2)

                    Cons e1 e2 ->
                        Cons
                            (fn e1)
                            (fn e2)

                    Lambda ({ body } as lambda_) ->
                        Lambda { lambda_ | body = fn body }

                    Call call ->
                        Call
                            { fn = fn call.fn
                            , argument = fn call.argument
                            }

                    If { test, then_, else_ } ->
                        If
                            { test = fn test
                            , then_ = fn then_
                            , else_ = fn else_
                            }

                    Let { bindings, body } ->
                        Let
                            { bindings =
                                Dict.map
                                    (always (Binding.map fn))
                                    bindings
                            , body = fn body
                            }

                    List items ->
                        List (List.map fn items)

                    Tuple e1 e2 ->
                        Tuple
                            (fn e1)
                            (fn e2)

                    Tuple3 e1 e2 e3 ->
                        Tuple3
                            (fn e1)
                            (fn e2)
                            (fn e3)

                    Unit ->
                        expr

                    Record bindings ->
                        Record
                            (Dict.map
                                (always (Binding.map fn))
                                bindings
                            )
            )


{-| Transform the expression once using the provided function.
Start at the children, apply once then go up.
-}
transformOnce : (LocatedExpr -> LocatedExpr) -> LocatedExpr -> LocatedExpr
transformOnce pass locatedExpr =
    Transform.transformOnce
        recurse
        pass
        locatedExpr


{-| Transform the expression using the provided function.
Start at the children, repeatedly apply on them until they stop changing,
then go up.
-}
transformAll : List (LocatedExpr -> Maybe LocatedExpr) -> LocatedExpr -> LocatedExpr
transformAll passes locatedExpr =
    Transform.transformAll
        recurse
        (Transform.orList passes)
        locatedExpr


{-| Find all the children of this expression (and their children, etc...)
-}
recursiveChildren : (LocatedExpr -> List LocatedExpr) -> LocatedExpr -> List LocatedExpr
recursiveChildren fn locatedExpr =
    case getExpr locatedExpr of
        Int _ ->
            []

        Float _ ->
            []

        Char _ ->
            []

        String _ ->
            []

        Bool _ ->
            []

        Var _ ->
            []

        Argument _ ->
            []

        Plus left right ->
            fn left
                ++ fn right

        Cons left right ->
            fn left
                ++ fn right

        Lambda { body } ->
            fn body

        Call data ->
            fn data.fn
                ++ fn data.argument

        If { test, then_, else_ } ->
            fn test
                ++ fn then_
                ++ fn else_

        Let { bindings, body } ->
            fn body
                ++ List.concatMap (.body >> fn) (Dict.values bindings)

        Unit ->
            []

        List items ->
            List.concatMap fn items

        Tuple e1 e2 ->
            fn e1 ++ fn e2

        Tuple3 e1 e2 e3 ->
            fn e1 ++ fn e2 ++ fn e3

        Record bindings ->
            List.concatMap (.body >> fn) (Dict.values bindings)


mapExpr : (Expr_ -> Expr_) -> LocatedExpr -> LocatedExpr
mapExpr fn locatedExpr =
    locatedExpr
        |> Located.map (Tuple.mapFirst fn)


{-| Replace the existing expression with the provided one.
-}
setExpr : Expr_ -> LocatedExpr -> LocatedExpr
setExpr expr locatedExpr =
    mapExpr (always expr) locatedExpr


{-| Extract the expression (remove the location and type information).
-}
getExpr : LocatedExpr -> Expr_
getExpr locatedExpr =
    Tuple.first <| Located.unwrap locatedExpr


{-| Extract the type (remove the location information and the expression).
-}
getType : LocatedExpr -> Type
getType locatedExpr =
    Tuple.second <| Located.unwrap locatedExpr


{-| Discard the [location metadata](Elm.Data.Located#Located).
-}
unwrap : LocatedExpr -> Unwrapped.Expr
unwrap expr =
    let
        ( expr_, type_ ) =
            Located.unwrap expr
    in
    ( case expr_ of
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

        Record bindings ->
            Unwrapped.Record <|
                Dict.map
                    (always (Binding.map unwrap))
                    bindings
    , type_
    )


{-| Go from `AST.Typed` to [`AST.Canonical`](Elm.AST.Canonical).
-}
dropTypes : LocatedExpr -> Canonical.LocatedExpr
dropTypes locatedExpr =
    locatedExpr
        |> Located.map
            (\( expr, _ ) ->
                case expr of
                    Int int ->
                        Canonical.Int int

                    Float float ->
                        Canonical.Float float

                    Char char ->
                        Canonical.Char char

                    String string ->
                        Canonical.String string

                    Bool bool ->
                        Canonical.Bool bool

                    Var var ->
                        Canonical.Var var

                    Argument var ->
                        Canonical.Argument var

                    Plus e1 e2 ->
                        Canonical.Plus
                            (dropTypes e1)
                            (dropTypes e2)

                    Cons e1 e2 ->
                        Canonical.Cons
                            (dropTypes e1)
                            (dropTypes e2)

                    Lambda { argument, body } ->
                        Canonical.Lambda
                            { argument = argument
                            , body = dropTypes body
                            }

                    Call { fn, argument } ->
                        Canonical.Call
                            { fn = dropTypes fn
                            , argument = dropTypes argument
                            }

                    If { test, then_, else_ } ->
                        Canonical.If
                            { test = dropTypes test
                            , then_ = dropTypes then_
                            , else_ = dropTypes else_
                            }

                    Let { bindings, body } ->
                        Canonical.Let
                            { bindings = Dict.map (always (Binding.map dropTypes)) bindings
                            , body = dropTypes body
                            }

                    List exprs ->
                        Canonical.List (List.map dropTypes exprs)

                    Unit ->
                        Canonical.Unit

                    Tuple e1 e2 ->
                        Canonical.Tuple
                            (dropTypes e1)
                            (dropTypes e2)

                    Tuple3 e1 e2 e3 ->
                        Canonical.Tuple3
                            (dropTypes e1)
                            (dropTypes e2)
                            (dropTypes e3)

                    Record bindings ->
                        Canonical.Record <|
                            Dict.map (always (Binding.map dropTypes)) bindings
            )
