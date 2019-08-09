module AST.Typed exposing
    ( Expr
    , Expr_(..)
    , LocatedExpr
    , ProjectFields
    , getExpr
    , getType
    , isArgument
    , lambda
    , let_
    , mapExpr
    , recursiveChildren
    , transformAll
    , transformOnce
    , unwrap
    )

import AST.Common.Literal exposing (Literal)
import AST.Common.Located as Located exposing (Located)
import AST.Common.Type exposing (Type)
import AST.Typed.Unwrapped as Unwrapped
import AssocList as Dict exposing (Dict)
import Data.Binding as Binding exposing (Binding)
import Data.Module exposing (Modules)
import Data.ModuleName exposing (ModuleName)
import Data.VarName exposing (VarName)
import Transform


type alias ProjectFields =
    { modules : Modules LocatedExpr }


type alias LocatedExpr =
    Located Expr


{-| Differs from Canonical.Expr by:

  - being a tuple of the underlying Expr\_ type and its type

TODO make this opaque, add accessors etc.

-}
type alias Expr =
    ( Expr_, Type )


type Expr_
    = Literal Literal
    | Var { qualifier : ModuleName, name : VarName }
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


lambda : VarName -> LocatedExpr -> Expr_
lambda argument body =
    Lambda
        { argument = argument
        , body = body
        }


let_ : Dict VarName (Binding LocatedExpr) -> LocatedExpr -> Expr_
let_ bindings body =
    Let
        { bindings = bindings
        , body = body
        }


{-| A helper for the Transform library.
-}
recurse : (LocatedExpr -> LocatedExpr) -> LocatedExpr -> LocatedExpr
recurse f located =
    mapExpr
        (\expr ->
            case expr of
                Literal _ ->
                    expr

                Var _ ->
                    expr

                Argument _ ->
                    expr

                Plus e1 e2 ->
                    Plus
                        (f e1)
                        (f e2)

                Cons e1 e2 ->
                    Cons
                        (f e1)
                        (f e2)

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
                        { bindings =
                            Dict.map
                                (always (Binding.map f))
                                bindings
                        , body = f body
                        }

                List items ->
                    List (List.map f items)

                Tuple e1 e2 ->
                    Tuple (f e1) (f e2)

                Tuple3 e1 e2 e3 ->
                    Tuple3 (f e1) (f e2) (f e3)

                Unit ->
                    expr
        )
        located


transformOnce : (LocatedExpr -> LocatedExpr) -> LocatedExpr -> LocatedExpr
transformOnce pass located =
    Transform.transformOnce
        recurse
        pass
        located


transformAll : List (LocatedExpr -> Maybe LocatedExpr) -> LocatedExpr -> LocatedExpr
transformAll passes located =
    Transform.transformAll
        recurse
        (Transform.orList passes)
        located


isArgument : VarName -> LocatedExpr -> Bool
isArgument name located =
    case getExpr located of
        Argument argName ->
            argName == name

        _ ->
            False


recursiveChildren : (LocatedExpr -> List LocatedExpr) -> LocatedExpr -> List LocatedExpr
recursiveChildren fn located =
    case getExpr located of
        Literal _ ->
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


mapExpr : (Expr_ -> Expr_) -> LocatedExpr -> LocatedExpr
mapExpr =
    Located.map << Tuple.mapFirst


getExpr : LocatedExpr -> Expr_
getExpr =
    Tuple.first << Located.unwrap


getType : LocatedExpr -> Type
getType =
    Tuple.second << Located.unwrap


unwrap : LocatedExpr -> Unwrapped.Expr
unwrap expr =
    let
        ( expr_, type_ ) =
            Located.unwrap expr
    in
    ( case expr_ of
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
    , type_
    )
