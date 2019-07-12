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
    )

import AST.Common.Literal exposing (Literal)
import AST.Common.Located as Located exposing (Located)
import AST.Common.Type exposing (Type)
import Common
import Common.Types
    exposing
        ( Binding
        , ModuleName
        , Modules
        , VarName
        )
import Dict.Any exposing (AnyDict)
import Transform


type alias ProjectFields =
    { modules : Modules LocatedExpr }


{-| Differs from Canonical.LocatedExpr by:

  - being a tuple of the underlying LocatedExpr\_ type and its type

TODO make this opaque, add accessors etc.

-}
type alias LocatedExpr =
    Located Expr


type alias Expr =
    ( Expr_, Type )


type Expr_
    = Literal Literal
    | Var { qualifier : ModuleName, name : VarName }
    | Argument VarName
    | Plus LocatedExpr LocatedExpr
    | Lambda
        { argument : VarName
        , body : LocatedExpr
        }
    | Call { fn : LocatedExpr, argument : LocatedExpr }
    | If { test : LocatedExpr, then_ : LocatedExpr, else_ : LocatedExpr }
    | Let { bindings : AnyDict String VarName (Binding LocatedExpr), body : LocatedExpr }
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


let_ : AnyDict String VarName (Binding LocatedExpr) -> LocatedExpr -> Expr_
let_ bindings body =
    Let
        { bindings = bindings
        , body = body
        }



--TODO: Refactor the rest using Transform


{-| A helper for the Transform library.
-}
recurse : (LocatedExpr -> LocatedExpr) -> LocatedExpr -> LocatedExpr
recurse f expr =
    mapExpr
        (\expr_ ->
            case expr_ of
                Literal _ ->
                    expr_

                Var _ ->
                    expr_

                Argument _ ->
                    expr_

                Plus e1 e2 ->
                    Plus
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
                        { bindings = Dict.Any.map (always (Common.mapBinding f)) bindings
                        , body = f body
                        }

                List items ->
                    List (List.map f items)

                Tuple e1 e2 ->
                    Tuple (f e1) (f e2)

                Tuple3 e1 e2 e3 ->
                    Tuple3 (f e1) (f e2) (f e3)

                Unit ->
                    expr_
        )
        expr


transformOnce : (LocatedExpr -> LocatedExpr) -> LocatedExpr -> LocatedExpr
transformOnce pass expr_ =
    Transform.transformOnce
        recurse
        pass
        expr_


transformAll : List (LocatedExpr -> Maybe LocatedExpr) -> LocatedExpr -> LocatedExpr
transformAll passes expr_ =
    Transform.transformAll
        recurse
        (Transform.orList passes)
        expr_


isArgument : VarName -> LocatedExpr -> Bool
isArgument name expr =
    case getExpr expr of
        Argument argName ->
            argName == name

        _ ->
            False


recursiveChildren : (LocatedExpr -> List LocatedExpr) -> LocatedExpr -> List LocatedExpr
recursiveChildren fn expr =
    case getExpr expr of
        Literal _ ->
            []

        Var _ ->
            []

        Argument _ ->
            []

        Plus left right ->
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
                ++ List.concatMap (.body >> fn) (Dict.Any.values bindings)

        Unit ->
            []

        List items ->
            List.concatMap fn items

        Tuple e1 e2 ->
            fn e1 ++ fn e2

        Tuple3 e1 e2 e3 ->
            fn e1 ++ fn e2 ++ fn e3


mapExpr : (a -> b) -> Located ( a, c ) -> Located ( b, c )
mapExpr =
    Located.map << Tuple.mapFirst


getExpr : Located ( a, b ) -> a
getExpr =
    Tuple.first << Located.unwrap


getType : Located ( a, b ) -> b
getType =
    Tuple.second << Located.unwrap
