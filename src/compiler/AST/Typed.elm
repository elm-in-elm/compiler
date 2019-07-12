module AST.Typed exposing
    ( Expr
    , Expr_(..)
    , ProjectFields
    , isArgument
    , lambda
    , let_
    , recursiveChildren
    , transformAll
    , transformOnce
    )

import AST.Common.Literal exposing (Literal)
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
    { modules : Modules Expr }


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
    | Plus Expr Expr
    | Lambda
        { argument : VarName
        , body : Expr
        }
    | Call { fn : Expr, argument : Expr }
    | If { test : Expr, then_ : Expr, else_ : Expr }
    | Let { bindings : AnyDict String VarName (Binding Expr), body : Expr }
    | List (List Expr)
    | Unit
    | Tuple Expr Expr
    | Tuple3 Expr Expr Expr


lambda : VarName -> Expr -> Expr_
lambda argument body =
    Lambda
        { argument = argument
        , body = body
        }


let_ : AnyDict String VarName (Binding Expr) -> Expr -> Expr_
let_ bindings body =
    Let
        { bindings = bindings
        , body = body
        }


{-| A helper for the Transform library.
-}
recurse : (Expr -> Expr) -> Expr -> Expr
recurse f ( expr, type_ ) =
    ( case expr of
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
                { bindings = Dict.Any.map (always (Common.mapBinding f)) bindings
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
    , type_
    )


transformOnce : (Expr -> Expr) -> Expr -> Expr
transformOnce pass expr =
    Transform.transformOnce
        recurse
        pass
        expr


transformAll : List (Expr -> Maybe Expr) -> Expr -> Expr
transformAll passes expr =
    Transform.transformAll
        recurse
        (Transform.orList passes)
        expr


isArgument : VarName -> Expr -> Bool
isArgument name ( expr_, _ ) =
    case expr_ of
        Argument argName ->
            argName == name

        _ ->
            False


recursiveChildren : (Expr -> List Expr) -> Expr -> List Expr
recursiveChildren fn ( expr, _ ) =
    case expr of
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
