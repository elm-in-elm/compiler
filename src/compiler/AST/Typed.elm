module AST.Typed exposing
    ( Expr
    , Expr_(..)
    , ProjectFields
    , lambda
    , let_
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
  - Lambda taking `argumentId` to help with typechecking
    (TODO describe what it is and what it means)

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
        , argumentId : Int
        , body : Expr
        }
    | Call { fn : Expr, argument : Expr }
    | If { test : Expr, then_ : Expr, else_ : Expr }
    | Let { bindings : AnyDict String VarName (Binding Expr), body : Expr }
    | List { list: List Expr }
    | Unit


lambda : VarName -> Expr -> Int -> Expr_
lambda argument body argumentId =
    Lambda
        { argument = argument
        , argumentId = argumentId
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

        List { list } ->
            List
                { list = List.map f list
                }

        Unit ->
            expr
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
