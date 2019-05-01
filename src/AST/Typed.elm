module AST.Typed exposing
    ( Expr
    , Expr_(..)
    , ProjectFields
    , call
    , if_
    , lambda
    , transformOnce
    )

import AST.Common.Literal exposing (Literal)
import AST.Common.Type exposing (Type)
import Common.Types
    exposing
        ( ModuleName
        , Modules
        , VarName
        )
import Transform


type alias ProjectFields =
    { modules : Modules Expr }


{-| Differs from Canonical.Expr by:

  - being a tuple of the underlying Expr\_ type and its type

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


lambda : VarName -> Expr -> Int -> Expr_
lambda argument body argumentId =
    Lambda
        { argument = argument
        , argumentId = argumentId
        , body = body
        }


call : Expr -> Expr -> Expr_
call fn argument =
    Call
        { fn = fn
        , argument = argument
        }


if_ : Expr -> Expr -> Expr -> Expr_
if_ test then_ else_ =
    If
        { test = test
        , then_ = then_
        , else_ = else_
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
    , type_
    )


transformOnce : (Expr -> Expr) -> Expr -> Expr
transformOnce pass expr =
    Transform.transformOnce
        recurse
        pass
        expr