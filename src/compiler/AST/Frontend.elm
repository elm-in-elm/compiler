module AST.Frontend exposing
    ( Expr(..)
    , LocatedExpr
    , ProjectFields
    , lambda
    , transform
    , var
    )

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



{- Let's not get ahead of ourselves

   | Let
       { varName : VarName
       , varBody : LocatedExpr
       , body : LocatedExpr
       }
   | Fixpoint LocatedExpr
   | Operator
       { opName : VarName
       , left : LocatedExpr
       , right : LocatedExpr
       }
-}


{-| A helper for the Transform library.
-}
recurse : (Expr -> Expr) -> Expr -> Expr
recurse f expr =
    case expr of
        Literal _ ->
            expr

        Var _ ->
            expr

        Argument _ ->
            expr

        Plus e1 e2 ->
            Plus
                (Located.map f e1)
                (Located.map f e2)

        Lambda ({ body } as lambda_) ->
            Lambda { lambda_ | body = Located.map f body }

        Call { fn, argument } ->
            Call
                { fn = Located.map f fn
                , argument = Located.map f argument
                }

        If { test, then_, else_ } ->
            If
                { test = Located.map f test
                , then_ = Located.map f then_
                , else_ = Located.map f else_
                }

        Let { bindings, body } ->
            Let
                { bindings = List.map (Common.mapBinding (Located.map f)) bindings
                , body = Located.map f body
                }

        List items ->
            List (List.map (Located.map f) items)

        Unit ->
            expr

        Tuple e1 e2 ->
            Tuple (Located.map f e1) (Located.map f e2)

        Tuple3 e1 e2 e3 ->
            Tuple3 (Located.map f e1) (Located.map f e2) (Located.map f e3)


transform : (Expr -> Expr) -> Expr -> Expr
transform pass expr =
    {- If we do more than one at the same time, we should use the Transform
       library a bit differently, see `Transform.orList`.
    -}
    Transform.transformAll
        recurse
        (Transform.toMaybe pass)
        expr
