module AST.Canonical.Unwrapped exposing (Expr(..))

import AST.Common.Literal exposing (Literal)
import Common.Types
    exposing
        ( Binding
        , ModuleName
        , VarName
        )
import Dict.Any exposing (AnyDict)


{-| This only differs from AST.Canonical.Expr by recursing on itself instead of
on LocatedExpr. Handy for type inference fuzzers!
-}
type Expr
    = Literal Literal
    | Var { qualifier : ModuleName, name : VarName }
    | Argument VarName
    | Plus Expr Expr
    | Cons Expr Expr
    | Lambda { argument : VarName, body : Expr }
    | Call { fn : Expr, argument : Expr }
    | If { test : Expr, then_ : Expr, else_ : Expr }
    | Let { bindings : AnyDict String VarName (Binding Expr), body : Expr }
    | List (List Expr)
    | Unit
    | Tuple Expr Expr
    | Tuple3 Expr Expr Expr
