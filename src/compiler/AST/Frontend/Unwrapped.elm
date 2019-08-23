module AST.Frontend.Unwrapped exposing (Expr(..))

import AST.Common.Literal exposing (Literal)
import Data.Binding exposing (Binding)
import Data.ModuleName exposing (ModuleName)
import Data.VarName exposing (VarName)


{-| This only differs from AST.Frontend.Expr by recursing on itself instead of
on LocatedExpr. Handy for parser tests or when you don't need the location info!
-}
type Expr
    = Literal Literal
    | Var { qualifier : Maybe ModuleName, name : VarName }
    | Argument VarName
    | Plus Expr Expr
    | Cons Expr Expr
    | ListConcat Expr Expr
    | Lambda { arguments : List VarName, body : Expr }
    | Call { fn : Expr, argument : Expr }
    | If { test : Expr, then_ : Expr, else_ : Expr }
    | Let { bindings : List (Binding Expr), body : Expr }
    | List (List Expr)
    | Unit
    | Tuple Expr Expr
    | Tuple3 Expr Expr Expr
