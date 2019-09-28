module Elm.AST.Typed.Unwrapped exposing (Expr, Expr_(..))

import AssocList exposing (Dict)
import Elm.AST.Common.Literal exposing (Literal)
import Elm.Data.Binding exposing (Binding)
import Elm.Data.ModuleName exposing (ModuleName)
import Elm.Data.Type exposing (Type)
import Elm.Data.VarName exposing (VarName)


{-| This only differs from AST.Typed.Expr by recursing on itself instead of
on LocatedExpr. Handy for type inference tests!
-}
type alias Expr =
    ( Expr_, Type )


type Expr_
    = Literal Literal
    | Var { module_ : ModuleName, name : VarName }
    | Argument VarName
    | Plus Expr Expr
    | Cons Expr Expr
    | Lambda { argument : VarName, body : Expr }
    | Call { fn : Expr, argument : Expr }
    | If { test : Expr, then_ : Expr, else_ : Expr }
    | Let { bindings : Dict VarName (Binding Expr), body : Expr }
    | List (List Expr)
    | Unit
    | Tuple Expr Expr
    | Tuple3 Expr Expr Expr
