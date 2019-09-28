module Elm.AST.Canonical.Unwrapped exposing (Expr(..))

import Dict exposing (Dict)
import Elm.Data.Binding exposing (Binding)
import Elm.Data.ModuleName exposing (ModuleName)
import Elm.Data.VarName exposing (VarName)


{-| This only differs from AST.Canonical.Expr by recursing on itself instead of
on LocatedExpr. Handy for type inference fuzzers!
-}
type Expr
    = Int Int
    | Float Float
    | Char Char
    | String String
    | Bool Bool
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
