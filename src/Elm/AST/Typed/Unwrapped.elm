module Elm.AST.Typed.Unwrapped exposing (Expr, Expr_(..))

{-| Version of [Typed AST](Elm.AST.Typed) without the location info.

Handy for type inference tests, or when you don't need the location info.

Convert to it using the [`Elm.AST.Typed.unwrap`](Elm.AST.Typed#unwrap).

@docs Expr, Expr_

-}

import Dict exposing (Dict)
import Elm.Data.Binding exposing (Binding)
import Elm.Data.ModuleName exposing (ModuleName)
import Elm.Data.Type exposing (Type)
import Elm.Data.VarName exposing (VarName)


{-| -}
type alias Expr =
    ( Expr_, Type )


{-| Note this type recurses not on itself but on Expr (so that children also hold
type information).
-}
type Expr_
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
