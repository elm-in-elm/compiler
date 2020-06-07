module Elm.AST.Frontend.Unwrapped exposing
    ( Expr(..)
    , Pattern(..)
    )

{-| Version of [Frontend AST](Elm.AST.Frontend) without the location info.

Handy for parser tests, or when you don't need the location info.

Convert to it using the [`Elm.AST.Frontend.unwrap`](Elm.AST.Frontend#unwrap).

@docs Expr

-}

import Elm.Data.Binding exposing (Binding)
import Elm.Data.ModuleName exposing (ModuleName)
import Elm.Data.VarName exposing (VarName)


{-| -}
type Expr
    = Int Int
    | Float Float
    | Char Char
    | String String
    | Bool Bool
    | Var { module_ : Maybe ModuleName, name : VarName }
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
    | Record (List (Binding Expr))
    | Case Expr (List { pattern : Pattern, body : Expr })


type Pattern
    = PAnything
    | PVar VarName
    | PRecord (List VarName)
    | PAlias Pattern VarName
    | PUnit
    | PTuple Pattern Pattern
    | PTuple3 Pattern Pattern Pattern
    | PList (List Pattern)
    | PCons Pattern Pattern
    | PBool Bool
    | PChar Char
    | PString String
    | PInt Int
    | PFloat Float
