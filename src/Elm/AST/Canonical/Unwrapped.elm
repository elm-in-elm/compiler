module Elm.AST.Canonical.Unwrapped exposing
    ( Expr(..)
    , Pattern(..)
    )

{-| Version of [Canonical AST](Elm.AST.Canonical) without the location info.

Handy for type inference fuzzers, or when you don't need the location info.

Convert to it using the [`Elm.AST.Canonical.unwrap`](Elm.AST.Canonical#unwrap)
and from it using [`Elm.AST.Canonical.fromUnwrapped`](Elm.AST.Canonical#fromUnwrapped)
(beware, uses [dummy location data!](Elm.Data.Located#dummyRegion)).

@docs Expr

-}

import Dict exposing (Dict)
import Elm.Data.Binding exposing (Binding)
import Elm.Data.ModuleName exposing (ModuleName)
import Elm.Data.VarName exposing (VarName)
import List.NonEmpty exposing (NonEmpty)


{-| -}
type Expr
    = Int Int
    | Float Float
    | Char Char
    | String String
    | Bool Bool
    | Var { module_ : ModuleName, name : VarName }
    | ConstructorValue { module_ : ModuleName, name : VarName }
    | Argument VarName
    | BinOp String Expr Expr
    | Lambda { argument : VarName, body : Expr }
    | Call { fn : Expr, argument : Expr }
    | If { test : Expr, then_ : Expr, else_ : Expr }
    | Let { bindings : Dict VarName (Binding Expr), body : Expr }
    | List (List Expr)
    | Unit
    | Tuple Expr Expr
    | Tuple3 Expr Expr Expr
    | Record (Dict VarName (Binding Expr))
    | RecordAccess Expr String
    | Case Expr (NonEmpty { pattern : Pattern, body : Expr })


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
    | PChar Char
    | PString String
    | PInt Int
    | PFloat Float
