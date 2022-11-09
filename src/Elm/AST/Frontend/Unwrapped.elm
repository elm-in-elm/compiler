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
import Elm.Data.Qualifiedness exposing (PossiblyQualified)
import Elm.Data.VarName exposing (VarName)
import List.NonEmpty exposing (NonEmpty)


{-| -}
type Expr
    = Int Int
    | HexInt Int
    | Float Float
    | Char Char
    | String String
    | Bool Bool
    | Var { qualifiedness : PossiblyQualified, name : VarName }
    | ConstructorValue { qualifiedness : PossiblyQualified, name : VarName }
    | Argument VarName
    | BinOp String Expr Expr
    | Lambda { arguments : NonEmpty VarName, body : Expr }
    | Call { fn : Expr, argument : Expr }
    | If { test : Expr, then_ : Expr, else_ : Expr }
    | Let { bindings : NonEmpty (Binding Expr), body : Expr }
    | List (List Expr)
    | Unit
    | Tuple Expr Expr
    | Tuple3 Expr Expr Expr
    | Record (List (Binding Expr))
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
