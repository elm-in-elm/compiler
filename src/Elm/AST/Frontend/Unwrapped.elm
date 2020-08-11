module Elm.AST.Frontend.Unwrapped exposing
    ( Expr(..)
    , Pattern(..)
    )

{-| Version of [Frontend AST](Elm.AST.Frontend) without the location info or comments.

Handy for parser tests, or when you don't need the location info and the comments.

Convert to it using the [`Elm.AST.Frontend.unwrap`](Elm.AST.Frontend#unwrap).

@docs Expr

-}

import Elm.Data.Binding as Binding exposing (Binding)
import Elm.Data.Comment exposing (Comment)
import Elm.Data.Qualifiedness exposing (PossiblyQualified)
import Elm.Data.VarName exposing (VarName)


{-| -}
type Expr
    = Unit
    | Bool Bool
    | Int Int
    | Float Float
    | Char Char
    | String String
    | Var { qualifiedness : PossiblyQualified, name : VarName }
    | Argument VarName
    | Tuple Expr Expr
    | Tuple3 Expr Expr Expr
    | Record (List (Binding Expr))
    | List (List Expr)
    | Call
        { fn : Expr
        , argument : Expr
        }
    | Lambda
        { arguments : List VarName
        , body : Expr
        }
    | Plus Expr Expr
    | Cons Expr Expr
    | ListConcat Expr Expr
    | Let
        { bindings : List (Binding Expr)
        , body : Expr
        }
    | If
        { test : Expr
        , then_ : Expr
        , else_ : Expr
        }
    | Case
        { test : Expr
        , branches :
            List
                { pattern : Pattern
                , body : Expr
                }
        }


type Pattern
    = PAnything
    | PUnit
    | PBool Bool
    | PInt Int
    | PFloat Float
    | PChar Char
    | PString String
    | PVar VarName
    | PTuple Pattern Pattern
    | PTuple3 Pattern Pattern Pattern
    | PRecord (List VarName)
    | PList (List Pattern)
    | PAlias Pattern VarName
    | PCons Pattern Pattern
