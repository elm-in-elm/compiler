module Elm.Data.Type.Concrete exposing (ConcreteType(..))

{-| A variant of Type that is always Type, never Id
-}

import Dict exposing (Dict)
import Elm.Data.VarName exposing (VarName)


type ConcreteType a
    = Var String
    | Function
        { from : ConcreteType a
        , to : ConcreteType a
        }
    | Int
    | Float
    | Char
    | String
    | Bool
    | List (ConcreteType a)
    | Unit
    | Tuple (ConcreteType a) (ConcreteType a)
    | Tuple3 (ConcreteType a) (ConcreteType a) (ConcreteType a)
    | Record (Dict VarName (ConcreteType a))
    | UserDefinedType
        { qualifiedness : a
        , name : String
        , args : List (ConcreteType a)
        }
