module Elm.Data.Type.Concrete exposing
    ( ConcreteType(..)
    , toType
    , toTypeOrId
    )

{-| A variant of Type that is always Type, never Id.

This simplifies some recursion, like in Function.

-}

import Dict exposing (Dict)
import Elm.Data.Type as Type exposing (Type, TypeOrId(..))
import Elm.Data.VarName exposing (VarName)


type ConcreteType a
    = TypeVar String
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


toTypeOrId : ConcreteType a -> TypeOrId a
toTypeOrId type_ =
    Type <| toType type_


toType : ConcreteType a -> Type a
toType type_ =
    let
        f =
            Type << toType
    in
    case type_ of
        TypeVar string ->
            Type.TypeVar string

        Function { from, to } ->
            Type.Function
                { from = f from
                , to = f to
                }

        Int ->
            Type.Int

        Float ->
            Type.Float

        Char ->
            Type.Char

        String ->
            Type.String

        Bool ->
            Type.Bool

        List listType ->
            Type.List (f listType)

        Unit ->
            Type.Unit

        Tuple a b ->
            Type.Tuple (f a) (f b)

        Tuple3 a b c ->
            Type.Tuple3 (f a) (f b) (f c)

        Record bindings ->
            Type.Record <| Dict.map (always f) bindings

        UserDefinedType { qualifiedness, name, args } ->
            Type.UserDefinedType
                { qualifiedness = qualifiedness
                , name = name
                , args = List.map f args
                }
