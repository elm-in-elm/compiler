module Elm.Data.Type.Concrete exposing
    ( ConcreteType(..)
    , combine
    , map
    , toType
    , toTypeOrId
    )

{-| A variant of Type that is always Type, never Id.

This simplifies some recursion, like in Function.

-}

import Dict exposing (Dict)
import Elm.Data.Type as Type exposing (Type, TypeOrId(..))
import Elm.Data.VarName exposing (VarName)
import OurExtras.Dict as Dict
import Result.Extra


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


map : (a -> b) -> ConcreteType a -> ConcreteType b
map fn type_ =
    let
        f =
            map fn
    in
    case type_ of
        TypeVar str ->
            TypeVar str

        Function { from, to } ->
            Function
                { from = f from
                , to = f to
                }

        Int ->
            Int

        Float ->
            Float

        Char ->
            Char

        String ->
            String

        Bool ->
            Bool

        List typeOrId ->
            List <| f typeOrId

        Unit ->
            Unit

        Tuple a b ->
            Tuple
                (f a)
                (f b)

        Tuple3 a b c ->
            Tuple3
                (f a)
                (f b)
                (f c)

        Record dict ->
            Record <| Dict.map (always f) dict

        UserDefinedType r ->
            UserDefinedType
                { qualifiedness = fn r.qualifiedness
                , name = r.name
                , args = List.map f r.args
                }


combine : ConcreteType (Result err a) -> Result err (ConcreteType a)
combine type_ =
    let
        f =
            combine
    in
    case type_ of
        TypeVar string ->
            Ok <| TypeVar string

        Function { from, to } ->
            Result.map2
                (\from_ to_ ->
                    Function
                        { from = from_
                        , to = to_
                        }
                )
                (f from)
                (f to)

        Int ->
            Ok Int

        Float ->
            Ok Float

        Char ->
            Ok Char

        String ->
            Ok String

        Bool ->
            Ok Bool

        List listType ->
            f listType
                |> Result.map List

        Unit ->
            Ok Unit

        Tuple a b ->
            Result.map2 Tuple
                (f a)
                (f b)

        Tuple3 a b c ->
            Result.map3 Tuple3
                (f a)
                (f b)
                (f c)

        Record bindings ->
            bindings
                |> Dict.map (always f)
                |> Dict.combine
                |> Result.map Record

        UserDefinedType { qualifiedness, name, args } ->
            Result.map2
                (\qualifiedness_ args_ ->
                    UserDefinedType
                        { qualifiedness = qualifiedness_
                        , name = name
                        , args = args_
                        }
                )
                qualifiedness
                (args
                    |> List.map f
                    |> Result.Extra.combine
                )
