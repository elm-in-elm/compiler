module Elm.Data.Type exposing (Type(..), TypeArgument(..), isParametric, varId)

{-| A data structure representing the Elm types.

@docs Type, TypeArgument, isParametric, varId

-}

import Dict exposing (Dict)
import Elm.Data.VarName exposing (VarName)


{-| -}
type Type
    = Var Int
    | Function Type Type
    | Int
    | Float
    | Char
    | String
    | Bool
    | List Type
    | Unit
    | Tuple Type Type
    | Tuple3 Type Type Type
    | Record (Dict VarName Type)
    | {- The actual definitions of type aliases and custom types are elsewhere
         (in the Declaration module), this is just a "pointer", "var".

         Also, this is the *usage* of a type! So while definition of Maybe
         might be `Maybe a`, here you'll most likely see specific stuff
         like `Maybe Int`.

         This constructor encompasses both type aliases and custom types:
      -}
      UserDefinedType { module_ : String, name : String } (List Type)


{-| Type argument of a polymorphic type.

    Maybe Int
    --> ConcreteType Int

    Maybe a
    --> TypeVariable "a"

-}
type TypeArgument
    = ConcreteType Type
    | TypeVariable VarName


{-| Unwrap the ID of the type variable
-}
varId : Type -> Maybe Int
varId type_ =
    case type_ of
        Var id ->
            Just id

        _ ->
            Nothing


{-| Does it contain lower-case type parameters?
-}
isParametric : Type -> Bool
isParametric type_ =
    case type_ of
        Var _ ->
            True

        Function input output ->
            [ input, output ]
                |> List.any isParametric

        List element ->
            isParametric element

        Tuple left right ->
            [ left, right ]
                |> List.any isParametric

        Tuple3 left middle right ->
            [ left, middle, right ]
                |> List.any isParametric

        Record bindings ->
            List.any isParametric (Dict.values bindings)

        _ ->
            False
