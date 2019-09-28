module Elm.Data.Type exposing (Type(..), TypeArgument(..), isParametric, varId)

{-| A data structure representing the Elm types.

@docs Type, TypeArgument, isParametric, varId

-}

import Dict exposing (Dict)


{-| -}
type Type
    = {- READ THIS!

         When adding a case that recurs on Type, you'll have to add a case to
         `InferTypes.Unify.unify`:

             | MyNewType Type Type

         will have to get a case:

             (MyNewType m1e1 m1e2, MyNewType m2e1 m2e2) ->
                 substitutionMap
                     |> unify m1e1 m2e1
                     |> Result.andThen (unify m1e2 m2e2)

      -}
      Var Int
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
    | {- The actual definitions of type aliases and custom types are elsewhere,
         this is just a "pointer", "var".

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
    | TypeVariable String


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

        _ ->
            False
