module Elm.Data.Type exposing
    ( Type(..), TypeOrId(..), isParametric
    , varName, varName_, varNames, getType
    , getId, varNames_
    )

{-| A data structure representing the Elm types.

@docs Type, TypeOrId, isParametric
@docs varName, varName_, varNames, varNames_ getId, getType

-}

import Dict exposing (Dict)
import Elm.Data.VarName exposing (VarName)
import Transform


{-| -}
type TypeOrId
    = Id Int
    | Type Type


{-| -}
type Type
    = Var String {- in `foo : a -> Int`, `a` is `Var "a"` -}
    | Function { from : TypeOrId, to : TypeOrId }
    | Int
    | Float
    | Char
    | String
    | Bool
    | List TypeOrId
    | Unit
    | Tuple TypeOrId TypeOrId
    | Tuple3 TypeOrId TypeOrId TypeOrId
    | Record (Dict VarName TypeOrId)
    | {- The actual definitions of type aliases and custom types are elsewhere
         (in the Declaration module), this is just a "pointer", "var".

         Also, this is the *usage* of a type! So while definition of Maybe
         might be `Maybe a`, here you'll most likely see specific stuff
         like `Maybe Int`.

         This constructor encompasses both type aliases and custom types:
      -}
      UserDefinedType { name : String, args : List TypeOrId }


{-| Unwrap the string inside the type variable
-}
varName : Type -> Maybe String
varName type_ =
    case type_ of
        Var string ->
            Just string

        _ ->
            Nothing


{-| Unwrap the string inside the type variable
-}
varName_ : TypeOrId -> Maybe String
varName_ typeOrId =
    case typeOrId of
        Id _ ->
            Nothing

        Type type_ ->
            varName type_


getId : TypeOrId -> Maybe Int
getId typeOrId =
    case typeOrId of
        Id id ->
            Just id

        Type _ ->
            Nothing


getType : TypeOrId -> Maybe Type
getType typeOrId =
    case typeOrId of
        Id _ ->
            Nothing

        Type type_ ->
            Just type_


{-| Does it contain lower-case type parameters?
-}
isParametric : Type -> Bool
isParametric type_ =
    let
        fn_ : TypeOrId -> Bool
        fn_ typeOrId =
            case typeOrId of
                Id _ ->
                    True

                Type t ->
                    isParametric t
    in
    case type_ of
        Var _ ->
            True

        Function { from, to } ->
            fn_ from || fn_ to

        Int ->
            False

        Float ->
            False

        Char ->
            False

        String ->
            False

        Bool ->
            False

        Unit ->
            False

        List element ->
            fn_ element

        Tuple t1 t2 ->
            fn_ t1 || fn_ t2

        Tuple3 t1 t2 t3 ->
            fn_ t1 || fn_ t2 || fn_ t3

        Record bindings ->
            List.any fn_ (Dict.values bindings)

        UserDefinedType { args } ->
            List.any fn_ args


varNames : Type -> List String
varNames type_ =
    type_
        |> Transform.children recursiveChildren
        |> List.filterMap varName


varNames_ : TypeOrId -> List String
varNames_ typeOrId =
    typeOrId
        |> Transform.children recursiveChildren_
        |> List.filterMap varName_


{-| Find all the children of this expression (and their children, etc...)
-}
recursiveChildren : (Type -> List Type) -> Type -> List Type
recursiveChildren fn type_ =
    let
        fn_ : TypeOrId -> List Type
        fn_ typeOrId =
            case typeOrId of
                Id _ ->
                    []

                Type t ->
                    fn t
    in
    case type_ of
        Var _ ->
            []

        Function _ ->
            []

        Int ->
            []

        Float ->
            []

        Char ->
            []

        String ->
            []

        Bool ->
            []

        List t ->
            fn_ t

        Unit ->
            []

        Tuple t1 t2 ->
            fn_ t1 ++ fn_ t2

        Tuple3 t1 t2 t3 ->
            fn_ t1 ++ fn_ t2 ++ fn_ t3

        Record bindings ->
            List.concatMap fn_ (Dict.values bindings)

        UserDefinedType { args } ->
            List.concatMap fn_ args


{-| Find all the children of this expression (and their children, etc...)
-}
recursiveChildren_ : (TypeOrId -> List TypeOrId) -> TypeOrId -> List TypeOrId
recursiveChildren_ fn typeOrId =
    case typeOrId of
        Id _ ->
            []

        Type (Var _) ->
            []

        Type (Function _) ->
            []

        Type Int ->
            []

        Type Float ->
            []

        Type Char ->
            []

        Type String ->
            []

        Type Bool ->
            []

        Type (List t) ->
            fn t

        Type Unit ->
            []

        Type (Tuple t1 t2) ->
            fn t1 ++ fn t2

        Type (Tuple3 t1 t2 t3) ->
            fn t1 ++ fn t2 ++ fn t3

        Type (Record bindings) ->
            List.concatMap fn (Dict.values bindings)

        Type (UserDefinedType { args }) ->
            List.concatMap fn args
