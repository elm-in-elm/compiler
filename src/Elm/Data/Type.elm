module Elm.Data.Type exposing
    ( Type(..), TypeOrId(..)
    , isParametric, mapType, mapTypeOrId
    , varName, varName_, varNames, varNames_
    , getId, getType
    )

{-| A data structure representing the Elm types.

The main confusion point here is "what is the

@docs Type, TypeOrId
@docs isParametric, mapType, mapTypeOrId
@docs varName, varName_, varNames, varNames_
@docs getId, getType

-}

import Dict exposing (Dict)
import Elm.Data.ModuleName exposing (ModuleName)
import Elm.Data.VarName exposing (VarName)
import Transform


{-| -}
type TypeOrId qualifiedness
    = Id Int
    | Type (Type qualifiedness)


{-| The `a` here is the same as the `qualifiedness` in `TypeOrId` above.
(We're shortening it to `a` for convenience.) See `Elm.Data.Qualifiedness`!

An example:

`MyModule.MyDataStructure` is a qualified type reference, so it will first be:

    UserDefinedType
        { module = PossiblyQualified (Just "MyModule")
        , name = "MyDataStructure"
        , args = []
        }
      : Type PossiblyQualified

in the Frontend stage, and then change into:

    UserDefinedType
        { module = Qualified "MyModule"
        , name = "MyDataStructure"
        , args = []
        }
      : Type Qualified

On the other hand, a `MyDataStructure` type in your source code begins as:

    UserDefinedType
        { module = PossiblyQualified Nothing
        , name = "MyDataStructure"
        , args = []
        }
      : Type PossiblyQualified

in the Frontend stage, and only then, when desugaring, has the module found for
it (or error message raised) and becomes:

    UserDefinedType
        { module = Qualified "MyModule"
        , name = "MyDataStructure"
        , args = []
        }
      : Type Qualified

We hold this possibility of not being optional in the type level (phantom types
FTW), to make tasks in the stages after desugaring easier (impossible states
become impossible, and a part of the desugaring task is:

    Type PossiblyQualified -> Type Qualified

-}
type Type a
    = Var String {- in `foo : a -> Int`, `a` is `Var "a"` -}
    | Function
        { from : TypeOrId a
        , to : TypeOrId a
        }
    | Int
    | Float
    | Char
    | String
    | Bool
    | List (TypeOrId a)
    | Unit
    | Tuple (TypeOrId a) (TypeOrId a)
    | Tuple3 (TypeOrId a) (TypeOrId a) (TypeOrId a)
    | Record (Dict VarName (TypeOrId a))
    | {- The actual definitions of type aliases and custom types are elsewhere
         (in the Declaration module), this is just a "pointer", "var".

         Also, this is the *usage* of a type! So while definition of Maybe
         might be `Maybe a`, here you'll most likely see specific stuff
         like `Maybe Int`.

         This constructor encompasses both type aliases and custom types:
      -}
      UserDefinedType
        { module_ : a
        , name : String
        , args : List (TypeOrId a)
        }


{-| Unwrap the string inside the type variable
-}
varName : Type a -> Maybe String
varName type_ =
    case type_ of
        Var string ->
            Just string

        _ ->
            Nothing


{-| Unwrap the string inside the type variable
-}
varName_ : TypeOrId a -> Maybe String
varName_ typeOrId =
    case typeOrId of
        Id _ ->
            Nothing

        Type type_ ->
            varName type_


getId : TypeOrId a -> Maybe Int
getId typeOrId =
    case typeOrId of
        Id id ->
            Just id

        Type _ ->
            Nothing


getType : TypeOrId a -> Maybe (Type a)
getType typeOrId =
    case typeOrId of
        Id _ ->
            Nothing

        Type type_ ->
            Just type_


{-| Does it contain lower-case type parameters?
-}
isParametric : Type a -> Bool
isParametric type_ =
    let
        fn_ : TypeOrId a -> Bool
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


varNames : Type a -> List String
varNames type_ =
    type_
        |> Transform.children recursiveChildren
        |> List.filterMap varName


varNames_ : TypeOrId a -> List String
varNames_ typeOrId =
    typeOrId
        |> Transform.children recursiveChildren_
        |> List.filterMap varName_


{-| Find all the children of this expression (and their children, etc...)
-}
recursiveChildren : (Type a -> List (Type a)) -> Type a -> List (Type a)
recursiveChildren fn type_ =
    let
        fn_ : TypeOrId a -> List (Type a)
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
recursiveChildren_ : (TypeOrId a -> List (TypeOrId a)) -> TypeOrId a -> List (TypeOrId a)
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


mapTypeOrId : (a -> b) -> TypeOrId a -> TypeOrId b
mapTypeOrId fn typeOrId =
    case typeOrId of
        Id id ->
            Id id

        Type type_ ->
            Type <| mapType fn type_


mapType : (a -> b) -> Type a -> Type b
mapType fn type_ =
    case type_ of
        Var str ->
            Var str

        Function { from, to } ->
            Function
                { from = mapTypeOrId fn from
                , to = mapTypeOrId fn to
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
            List <| mapTypeOrId fn typeOrId

        Unit ->
            Unit

        Tuple a b ->
            Tuple
                (mapTypeOrId fn a)
                (mapTypeOrId fn b)

        Tuple3 a b c ->
            Tuple3
                (mapTypeOrId fn a)
                (mapTypeOrId fn b)
                (mapTypeOrId fn c)

        Record dict ->
            Record <| Dict.map (\k v -> mapTypeOrId fn v) dict

        UserDefinedType r ->
            UserDefinedType
                { module_ = fn r.module_
                , name = r.name
                , args = List.map (mapTypeOrId fn) r.args
                }
