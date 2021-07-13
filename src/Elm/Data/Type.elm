module Elm.Data.Type exposing
    ( Type(..), TypeOrId(..), Id
    , isParametric, mapType, mapTypeOrId
    , varName, varName_, varNames, varNames_
    , getId, getType
    , combineType, combineTypeOrId
    )

{-| A data structure representing the Elm types.

The main confusion point here is "what is the

@docs Type, TypeOrId, Id
@docs isParametric, mapType, mapTypeOrId
@docs varName, varName_, varNames, varNames_
@docs getId, getType
@docs combineType, combineTypeOrId

-}

import Dict exposing (Dict)
import Elm.Data.VarName exposing (VarName)
import OurExtras.Dict as Dict
import OurExtras.List as List
import Result.Extra
import Transform


{-| -}
type TypeOrId qualifiedness
    = Id Id
    | Type (Type qualifiedness)


type alias Id =
    Int


{-| The `a` here is the same as the `qualifiedness` in `TypeOrId` above.
(We're shortening it to `a` for convenience.) See `Elm.Data.Qualifiedness`!

An example:

`MyModule.MyDataStructure` is a qualified type reference, so it will first be:

    UserDefinedType
        { qualifiedness = PossiblyQualified (Just "MyModule")
        , name = "MyDataStructure"
        , args = []
        }
      : Type PossiblyQualified

in the Frontend stage, and then change into:

    UserDefinedType
        { qualifiedness = Qualified "MyModule"
        , name = "MyDataStructure"
        , args = []
        }
      : Type Qualified

On the other hand, a `MyDataStructure` type in your source code begins as:

    UserDefinedType
        { qualifiedness = PossiblyQualified Nothing
        , name = "MyDataStructure"
        , args = []
        }
      : Type PossiblyQualified

in the Frontend stage, and only then, when desugaring, has the module found for
it (or error message raised) and becomes:

    UserDefinedType
        { qualifiedness = Qualified "MyModule"
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
    = {- Example of a `TypeVar`:

             foo : a -> Int

         will be parsed as

            TypeAnnotation
                { name = "foo"
                , type_ =
                    Function
                        { from = TypeVar "a"
                        , to = Int
                        }
                }

         These are the type variables user has given name to. (There are also
         `Id 0`-like values which are being given names by the compiler.)
      -}
      TypeVar String
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
        { qualifiedness : a
        , name : String
        , args : List (TypeOrId a)
        }


{-| Unwrap the string inside the type variable
-}
varName : Type a -> Maybe String
varName type_ =
    case type_ of
        TypeVar string ->
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
isParametric : TypeOrId a -> Bool
isParametric typeOrId =
    let
        f =
            isParametric
    in
    case typeOrId of
        Id _ ->
            True

        Type type_ ->
            case type_ of
                TypeVar _ ->
                    True

                Function { from, to } ->
                    f from || f to

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
                    f element

                Tuple t1 t2 ->
                    f t1 || f t2

                Tuple3 t1 t2 t3 ->
                    f t1 || f t2 || f t3

                Record bindings ->
                    List.any f (Dict.values bindings)

                UserDefinedType { args } ->
                    List.any f args


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
        TypeVar _ ->
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
            List.fastConcatMap fn_ (Dict.values bindings)

        UserDefinedType { args } ->
            List.fastConcatMap fn_ args


{-| Find all the children of this expression (and their children, etc...)
-}
recursiveChildren_ : (TypeOrId a -> List (TypeOrId a)) -> TypeOrId a -> List (TypeOrId a)
recursiveChildren_ fn typeOrId =
    case typeOrId of
        Id _ ->
            []

        Type (TypeVar _) ->
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
            List.fastConcatMap fn (Dict.values bindings)

        Type (UserDefinedType { args }) ->
            List.fastConcatMap fn args


mapTypeOrId : (a -> b) -> TypeOrId a -> TypeOrId b
mapTypeOrId fn typeOrId =
    case typeOrId of
        Id id ->
            Id id

        Type type_ ->
            Type <| mapType fn type_


mapType : (a -> b) -> Type a -> Type b
mapType fn type_ =
    let
        f =
            mapTypeOrId fn
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


combineType : Type (Result err a) -> Result err (Type a)
combineType type_ =
    let
        f =
            combineTypeOrId
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


combineTypeOrId : TypeOrId (Result err a) -> Result err (TypeOrId a)
combineTypeOrId typeOrId =
    case typeOrId of
        Id id ->
            Ok <| Id id

        Type type_ ->
            combineType type_
                |> Result.map Type
