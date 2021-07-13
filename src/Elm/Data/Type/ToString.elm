module Elm.Data.Type.ToString exposing
    ( toString, toStringType
    , toStringPossiblyQualified, toStringTypePossiblyQualified
    , State, fromType, fromTypeOrId, fromTypes, fromTypesOrIds, addType, addTypeOrId
    , niceVarName
    )

{-| Functions for printing types.

The reason this isn't just a simple `toString` function is that type variables
need to have consistent names across multiple `toString` calls (eg. in error
messages).

To accomplish this, we pass a state around as an argument.

State can be created using functions like `fromType`. (We're explicitly not
including an `emptyState` to force you to use this API properly and not forget
to add a type.)

This is how the simple case would look:

    toString (fromType (List Int)) (List Int)
    --> ("List Int", <state>)

And for cases where you're printing multiple types and they have to make sense
together, you pass the state returned from first call to the second call:

    let
        type1 =
            Function (Var 0) (Var 1)

        type2 =
            Function Int (Var 1)

        state =
            fromTypes [ type1, type2 ]

        (typeString1, state1) =
            toString state type1

        (typeString2, _) =
            toString state1 type2
    in
    typeString1 ++ " is not the same as " ++ typeString2
    --> "(a -> b) is not the same as (Int -> b)

The important thing here is that the `Var 1` was rendered the same in both cases.
If you didn't do that and passed `emptyState` to both cases, you'd get:

    "(a -> b) is not the same as (Int -> a)"

Which would be wrong and misleading!

@docs toString, toStringType
@docs toStringPossiblyQualified, toStringTypePossiblyQualified
@docs State, fromType, fromTypeOrId, fromTypes, fromTypesOrIds, addType, addTypeOrId
@docs niceVarName

-}

import Dict exposing (Dict)
import Elm.Data.Qualifiedness exposing (PossiblyQualified(..), Qualified(..))
import Elm.Data.Type as Type exposing (Type(..), TypeOrId(..))
import OurExtras.List as List
import Set exposing (Set)


{-| State for keeping track of the type variables' chosen names.
-}
type State
    = State
        { mapping : Dict Int String
        , counter : Int
        , used : Set String
        }


{-| Initial state to start with.
-}
fromType : Type a -> State
fromType type_ =
    State
        { mapping = Dict.empty
        , counter = 0
        , used = Set.fromList (Type.varNames type_)
        }


{-| Initial state to start with.
-}
fromTypeOrId : TypeOrId a -> State
fromTypeOrId typeOrId =
    State
        { mapping = Dict.empty
        , counter = 0
        , used = Set.fromList (Type.varNames_ typeOrId)
        }


{-| Initial state to start with.
-}
fromTypes : List (Type a) -> State
fromTypes types =
    State
        { mapping = Dict.empty
        , counter = 0
        , used = Set.fromList (List.fastConcatMap Type.varNames types)
        }


{-| Initial state to start with.
-}
fromTypesOrIds : List (TypeOrId a) -> State
fromTypesOrIds typesOrIds =
    State
        { mapping = Dict.empty
        , counter = 0
        , used = Set.fromList (List.fastConcatMap Type.varNames_ typesOrIds)
        }


addType : Type a -> State -> State
addType type_ (State state) =
    State
        { state
            | used =
                Set.union
                    (Set.fromList (Type.varNames type_))
                    state.used
        }


addTypeOrId : TypeOrId a -> State -> State
addTypeOrId typeOrId (State state) =
    State
        { state
            | used =
                Set.union
                    (Set.fromList (Type.varNames_ typeOrId))
                    state.used
        }


qualifiedToString : Qualified -> String
qualifiedToString (Qualified moduleName) =
    moduleName


possiblyQualifiedToString : PossiblyQualified -> String
possiblyQualifiedToString (PossiblyQualified maybeModuleName) =
    maybeModuleName
        |> Maybe.withDefault ""


{-| The main function of this module. Use the state returned here
in the subsequent calls (if they're going to end up as part of the same string!)
-}
toString : State -> TypeOrId Qualified -> ( String, State )
toString state type_ =
    toString_
        qualifiedToString
        state
        type_


toStringType : State -> Type Qualified -> ( String, State )
toStringType state type_ =
    toStringType_
        qualifiedToString
        state
        type_


toStringPossiblyQualified : State -> TypeOrId PossiblyQualified -> ( String, State )
toStringPossiblyQualified state type_ =
    toString_
        possiblyQualifiedToString
        state
        type_


toStringTypePossiblyQualified : State -> Type PossiblyQualified -> ( String, State )
toStringTypePossiblyQualified state type_ =
    toStringType_
        possiblyQualifiedToString
        state
        type_


toString_ : (qualifiedness -> String) -> State -> TypeOrId qualifiedness -> ( String, State )
toString_ qualifiednessToString state type_ =
    case type_ of
        Id id ->
            getName state id

        Type type__ ->
            toStringType_ qualifiednessToString state type__


toStringType_ : (qualifiedness -> String) -> State -> Type qualifiedness -> ( String, State )
toStringType_ qualifiednessToString state type_ =
    let
        recur state_ type__ =
            toString_ qualifiednessToString state_ type__
    in
    case type_ of
        TypeVar string ->
            ( string, state )

        Function args ->
            let
                t1 =
                    args.from

                t2 =
                    args.to

                ( t1String, state1 ) =
                    recur state t1
                        |> maybeWrapParens t1

                ( t2String, state2 ) =
                    recur state1 t2
            in
            ( t1String ++ " -> " ++ t2String
            , state2
            )

        List param ->
            let
                ( paramString, state1 ) =
                    recur state param
                        |> maybeWrapParens param
            in
            ( "List " ++ paramString
            , state1
            )

        Int ->
            ( "Int", state )

        Float ->
            ( "Float", state )

        Char ->
            ( "Char", state )

        String ->
            ( "String", state )

        Bool ->
            ( "Bool", state )

        Unit ->
            ( "()", state )

        Tuple t1 t2 ->
            let
                ( t1String, state1 ) =
                    recur state t1

                ( t2String, state2 ) =
                    recur state1 t2
            in
            ( "( " ++ t1String ++ ", " ++ t2String ++ " )", state2 )

        Tuple3 t1 t2 t3 ->
            let
                ( t1String, state1 ) =
                    recur state t1

                ( t2String, state2 ) =
                    recur state1 t2

                ( t3String, state3 ) =
                    recur state2 t3
            in
            ( "( " ++ t1String ++ ", " ++ t2String ++ ", " ++ t3String ++ " )", state3 )

        UserDefinedType { qualifiedness, name, args } ->
            let
                ( spaceAndArgs, state1 ) =
                    if List.isEmpty args then
                        ( "", state )

                    else
                        List.foldl
                            (\arg ( strings, state2 ) ->
                                let
                                    ( string, state3 ) =
                                        recur state2 arg
                                in
                                ( string :: strings, state3 )
                            )
                            ( [], state )
                            args
                            |> Tuple.mapFirst (\argStrings -> " " ++ String.join " " argStrings)

                moduleName =
                    qualifiednessToString qualifiedness

                moduleNameAndDot =
                    if String.isEmpty moduleName then
                        moduleName

                    else
                        moduleName ++ "."
            in
            ( moduleNameAndDot ++ name ++ spaceAndArgs
            , state1
            )

        Record bindings ->
            let
                ( bindingsStr, state1 ) =
                    List.foldr
                        (\param ( acc, state2 ) ->
                            let
                                ( string, state3 ) =
                                    niceRecordBinding qualifiednessToString state2 param
                            in
                            ( string :: acc, state3 )
                        )
                        ( [], state )
                        (Dict.toList bindings)
                        |> Tuple.mapFirst (String.join ", ")
            in
            if bindingsStr == "" then
                ( "{}", state1 )

            else
                ( "{ " ++ bindingsStr ++ " }", state1 )


getName : State -> Int -> ( String, State )
getName ((State { counter, mapping, used }) as state) varId =
    case Dict.get varId mapping of
        Just letter_ ->
            ( letter_, state )

        Nothing ->
            let
                name =
                    niceVarName counter
                        |> findUnused used
            in
            ( name
            , State
                { counter = counter + 1
                , mapping = Dict.insert varId name mapping
                , used = Set.insert name used
                }
            )


findUnused : Set String -> String -> String
findUnused used newName =
    if Set.member newName used then
        findUnusedHelp 1 used newName

    else
        newName


findUnusedHelp : Int -> Set String -> String -> String
findUnusedHelp n used newName =
    let
        newName_ =
            newName ++ String.fromInt n
    in
    if Set.member newName_ used then
        findUnusedHelp (n + 1) used newName_

    else
        newName_


{-| Function to get from a number to a nice type variable name.
It follows this sequence:

    a, b, ..., z, | a1, b1, ..., z1, | a2, b2, ..., z2, | ...
    0  1       25 | 26  27       51  | 52  53       77  |

Note the number passed into this function isn't the number inside `Var`
but the counter inside [`State`](#State).

-}
niceVarName : Int -> String
niceVarName id =
    if id < 26 then
        letter id

    else
        let
            wraparounds =
                id // 26

            remainder =
                id |> modBy 26
        in
        letter remainder ++ String.fromInt wraparounds


letter : Int -> String
letter int =
    (int + 97 {- `a` -})
        |> Char.fromCode
        |> String.fromChar


niceRecordBinding : (qualifiedness -> String) -> State -> ( String, TypeOrId qualifiedness ) -> ( String, State )
niceRecordBinding qualifiednessToString state ( varName, typeOrId ) =
    let
        ( typeStr, state1 ) =
            toString_ qualifiednessToString state typeOrId
    in
    ( varName ++ " : " ++ typeStr, state1 )


{-|

     maybeWrapParens (List Int) ( "List Int", state )
     --> ( "(List Int)", state )

     maybeWrapParens Int ( "Int", state )
     --> ( "Int", state )

-}
maybeWrapParens : TypeOrId b -> ( String, a ) -> ( String, a )
maybeWrapParens typeOrId ( string, state ) =
    let
        wrapParens : String -> String
        wrapParens x =
            "(" ++ x ++ ")"
    in
    if shouldWrapParens typeOrId then
        ( wrapParens string, state )

    else
        ( string, state )


{-| "Is there a possibility this type would need to be surrounded by parentheses?

Eg. function types: normally no need for parentheses:

    fn : Int -> Bool

but there are usecases that need parentheses:

    task : Task (Int -> Bool) String

-}
shouldWrapParens : TypeOrId a -> Bool
shouldWrapParens typeOrId =
    case typeOrId of
        Id _ ->
            False

        Type type_ ->
            case type_ of
                TypeVar _ ->
                    False

                Function _ ->
                    True

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

                List _ ->
                    True

                Unit ->
                    False

                Tuple _ _ ->
                    False

                Tuple3 _ _ _ ->
                    False

                UserDefinedType { args } ->
                    not (List.isEmpty args)

                Record _ ->
                    False
