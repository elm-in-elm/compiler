module Elm.Data.Type.ToString exposing
    ( toString, toStringType
    , State, fromType, fromTypeOrId, fromTypes, fromTypesOrIds
    , niceVarName
    )

{-| Functions for printing types.

The reason this isn't just a simple `toString` function is that type variables
need to have consistent names across multiple `toString` calls (eg. in error
messages).

To accomplish this, we pass a state around as an argument.

This is how the simple case would look:

    toString emptyState (List Int)
    --> ("List Int", <state>)

And for cases where you're printing multiple types and they have to make sense
together, you pass the state returned from first call to the second call:

    let
        (typeString1, state1) =
            toString emptyState (Function (Var 0) (Var 1))

        (typeString2, _) =
            toString state1 (Function Int (Var 1))
    in
    typeString1 ++ " is not the same as " ++ typeString2
    --> "(a -> b) is not the same as (Int -> b)

The important thing here is that the `Var 1` was rendered the same in both cases.
If you didn't do that and passed `emptyState` to both cases, you'd get:

    "(a -> b) is not the same as (Int -> a)"

Which would be wrong and misleading!

@docs toString, toStringType
@docs State, fromType, fromTypeOrId, fromTypes, fromTypesOrIds
@docs niceVarName

-}

import Dict exposing (Dict)
import Elm.Data.Type as Type exposing (Type(..), TypeOrId(..))
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
        , used = Set.fromList (List.concatMap Type.varNames types)
        }


{-| Initial state to start with.
-}
fromTypesOrIds : List (TypeOrId a) -> State
fromTypesOrIds typesOrIds =
    State
        { mapping = Dict.empty
        , counter = 0
        , used = Set.fromList (List.concatMap Type.varNames_ typesOrIds)
        }


{-| The main function of this module. Use the state returned here
in the subsequent calls (if they're going to end up as part of the same string!)
-}
toString : State -> TypeOrId a -> ( String, State )
toString state type_ =
    case type_ of
        Id id ->
            getName state id

        Type type__ ->
            toStringType state type__


toStringType : State -> Type a -> ( String, State )
toStringType state type_ =
    case type_ of
        Var string ->
            ( string, state )

        Function args ->
            let
                t1 =
                    args.from

                t2 =
                    args.to

                ( t1String, state1 ) =
                    toString state t1
                        |> maybeWrapParens t1

                ( t2String, state2 ) =
                    toString state1 t2
            in
            ( t1String ++ " -> " ++ t2String
            , state2
            )

        List param ->
            let
                ( paramString, state1 ) =
                    toString state param
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
            ( "Unit", state )

        Tuple t1 t2 ->
            let
                ( t1String, state1 ) =
                    toString state t1

                ( t2String, state2 ) =
                    toString state1 t2
            in
            ( "( " ++ t1String ++ ", " ++ t2String ++ " )", state2 )

        Tuple3 t1 t2 t3 ->
            let
                ( t1String, state1 ) =
                    toString state t1

                ( t2String, state2 ) =
                    toString state1 t2

                ( t3String, state3 ) =
                    toString state2 t3
            in
            ( "( " ++ t1String ++ ", " ++ t2String ++ ", " ++ t3String ++ " )", state3 )

        UserDefinedType { name, args } ->
            let
                ( argsString, state1 ) =
                    if List.isEmpty args then
                        ( "", state )

                    else
                        List.foldl
                            (\arg ( strings, state2 ) ->
                                let
                                    ( string, state3 ) =
                                        toString state2 arg
                                in
                                ( string :: strings, state3 )
                            )
                            ( [], state )
                            args
                            |> Tuple.mapFirst (\argStrings -> " " ++ String.join " " argStrings)
            in
            ( name ++ argsString
            , state1
            )

        Record bindings ->
            let
                ( bindingsStr, state1 ) =
                    List.foldr
                        (\param ( acc, state2 ) ->
                            let
                                ( string, state3 ) =
                                    niceRecordBinding state2 param
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


niceRecordBinding : State -> ( String, TypeOrId a ) -> ( String, State )
niceRecordBinding state ( varName, typeOrId ) =
    let
        ( typeStr, state1 ) =
            toString state typeOrId
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
                Var _ ->
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
