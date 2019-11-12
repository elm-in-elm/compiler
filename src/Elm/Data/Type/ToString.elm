module Elm.Data.Type.ToString exposing
    ( toString
    , State, emptyState
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

@docs toString
@docs State, emptyState
@docs niceVarName

-}

import Dict exposing (Dict)
import Elm.Data.Type exposing (Type(..))


{-| State for keeping track of the type variables' chosen names.
-}
type State
    = State
        { mapping : Dict Int String
        , counter : Int
        }


{-| Initial state to start with.
-}
emptyState : State
emptyState =
    State
        { mapping = Dict.empty
        , counter = 0
        }


{-| The main function of this module. Use the state returned here
in the subsequent calls (if they're going to end up as part of the same string!)
-}
toString : State -> Type -> ( String, State )
toString state type_ =
    case type_ of
        Var int ->
            getName state int

        Function t1 t2 ->
            let
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

        UserDefinedType { module_, name } typeVariables ->
            let
                ( paramsString, state1 ) =
                    if List.isEmpty typeVariables then
                        ( "", state )

                    else
                        List.foldl
                            (\param ( strings, state2 ) ->
                                let
                                    ( string, state3 ) =
                                        toString state2 param
                                in
                                ( string :: strings, state3 )
                            )
                            ( [], state )
                            typeVariables
                            |> Tuple.mapFirst (\paramStrings -> " " ++ String.join " " paramStrings)
            in
            ( module_ ++ "." ++ name ++ paramsString
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
getName ((State { counter, mapping }) as state) varId =
    case Dict.get varId mapping of
        Just letter_ ->
            ( letter_, state )

        Nothing ->
            let
                name =
                    niceVarName counter
            in
            ( name
            , State
                { counter = counter + 1
                , mapping = Dict.insert varId name mapping
                }
            )


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


niceRecordBinding : State -> ( String, Type ) -> ( String, State )
niceRecordBinding state ( varName, type_ ) =
    let
        ( typeStr, state1 ) =
            toString
                state
                type_
    in
    ( varName ++ " : " ++ typeStr, state1 )


{-|

     maybeWrapParens (List Int) ( "List Int", state )
     --> ( "(List Int)", state )

     maybeWrapParens Int ( "Int", state )
     --> ( "Int", state )

-}
maybeWrapParens : Type -> ( String, a ) -> ( String, a )
maybeWrapParens type_ ( string, state ) =
    let
        wrapParens : String -> String
        wrapParens x =
            "(" ++ x ++ ")"
    in
    if shouldWrapParens type_ then
        ( wrapParens string, state )

    else
        ( string, state )


{-| "Is there a possibility this type would need to be surrounded by parentheses?

Eg. function types: normally no need for parentheses:

    fn : Int -> Bool

but there are usecases that need parentheses:

    task : Task (Int -> Bool) String

-}
shouldWrapParens : Type -> Bool
shouldWrapParens type_ =
    case type_ of
        Var _ ->
            False

        Function _ _ ->
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

        UserDefinedType _ params ->
            not (List.isEmpty params)

        Record _ ->
            False
