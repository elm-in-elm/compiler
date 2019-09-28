module Elm.Data.Type.ToString exposing
    ( State
    , emptyState
    , niceVarName
    , toString
    )

import Dict exposing (Dict)
import Elm.Data.Type exposing (Type)


type State
    = State
        { mapping : Dict Int String
        , counter : Int
        }


emptyState : State
emptyState =
    State
        { mapping = Dict.empty
        , counter = 0
        }


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

        UserDefinedType { module_, name } typeParameters ->
            let
                ( paramsString, state1 ) =
                    if List.isEmpty typeParameters then
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
                            typeParameters
                            |> Tuple.mapFirst (\paramStrings -> " " ++ String.join " " paramStrings)
            in
            ( module_ ++ "." ++ name ++ paramsString
            , state1
            )


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


{-| We're generating the nice variable names using this sequence:

    a, b, ..., z, | a1, b1, ..., z1, | a2, b2, ..., z2, | ...
    0  1       25 | 26  27       51  | 52  53       77  |

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
