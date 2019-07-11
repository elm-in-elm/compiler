module AST.Common.Type exposing
    ( State
    , Type(..)
    , dump
    , empty
    , getVarId
    , toString
    )

{-| In its own module because both Error.TypeError and AST.Typed need to see it
-}

import Dict exposing (Dict)


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


type State
    = State
        { freeLetters : List String
        , mapping : Dict Int String
        }


getVarId : Type -> Maybe Int
getVarId type_ =
    case type_ of
        Var id ->
            Just id

        _ ->
            Nothing


empty : State
empty =
    State
        { freeLetters = varsLetters
        , mapping = Dict.empty
        }


toString : Type -> State -> ( String, State )
toString type_ state =
    case type_ of
        Var int ->
            getVariable int state

        Function t1 t2 ->
            let
                ( type1, state1 ) =
                    wrapParens t1 state

                ( type2, state2 ) =
                    toString t2 state1
            in
            ( type1 ++ " -> " ++ type2
            , state2
            )

        List param ->
            wrapParens param state
                |> Tuple.mapFirst ((++) "List ")

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


dump : Type -> String
dump type_ =
    toString type_ empty
        |> Tuple.first


getVariable : Int -> State -> ( String, State )
getVariable int ((State { freeLetters, mapping }) as state) =
    case Dict.get int mapping of
        Just letter ->
            ( letter
            , state
            )

        Nothing ->
            case freeLetters of
                letter :: letters ->
                    ( letter
                    , State
                        { freeLetters = letters
                        , mapping = Dict.insert int letter mapping
                        }
                    )

                [] ->
                    ( "t" ++ String.fromInt int
                    , state
                    )


varsLetters : List String
varsLetters =
    -- Stops at letter `s`, so that outbound variables display `tN`
    [ "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s" ]


wrapParens : Type -> State -> ( String, State )
wrapParens type_ state =
    toString type_ state
        |> Tuple.mapFirst
            (\t ->
                case type_ of
                    Function t1 t2 ->
                        "(" ++ t ++ ")"

                    List param ->
                        "(" ++ t ++ ")"

                    _ ->
                        t
            )
