module AST.Common.Type exposing
    ( State
    , Type(..)
    , emptyState
    , getVarId
    , niceVarName
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


getVarId : Type -> Maybe Int
getVarId type_ =
    case type_ of
        Var id ->
            Just id

        _ ->
            Nothing



--------------
-- toString --
--------------


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


{-|

    maybeWrapParens (List Int) ( "List Int", state )

    -->
    ( "(List Int)", state )

    maybeWrapParens Int ( "Int", state )

    -->
    ( "Int", state )

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


shouldWrapParens : Type -> Bool
shouldWrapParens type_ =
    case type_ of
        Var _ ->
            False

        Function t1 t2 ->
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

        List param ->
            True

        Unit ->
            False
