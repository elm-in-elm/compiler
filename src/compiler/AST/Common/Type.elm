module AST.Common.Type exposing
    ( Type(..)
    , emptyVars
    , getVarId
    , toString
    , toStringWithVars
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


type alias TypeVariableIndex =
    ( List String, Dict Int String )


getVarId : Type -> Maybe Int
getVarId type_ =
    case type_ of
        Var id ->
            Just id

        _ ->
            Nothing


toString : Type -> String
toString type_ =
    toStringWithVars type_ emptyVars
        |> Tuple.first


emptyVars : TypeVariableIndex
emptyVars =
    ( typeVariablesLetters, Dict.empty )


toStringWithVars : Type -> TypeVariableIndex -> ( String, TypeVariableIndex )
toStringWithVars type_ typeVariablesIndex =
    case type_ of
        Var int ->
            getVariable int typeVariablesIndex

        Function t1 t2 ->
            let
                ( type1, typeVariablesIndex1 ) =
                    wrapParens t1 typeVariablesIndex

                ( type2, typeVariablesIndex2 ) =
                    toStringWithVars t2 typeVariablesIndex1
            in
            ( type1 ++ " -> " ++ type2
            , typeVariablesIndex2
            )

        List param ->
            wrapParens param typeVariablesIndex
                |> Tuple.mapFirst ((++) "List ")

        Int ->
            ( "Int", typeVariablesIndex )

        Float ->
            ( "Float", typeVariablesIndex )

        Char ->
            ( "Char", typeVariablesIndex )

        String ->
            ( "String", typeVariablesIndex )

        Bool ->
            ( "Bool", typeVariablesIndex )

        Unit ->
            ( "Unit", typeVariablesIndex )


getVariable : Int -> TypeVariableIndex -> ( String, TypeVariableIndex )
getVariable int (( letters, dict ) as typeVariablesIndex) =
    case Dict.get int dict of
        Just letter ->
            ( letter
            , typeVariablesIndex
            )

        Nothing ->
            case letters of
                letter :: freeLetters ->
                    ( letter
                    , ( freeLetters
                      , Dict.insert int letter dict
                      )
                    )

                [] ->
                    ( "t" ++ String.fromInt int
                    , typeVariablesIndex
                    )


typeVariablesLetters : List String
typeVariablesLetters =
    -- Stops at letter `s`, so that outbound variables display `tN`
    [ "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s" ]


wrapParens : Type -> TypeVariableIndex -> ( String, TypeVariableIndex )
wrapParens type_ typeVariablesIndex =
    toStringWithVars type_ typeVariablesIndex
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
