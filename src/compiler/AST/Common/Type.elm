module AST.Common.Type exposing
    ( Type(..)
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


getVarId : Type -> Maybe Int
getVarId type_ =
    case type_ of
        Var id ->
            Just id

        _ ->
            Nothing

toString : Type -> String
toString type_ =
    getTypeVariablesIndex type_
    |> toStringHelp type_


toStringHelp : Type -> Dict Int String -> String
toStringHelp type_ dict =
    case type_ of
        Var int ->
            Dict.get int dict
            |> Maybe.withDefault ("t" ++ String.fromInt int)

        Function t1 t2 ->
            wrapParens t1 dict ++ " -> " ++ toStringHelp t2 dict

        Int ->
            "Int"

        Float ->
            "Float"

        Char ->
            "Char"

        String ->
            "String"

        Bool ->
            "Bool"

        List param ->
            "List " ++ wrapParens param dict

        Unit ->
            "()"


getTypeVariablesIndex : Type -> Dict Int String
getTypeVariablesIndex type_ =
    getTypeVariablesIndexHelp type_ (typeVariablesLetters, Dict.empty)
    |> Tuple.second


getTypeVariablesIndexHelp : Type -> (List String, Dict Int String) -> (List String, Dict Int String)
getTypeVariablesIndexHelp type_ (freeLetters, dict) =
    case type_ of
        Var int ->
            case freeLetters of
                [] ->
                    ([], dict)

                hd :: xs ->
                    if Dict.member int dict then
                        (freeLetters, dict)
                    else
                        (xs, Dict.insert int hd dict)

        Function t1 t2 ->
            getTypeVariablesIndexHelp t1 (freeLetters, dict)
            |> getTypeVariablesIndexHelp t2

        List param ->
            getTypeVariablesIndexHelp param (freeLetters, dict)

        Int ->
            (freeLetters, dict)

        Float ->
            (freeLetters, dict)

        Char ->
            (freeLetters, dict)

        String ->
            (freeLetters, dict)

        Bool ->
            (freeLetters, dict)

        Unit ->
            (freeLetters, dict)


-- Stops at letter `s`, so that outbound variables display `tN`
typeVariablesLetters =
    [ "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s" ]


wrapParens : Type -> Dict Int String -> String
wrapParens type_ dict =
    case type_ of
        Function t1 t2 ->
            "(" ++ toStringHelp type_ dict ++ ")"

        List param ->
            "(" ++ toStringHelp type_ dict ++ ")"

        _ ->
            toStringHelp type_ dict
