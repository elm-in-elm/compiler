module Stage.Parse.Token exposing (..)


type Keyword
    = Module
    | Type
    | Alias
    | Exposing
    | Case
    | Of
    | If
    | Then
    | Else


type TypeOrConstructor
    = TypeOrConstructor String


type ValueOrFunction
    = ValueOrFunction String


type Token
    = TokenTypeOrConstructor TypeOrConstructor
    | TokenValueOrFunction ValueOrFunction
    | TokenKeyword Keyword


keywordToString : Keyword -> String
keywordToString keyword =
    case keyword of
        Module ->
            "module"

        Type ->
            "type"

        Alias ->
            "alias"

        Exposing ->
            "exposing"

        Case ->
            "case"

        Of ->
            "of"

        If ->
            "if"

        Then ->
            "then"

        Else ->
            "else"


{-| Note: empty strings become `ValueOrFunction`s
-}
classifyToken : String -> Token
classifyToken token =
    case token of
        "module" ->
            TokenKeyword Module

        "type" ->
            TokenKeyword Type

        "alias" ->
            TokenKeyword Alias

        "exposing" ->
            TokenKeyword Exposing

        "case" ->
            TokenKeyword Case

        "of" ->
            TokenKeyword Of

        "if" ->
            TokenKeyword If

        "then" ->
            TokenKeyword Then

        "else" ->
            TokenKeyword Else

        _ ->
            case String.uncons token of
                Just ( first, _ ) ->
                    if Char.isUpper first then
                        TokenTypeOrConstructor (TypeOrConstructor token)

                    else
                        TokenValueOrFunction (ValueOrFunction token)

                Nothing ->
                    TokenValueOrFunction (ValueOrFunction token)
