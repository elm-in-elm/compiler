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


type UpperCase
    = UpperCase String


type LowerCase
    = LowerCase String


type Token
    = TokenUpperCase UpperCase
    | TokenLowerCase LowerCase


tokenToString t =
    case t of
        TokenUpperCase (UpperCase s) ->
            s

        TokenLowerCase (LowerCase s) ->
            s


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
