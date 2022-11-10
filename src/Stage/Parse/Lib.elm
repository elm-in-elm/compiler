module Stage.Parse.Lib exposing
    ( Parser, run
    , succeed, fail
    , keep, skip
    , map, andMap, map2
    , oneOf, end, lazy
    , andThen
    , optional, loop
    , many, many1, manyWithSeparator, many1WithSeparator, sequence, sequence1
    , token, tokenString, tokenInt, tokenFloat, tokenChar
    , getPosition
    , Step(..)
    )

{-|

@docs Parser, run
@docs succeed, fail
@docs keep, skip
@docs map, andMap, map2
@docs oneOf, end, lazy
@docs andThen
@docs optional, loop
@docs many, many1, manyWithSeparator, many1WithSeparator, sequence, sequence1
@docs token, tokenString, tokenInt, tokenFloat, tokenChar
@docs getPosition
@docs Step

-}

import Elm.Compiler.Error
    exposing
        ( Error(..)
        , LocatedParseError
        , LocatedParseErrorType(..)
        , ParseError(..)
        )
import Elm.Data.Token as Token exposing (Token)


type Parser a
    = Parser (List Token -> Result Error ( a, List Token ))


run : Parser a -> List Token -> Result Error a
run (Parser parse) tokens =
    parse tokens
        |> Result.map Tuple.first


{-| Reports the last parser's error if needed.

Automatically backtracks!

-}
oneOf : List (Parser a) -> Parser a
oneOf parsers =
    Parser <| oneOfHelp parsers


oneOfHelp : List (Parser a) -> List Token -> Result Error ( a, List Token )
oneOfHelp parsers tokens =
    case parsers of
        [] ->
            failInner EmptyOneOf tokens

        (Parser parse) :: restOfParsers ->
            case parse tokens of
                Err err ->
                    if List.isEmpty restOfParsers then
                        Err err

                    else
                        oneOfHelp restOfParsers tokens

                Ok ( val, restOfTokens ) ->
                    Ok ( val, restOfTokens )


succeed : a -> Parser a
succeed value =
    Parser <| \tokens -> Ok ( value, tokens )


fail : LocatedParseErrorType -> Parser a
fail errorType =
    Parser <| failInner errorType


failInner : LocatedParseErrorType -> List Token -> Result Error ( a, List Token )
failInner errorType tokens =
    let
        ( line, column ) =
            case tokens of
                t :: _ ->
                    ( t.line, t.column )

                [] ->
                    ( 1, 1 )
    in
    Err
        (ParseError
            (LocatedError
                { line = line
                , column = column
                , type_ = errorType
                }
            )
        )


keep : Parser a -> Parser (a -> b) -> Parser b
keep =
    andMap


skip : Parser a -> Parser b -> Parser b
skip next original =
    map2 (\o _ -> o) original next


andThen : (a -> Parser b) -> Parser a -> Parser b
andThen fn (Parser parse) =
    Parser <|
        \tokens ->
            case parse tokens of
                Err err ->
                    Err err

                Ok ( andThe, rest ) ->
                    let
                        (Parser nextParse) =
                            fn andThe
                    in
                    nextParse rest


map : (a -> b) -> Parser a -> Parser b
map fn (Parser parse) =
    Parser <|
        \tokens ->
            case parse tokens of
                Err err ->
                    Err err

                Ok ( andThe, rest ) ->
                    Ok ( fn andThe, rest )


andMap : Parser a -> Parser (a -> b) -> Parser b
andMap parserA parserFn =
    parserFn
        |> andThen (\fn -> map fn parserA)


map2 : (a -> b -> c) -> Parser a -> Parser b -> Parser c
map2 fn parserA parserB =
    succeed fn
        |> andMap parserA
        |> andMap parserB


optional : Parser a -> Parser (Maybe a)
optional p =
    oneOf
        [ p |> map Just
        , succeed Nothing
        ]


token : Token.Type -> Parser Token
token wantedToken =
    Parser <|
        \tokens ->
            case tokens of
                [] ->
                    failInner (ExpectedToken wantedToken) tokens

                t :: ts ->
                    if t.type_ == wantedToken then
                        Ok ( t, ts )

                    else
                        failInner (ExpectedToken wantedToken) tokens


token_ : (Token.Type -> Maybe a) -> (Token.T -> LocatedParseErrorType) -> Token.T -> Parser a
token_ toVal toError wantedToken =
    Parser <|
        \tokens ->
            case tokens of
                [] ->
                    failInner (ExpectedTokenT wantedToken) tokens

                t :: ts ->
                    if Token.flatten t.type_ == wantedToken then
                        case toVal t.type_ of
                            Nothing ->
                                failInner (toError wantedToken) tokens

                            Just val ->
                                Ok ( val, ts )

                    else
                        failInner (ExpectedTokenT wantedToken) tokens


tokenString : Token.T -> Parser String
tokenString wantedToken =
    token_ Token.getString TokenDidNotContainString wantedToken


tokenInt : Token.T -> Parser Int
tokenInt wantedToken =
    token_ Token.getInt TokenDidNotContainInt wantedToken


tokenFloat : Token.T -> Parser Float
tokenFloat wantedToken =
    token_ Token.getFloat TokenDidNotContainFloat wantedToken


tokenChar : Token.T -> Parser Char
tokenChar wantedToken =
    token_ Token.getChar TokenDidNotContainChar wantedToken


type Step state a
    = Loop state
    | Done a


loop : (state -> Parser (Step state a)) -> state -> Parser a
loop callback state =
    Parser <| loopHelp callback state


loopHelp : (state -> Parser (Step state a)) -> state -> List Token -> Result Error ( a, List Token )
loopHelp callback state tokens =
    let
        (Parser parse) =
            callback state
    in
    case parse tokens of
        Err err ->
            Err err

        Ok ( Loop newState, newTokens ) ->
            loopHelp callback newState newTokens

        Ok ( Done val, newTokens ) ->
            Ok ( val, newTokens )


many : Parser a -> Parser (List a)
many innerParser =
    let
        manyHelp : List a -> Parser (Step (List a) (List a))
        manyHelp state =
            oneOf
                [ innerParser
                    |> map (\inner -> Loop (inner :: state))
                , succeed (Done (List.reverse state))
                ]
    in
    loop manyHelp []


many1 : Parser a -> Parser ( a, List a )
many1 innerParser =
    many innerParser
        |> andThen
            (\list ->
                case list of
                    [] ->
                        fail ExpectedNonemptyList

                    x :: xs ->
                        succeed ( x, xs )
            )


manyWithSeparator :
    { item : Parser a
    , separator : Parser sep
    }
    -> Parser (List a)
manyWithSeparator { item, separator } =
    let
        sepAndItem : Parser a
        sepAndItem =
            succeed identity
                |> skip separator
                |> keep item
    in
    oneOf
        [ -- 1+
          succeed (::)
            |> keep item
            |> keep (many sepAndItem)

        -- 0
        , succeed []
        ]


many1WithSeparator :
    { item : Parser a
    , separator : Parser sep
    }
    -> Parser ( a, List a )
many1WithSeparator { item, separator } =
    let
        sepAndItem : Parser a
        sepAndItem =
            succeed identity
                |> skip separator
                |> keep item
    in
    succeed Tuple.pair
        |> keep item
        |> keep (many sepAndItem)


end : Parser ()
end =
    Parser <|
        \tokens ->
            case tokens of
                -- TODO one of these is surely not right?
                [] ->
                    Ok ( (), tokens )

                [ t ] ->
                    if t.type_ == Token.End then
                        Ok ( (), tokens )

                    else
                        failInner (ExpectedEOF tokens) tokens

                _ ->
                    failInner (ExpectedEOF tokens) tokens


sequence :
    { start : Parser start
    , end : Parser end
    , separator : Parser separator
    , item : Parser a
    }
    -> Parser (List a)
sequence c =
    succeed identity
        |> skip c.start
        |> keep
            (manyWithSeparator
                { item = c.item
                , separator = c.separator
                }
            )
        |> skip c.end


sequence1 :
    { start : Parser start
    , end : Parser end
    , separator : Parser separator
    , item : Parser a
    }
    -> Parser ( a, List a )
sequence1 c =
    succeed identity
        |> skip c.start
        |> keep
            (many1WithSeparator
                { item = c.item
                , separator = c.separator
                }
            )
        |> skip c.end


lazy : (() -> Parser a) -> Parser a
lazy toParser =
    Parser <|
        \tokens ->
            let
                (Parser parse) =
                    toParser ()
            in
            parse tokens


getPosition : Parser ( Int, Int )
getPosition =
    Parser <|
        \tokens ->
            case tokens of
                t :: ts ->
                    Ok ( ( t.line, t.column ), tokens )

                [] ->
                    -- Shouldn't happen as we have an End token
                    Ok ( ( -1, -1 ), tokens )
