module Stage.Tokenize exposing (tokenize)

import Dict exposing (Dict)
import Elm.Compiler.Error exposing (Error(..), TokenizeError(..))
import Elm.Data.FileContents exposing (FileContents)
import Elm.Data.FilePath exposing (FilePath)
import Elm.Data.Token as Token exposing (Token, Type(..))
import Hex
import List.Extra as List
import List.NonEmpty exposing (NonEmpty)
import Set exposing (Set)
import Stage.Tokenize.Lib as Tokenize


tokenize : FileContents -> Result Error (List Token)
tokenize fileContents =
    tokenizeString fileContents
        |> Result.mapError TokenizeError
        |> Result.map (.tokens >> List.reverse)


type Possibility
    = Exact String (State -> ( Maybe TokenizeError, State ))
    | IsNegatedNumber


type alias State =
    { program : List Char
    , line : Int
    , column : Int
    , startLine : Int
    , startColumn : Int
    , tokens : List Token
    }


initState : String -> State
initState string =
    { program = String.toList string
    , line = 1
    , column = 1
    , startLine = 1
    , startColumn = 1
    , tokens = []
    }


tokenizeString : String -> Result TokenizeError State
tokenizeString string =
    let
        go : State -> Result TokenizeError State
        go state =
            if List.isEmpty state.program then
                Ok
                    { state
                        | tokens =
                            { line = state.line
                            , column = state.column
                            , type_ = End
                            }
                                :: state.tokens
                    }

            else
                let
                    ( err, newState ) =
                        parseNextToken state
                in
                case err of
                    Just err_ ->
                        Err err_

                    Nothing ->
                        go newState
    in
    go (initState string)


parseNextToken : State -> ( Maybe TokenizeError, State )
parseNextToken state_ =
    let
        state =
            { state_
                | startLine = state_.line
                , startColumn = state_.column
            }
    in
    case state.program of
        [] ->
            ( Nothing, state )

        char :: rest ->
            (case char of
                ' ' ->
                    \s -> ( Nothing, Tokenize.skipWhile (\c -> c == ' ') s )

                '\n' ->
                    \s -> ( Nothing, Tokenize.skipWhile (\c -> c == '\n') s )

                '\t' ->
                    \s ->
                        ( Just
                            (FoundTabulator
                                { line = s.line
                                , column = s.column
                                }
                            )
                        , s
                        )

                '(' ->
                    oneOf
                        [ Exact "(..)" (found All) ]
                        { else_ = Tokenize.skip 1 >> found LeftParen }

                ')' ->
                    Tokenize.skip 1 >> found RightParen

                '[' ->
                    oneOf
                        [ Exact "[glsl|" matchGlslShader ]
                        { else_ = Tokenize.skip 1 >> found LeftSquareBracket }

                ']' ->
                    Tokenize.skip 1 >> found RightSquareBracket

                ',' ->
                    Tokenize.skip 1 >> found Comma

                '-' ->
                    oneOf
                        [ Exact "--" matchLineComment
                        , Exact "->" (found RightArrow)
                        , IsNegatedNumber
                        ]
                        { else_ = Tokenize.skip 1 >> found Minus }

                '\\' ->
                    Tokenize.skip 1 >> found Backslash

                '_' ->
                    Tokenize.skip 1 >> found Underscore

                '{' ->
                    oneOf
                        [ Exact "{-|" matchMultilineComment
                        , Exact "{-" matchMultilineComment
                        ]
                        { else_ = Tokenize.skip 1 >> found LeftCurlyBracket }

                '}' ->
                    Tokenize.skip 1 >> found RightCurlyBracket

                '\'' ->
                    Tokenize.skip 1 >> matchChar

                '"' ->
                    oneOf
                        [ Exact "\"\"\"" matchMultilineString
                        ]
                        { else_ = Tokenize.skip 1 >> matchString }

                _ ->
                    if Char.isLower char then
                        Tokenize.skip 1 >> matchLowerName char

                    else if Char.isUpper char then
                        Tokenize.skip 1 >> matchUpperName char

                    else if Char.isDigit char then
                        \s ->
                            case matchNumber { shouldNegate = False } s of
                                Nothing ->
                                    -- Shouldn't happen
                                    ( Nothing, s )

                                Just result ->
                                    result

                    else if isOperatorChar char then
                        matchOperator

                    else
                        \s ->
                            ( Just
                                (UnexpectedChar
                                    { char = char
                                    , line = s.line
                                    , column = s.column
                                    }
                                )
                            , s
                            )
            )
                state


oneOf :
    List Possibility
    -> { else_ : State -> ( Maybe TokenizeError, State ) }
    -> State
    -> ( Maybe TokenizeError, State )
oneOf possibilities ({ else_ } as else__) state =
    case possibilities of
        [] ->
            else_ state

        possibility :: restOfPossibilities ->
            let
                nope () =
                    oneOf restOfPossibilities else__ state
            in
            case possibility of
                Exact string then_ ->
                    if List.isPrefixOf (String.toList string) state.program then
                        state
                            |> Tokenize.skip (String.length string)
                            |> then_

                    else
                        nope ()

                IsNegatedNumber ->
                    -- we're guaranteed we're sitting on `-`
                    case matchNumber { shouldNegate = True } (Tokenize.skip 1 state) of
                        Nothing ->
                            nope ()

                        Just result ->
                            result


found : Token.Type -> State -> ( Maybe TokenizeError, State )
found type_ state =
    let
        token : Token
        token =
            { line = state.startLine
            , column = state.startColumn
            , type_ = type_
            }
    in
    ( Nothing, { state | tokens = token :: state.tokens } )


matchChar : State -> ( Maybe TokenizeError, State )
matchChar state =
    let
        endNotFound () =
            ( Just
                (EndOfCharNotFound
                    { startLine = state.line
                    , startColumn = state.column
                    }
                )
            , state
            )

        foundTooLong () =
            let
                ( foundContents, newState ) =
                    Tokenize.matchWhile (\c -> c /= '\'') state
            in
            ( Just
                (CharTooLong
                    { charContents = foundContents
                    , startLine = state.line
                    , startColumn = state.column
                    }
                )
            , newState
            )
    in
    case state.program of
        [] ->
            endNotFound ()

        '\'' :: _ ->
            ( Just
                (CharWasEmpty
                    { startLine = state.line
                    , startColumn = state.column
                    }
                )
            , state
            )

        '\\' :: cs ->
            let
                ( escapedChar, stateAfterEscaped ) =
                    matchEscapedChar (Tokenize.skip 1 state)
            in
            case escapedChar of
                Err err ->
                    ( Just err, stateAfterEscaped )

                Ok escapedChar_ ->
                    case stateAfterEscaped.program of
                        [] ->
                            endNotFound ()

                        '\'' :: _ ->
                            found (Char escapedChar_) (Tokenize.skip 1 stateAfterEscaped)

                        _ ->
                            foundTooLong ()

        c :: '\'' :: _ ->
            found (Char c) (Tokenize.skip 2 state)

        _ ->
            foundTooLong ()


matchEscapedChar : State -> ( Result TokenizeError Char, State )
matchEscapedChar state =
    case state.program of
        [] ->
            ( Err <|
                EndOfEscapeNotFound
                    { startLine = state.line
                    , startColumn = state.column
                    }
            , state
            )

        'u' :: '{' :: cs ->
            matchUnicodeEscapedChar (Tokenize.skip 2 state)

        c :: cs ->
            case Dict.get c allowedEscapes of
                Just escaped ->
                    ( Ok escaped, Tokenize.skip 1 state )

                Nothing ->
                    ( Err <|
                        UnexpectedEscapeChar
                            { char = c
                            , line = state.line
                            , column = state.column
                            }
                    , state
                    )


matchUnicodeEscapedChar : State -> ( Result TokenizeError Char, State )
matchUnicodeEscapedChar state =
    let
        ( hexString, stateAfterHex ) =
            Tokenize.matchWhile (\c -> c /= '}') state
    in
    if String.length hexString /= 4 then
        ( Err <|
            WrongUnicodeEscapeLength
                { hexString = hexString
                , startLine = state.line
                , startColumn = state.column
                }
        , stateAfterHex
        )

    else
        case Hex.fromString (String.toLower hexString) of
            Err _ ->
                ( Err <|
                    WrongUnicodeEscape
                        { hexString = hexString
                        , startLine = state.line
                        , startColumn = state.column
                        }
                , stateAfterHex
                )

            Ok num ->
                case stateAfterHex.program of
                    '}' :: _ ->
                        ( Ok <| Char.fromCode num
                        , Tokenize.skip 1 stateAfterHex
                        )

                    _ ->
                        ( Err <|
                            EndOfUnicodeEscapeNotFound
                                { startLine = state.line
                                , startColumn = state.column
                                }
                        , stateAfterHex
                        )


allowedEscapes : Dict Char Char
allowedEscapes =
    Dict.fromList
        [ ( 'n', '\n' )
        , ( 'r', '\u{000D}' )
        , ( 't', '\t' )
        , ( '"', '"' )
        , ( '\'', '\'' )
        , ( '\\', '\\' )
        ]


matchString : State -> ( Maybe TokenizeError, State )
matchString state =
    let
        go : List String -> State -> ( Maybe TokenizeError, State )
        go accString accState =
            let
                ( maybeMatch, stateAfterChunk ) =
                    Tokenize.matchUntilOneOf [ '"', '\\' ] accState
            in
            case maybeMatch of
                Nothing ->
                    ( Just
                        (EndOfStringNotFound
                            { startLine = state.line
                            , startColumn = state.column
                            }
                        )
                    , stateAfterChunk
                    )

                Just ( matchedChar, matchedString ) ->
                    case matchedChar of
                        '"' ->
                            let
                                newAccString =
                                    if String.isEmpty matchedString then
                                        accString

                                    else
                                        matchedString :: accString
                            in
                            found
                                (String
                                    (newAccString
                                        |> List.reverse
                                        |> String.concat
                                    )
                                )
                                stateAfterChunk

                        '\\' ->
                            let
                                ( escapedCharResult, stateAfterChar ) =
                                    matchEscapedChar stateAfterChunk
                            in
                            case escapedCharResult of
                                Err err ->
                                    ( Just err, stateAfterChar )

                                Ok escapedChar ->
                                    let
                                        newAccString =
                                            if String.isEmpty matchedString then
                                                String.fromChar escapedChar :: accString

                                            else
                                                String.fromChar escapedChar :: matchedString :: accString
                                    in
                                    go newAccString stateAfterChar

                        _ ->
                            -- Shouldn't be possible
                            ( Just (TokenizeCompilerBug "matchUntilOneOf returned a char that wasn't in its input list")
                            , stateAfterChunk
                            )
    in
    go [] state


matchMultilineString : State -> ( Maybe TokenizeError, State )
matchMultilineString state =
    let
        go : List String -> State -> ( Maybe TokenizeError, State )
        go accString accState =
            let
                ( maybeMatch, stateAfterChunk ) =
                    Tokenize.matchUntilOneOf [ '"', '\\' ] accState
            in
            case maybeMatch of
                Nothing ->
                    ( Just
                        (EndOfStringNotFound
                            { startLine = state.line
                            , startColumn = state.column
                            }
                        )
                    , stateAfterChunk
                    )

                Just ( matchedChar, matchedString ) ->
                    case matchedChar of
                        '"' ->
                            -- might be the end of the string (if """ is found)
                            -- or it might be just a " character
                            case stateAfterChunk.program of
                                '"' :: '"' :: _ ->
                                    -- end of the string!
                                    let
                                        newAccString =
                                            if String.isEmpty matchedString then
                                                accString

                                            else
                                                matchedString :: accString
                                    in
                                    found
                                        (String
                                            (newAccString
                                                |> List.reverse
                                                |> String.concat
                                            )
                                        )
                                        (Tokenize.skip 2 stateAfterChunk)

                                _ ->
                                    -- just a " character!
                                    let
                                        newAccString =
                                            if String.isEmpty matchedString then
                                                String.fromChar matchedChar :: accString

                                            else
                                                String.fromChar matchedChar :: matchedString :: accString
                                    in
                                    go newAccString stateAfterChunk

                        '\\' ->
                            let
                                ( escapedCharResult, stateAfterChar ) =
                                    matchEscapedChar stateAfterChunk
                            in
                            case escapedCharResult of
                                Err err ->
                                    ( Just err, stateAfterChar )

                                Ok escapedChar ->
                                    let
                                        newAccString =
                                            if String.isEmpty matchedString then
                                                String.fromChar escapedChar :: accString

                                            else
                                                String.fromChar escapedChar :: matchedString :: accString
                                    in
                                    go newAccString stateAfterChar

                        _ ->
                            -- Shouldn't be possible
                            ( Just (TokenizeCompilerBug "matchUntilOneOf returned a char that wasn't in its input list")
                            , stateAfterChunk
                            )
    in
    go [] state


matchGlslShader : State -> ( Maybe TokenizeError, State )
matchGlslShader state =
    -- [glsl| was already parsed
    let
        go : List String -> State -> ( Maybe TokenizeError, State )
        go accMatched accState =
            let
                ( matchedShader, stateAfterShader ) =
                    Tokenize.matchWhile (\c -> c /= '|') accState
            in
            if Tokenize.next 2 stateAfterShader == "|]" then
                let
                    newAccMatched =
                        if String.isEmpty matchedShader then
                            accMatched

                        else
                            matchedShader :: accMatched
                in
                found
                    (GlslShader
                        (newAccMatched
                            |> List.reverse
                            |> String.concat
                        )
                    )
                    (Tokenize.skip 2 stateAfterShader)

            else
                let
                    newAccMatched =
                        if String.isEmpty matchedShader then
                            "|" :: accMatched

                        else
                            "|" :: matchedShader :: accMatched
                in
                go newAccMatched (Tokenize.skip 1 stateAfterShader)
    in
    go [] state


matchNumber : { shouldNegate : Bool } -> State -> Maybe ( Maybe TokenizeError, State )
matchNumber c state =
    {- This one is a bit special (returns things wrapped in an extra Maybe
       because it's called after an `-` while it's not yet clear whether a number
       follows)
    -}
    if Tokenize.next 2 state == "0x" then
        matchHexInt c (Tokenize.skip 2 state)
            |> Just

    else
        let
            ( finalNumber, stateAfterInt ) =
                Tokenize.matchWhile Char.isDigit state
        in
        case String.toInt finalNumber of
            Nothing ->
                {- Can happen eg. with `1 - 2` right after the "-": the " " is not
                   a number so our matched numeric string is empty.
                -}
                Nothing

            Just int ->
                Just <|
                    case stateAfterInt.program of
                        '.' :: _ ->
                            matchDotFloat int c (Tokenize.skip 1 stateAfterInt)

                        'E' :: _ ->
                            matchScientificFloat (toFloat int) c (Tokenize.skip 1 stateAfterInt)

                        'e' :: _ ->
                            matchScientificFloat (toFloat int) c (Tokenize.skip 1 stateAfterInt)

                        _ ->
                            found
                                (Int (possiblyNegate c int))
                                stateAfterInt


matchHexInt : { shouldNegate : Bool } -> State -> ( Maybe TokenizeError, State )
matchHexInt c state =
    -- 0x was already parsed
    let
        ( finalHexNumber, state_ ) =
            Tokenize.matchWhile Char.isHexDigit state
    in
    case Hex.fromString (String.toLower finalHexNumber) of
        Err _ ->
            -- Shouldn't happen
            ( Just (TokenizeCompilerBug "matchWhile isHexDigit -> Hex.fromString failed")
            , state_
            )

        Ok int ->
            found (Int (possiblyNegate c int)) state_


possiblyNegate : { shouldNegate : Bool } -> number -> number
possiblyNegate { shouldNegate } n =
    if shouldNegate then
        negate n

    else
        n


matchScientificFloat : Float -> { shouldNegate : Bool } -> State -> ( Maybe TokenizeError, State )
matchScientificFloat float c state =
    -- 'E' has already been parsed
    let
        ( shouldNegateExponent, stateAfterMinus ) =
            if Tokenize.next 1 state == "-" then
                ( True, Tokenize.skip 1 state )

            else
                ( False, state )

        ( finalExponent, stateAfterExponent ) =
            Tokenize.matchWhile Char.isDigit stateAfterMinus
    in
    case String.toInt finalExponent of
        Nothing ->
            ( Just (TokenizeCompilerBug "matchWhile isDigit >> String.toInt failed")
            , stateAfterExponent
            )

        Just exponent ->
            let
                coefficient =
                    possiblyNegate c float

                negatedExponent =
                    exponent
                        |> toFloat
                        |> possiblyNegate { shouldNegate = shouldNegateExponent }
            in
            found (Float (coefficient * 10 ^ negatedExponent)) stateAfterExponent


matchDotFloat : Int -> { shouldNegate : Bool } -> State -> ( Maybe TokenizeError, State )
matchDotFloat int c state =
    -- '.' has already been parsed
    let
        ( finalDecimal, stateAfterDecimal ) =
            Tokenize.matchWhile Char.isDigit state

        floatString =
            String.fromInt int ++ "." ++ finalDecimal
    in
    case String.toFloat floatString of
        Nothing ->
            ( Just (TokenizeCompilerBug "matchWhile isDigit >> String.toFloat failed")
            , stateAfterDecimal
            )

        Just coefficient ->
            -- We could end here but we could also continue with `E-123` scientific notation
            if String.toUpper (Tokenize.next 1 stateAfterDecimal) == "E" then
                matchScientificFloat coefficient c (Tokenize.skip 1 stateAfterDecimal)

            else
                found (Float (possiblyNegate c coefficient)) stateAfterDecimal


matchLineComment : State -> ( Maybe TokenizeError, State )
matchLineComment state =
    ( Nothing, Tokenize.skipUntil (\c -> c == '\n') state )


matchMultilineComment : State -> ( Maybe TokenizeError, State )
matchMultilineComment state =
    let
        ranOut : State -> ( Maybe TokenizeError, State )
        ranOut s =
            ( Just
                (EndOfDocCommentNotFound
                    { startLine = state.line
                    , startColumn = state.column
                    }
                )
            , s
            )

        go : Int -> State -> ( Maybe TokenizeError, State )
        go level accState =
            if level <= 0 then
                ( Nothing, accState )

            else
                let
                    ( matchedChar, stateAfterSkip ) =
                        Tokenize.skipUntilOneOf [ '-', '{' ] accState
                in
                case matchedChar of
                    Nothing ->
                        ranOut stateAfterSkip

                    Just '-' ->
                        case stateAfterSkip.program of
                            [] ->
                                ranOut stateAfterSkip

                            '}' :: _ ->
                                go (level - 1) (Tokenize.skip 1 stateAfterSkip)

                            _ ->
                                -- This `-` was not part of the ending `-}`
                                go level stateAfterSkip

                    Just '{' ->
                        case stateAfterSkip.program of
                            [] ->
                                ranOut stateAfterSkip

                            '-' :: _ ->
                                go (level + 1) (Tokenize.skip 1 stateAfterSkip)

                            _ ->
                                -- This `{` was not part of a new `{-`
                                go level stateAfterSkip

                    Just _ ->
                        -- Shouldn't happen
                        go level stateAfterSkip
    in
    go 1 state


matchLowerName : Char -> State -> ( Maybe TokenizeError, State )
matchLowerName first state =
    matchName lowerNameWithKeywordCheck first state


matchUpperName : Char -> State -> ( Maybe TokenizeError, State )
matchUpperName first state =
    matchName UpperName first state


lowerNameWithKeywordCheck : String -> Token.Type
lowerNameWithKeywordCheck name =
    Dict.get name reservedKeywords
        |> Maybe.withDefault (LowerName name)


reservedOperators : Dict String Token.Type
reservedOperators =
    Dict.fromList
        [ ( ".", Dot )
        , ( "=", Equals )
        , ( "-", Minus )
        , ( "->", RightArrow )
        , ( ":", Colon )
        , ( "|", Pipe )

        --, ( "(..)", All ) -- '(' is not reserved
        --, ( ",", Comma ) -- ',' is not reserved
        --, ( "\\", Backslash ) -- '\' is not reserved
        --, ( "(", LeftParen ) -- '(' is not reserved
        --, ( ")", RightParen ) -- ')' is not reserved
        --, ( "[", LeftSquareBracket ) -- '[' is not reserved
        --, ( "]", RightSquareBracket ) -- ']' is not reserved
        --, ( "{", LeftCurlyBracket ) -- '{' is not reserved
        --, ( "}", RightCurlyBracket ) -- '}' is not reserved
        --, ( "_", Underscore ) -- '_' is not reserved
        ]


reservedKeywords : Dict String Token.Type
reservedKeywords =
    Dict.fromList
        [ ( "alias", Alias )
        , ( "as", As )
        , ( "case", Case )
        , ( "effect", Effect )
        , ( "else", Else )
        , ( "exposing", Exposing )
        , ( "if", If )
        , ( "import", Import )
        , ( "in", In )
        , ( "let", Let )
        , ( "module", Module )
        , ( "of", Of )
        , ( "port", Port )
        , ( "then", Then )
        , ( "type", Type )
        ]


matchName : (String -> Token.Type) -> Char -> State -> ( Maybe TokenizeError, State )
matchName toTokenType first state =
    let
        ( finalName, state_ ) =
            Tokenize.matchWhile isIdentifierTailChar state
                |> Tuple.mapFirst (String.cons first)

        tokenType =
            toTokenType finalName
    in
    found tokenType state_


matchOperator : State -> ( Maybe TokenizeError, State )
matchOperator state =
    let
        ( finalOperator, state_ ) =
            Tokenize.matchWhile isOperatorChar state
    in
    Dict.get finalOperator reservedOperators
        |> Maybe.withDefault (Operator finalOperator)
        |> (\op -> found op state_)


isOperatorChar : Char -> Bool
isOperatorChar char =
    Set.member char operatorChars


operatorChars : Set Char
operatorChars =
    "+-/*=.<>:&|^?%!"
        |> String.toList
        |> Set.fromList


isIdentifierTailChar : Char -> Bool
isIdentifierTailChar char =
    Char.isUpper char || Char.isLower char || Char.isDigit char || char == '_'
