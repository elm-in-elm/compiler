module Stage.Tokenize exposing (tokenize)

import Elm.Compiler.Error exposing (Error(..), TokenizeError(..))
import Elm.Data.FileContents exposing (FileContents)
import Elm.Data.FilePath exposing (FilePath)
import Elm.Data.Token exposing (Token(..))
import List.Extra as List
import List.NonEmpty exposing (NonEmpty)
import Set exposing (Set)


tokenize : String -> Result Error (List Token)
tokenize string =
    tokenizeString string
        |> Result.mapError TokenizeError
        |> Result.map (.tokens >> List.reverse)


type Possibility
    = Exact String (State -> ( Maybe TokenizeError, State ))
    | IsNegatedNumber
    | IsOperatorStartingWithEquals


type alias State =
    { program : List Char
    , line : Int
    , column : Int
    , tokens : List Token
    }


initState : String -> State
initState string =
    { program = String.toList string
    , line = 1
    , column = 1
    , tokens = []
    }


tokenizeString : String -> Result TokenizeError State
tokenizeString string =
    let
        go : State -> Result TokenizeError State
        go state =
            if List.isEmpty state.program then
                Ok state

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
parseNextToken state =
    case state.program of
        [] ->
            ( Nothing, state )

        char :: rest ->
            (case char of
                ' ' ->
                    \s -> ( Nothing, skipWhile (\c -> c == ' ') s )

                '\n' ->
                    \s -> ( Nothing, skipWhile (\c -> c == '\n') s )

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
                        { else_ = skip 1 >> found LeftParen }

                ')' ->
                    skip 1 >> found RightParen

                ',' ->
                    skip 1 >> found Comma

                '-' ->
                    oneOf
                        [ Exact "--" matchLineComment
                        , Exact "->" (found RightArrow)
                        , IsNegatedNumber
                        ]
                        { else_ = skip 1 >> found Minus }

                '.' ->
                    skip 1 >> found Dot

                ':' ->
                    skip 1 >> found Colon

                '=' ->
                    oneOf
                        [ IsOperatorStartingWithEquals ]
                        { else_ = skip 1 >> found Equals }

                '\\' ->
                    skip 1 >> found Backslash

                '_' ->
                    skip 1 >> found Underscore

                '{' ->
                    oneOf
                        [ Exact "{-|" matchMultilineComment
                        , Exact "{-" matchMultilineComment
                        ]
                        { else_ = skip 1 >> found LeftCurlyBracket }

                '}' ->
                    skip 1 >> found RightCurlyBracket

                '|' ->
                    skip 1 >> found Pipe

                'a' ->
                    oneOf
                        [ Exact "alias" (found Alias)
                        , Exact "as" (found As)
                        ]
                        { else_ = skip 1 >> matchLowerName char }

                'c' ->
                    oneOf
                        [ Exact "case" (found Case) ]
                        { else_ = skip 1 >> matchLowerName char }

                'e' ->
                    oneOf
                        [ Exact "effect" (found Effect)
                        , Exact "else" (found Else)
                        , Exact "exposing" (found Exposing)
                        ]
                        { else_ = skip 1 >> matchLowerName char }

                'i' ->
                    oneOf
                        [ Exact "if" (found If)
                        , Exact "import" (found Import)
                        , Exact "in" (found In)
                        ]
                        { else_ = skip 1 >> matchLowerName char }

                'l' ->
                    oneOf
                        [ Exact "let" (found Let) ]
                        { else_ = skip 1 >> matchLowerName char }

                'm' ->
                    oneOf
                        [ Exact "module" (found Module) ]
                        { else_ = skip 1 >> matchLowerName char }

                'o' ->
                    oneOf
                        [ Exact "of" (found Of) ]
                        { else_ = skip 1 >> matchLowerName char }

                'p' ->
                    oneOf
                        [ Exact "port" (found Port) ]
                        { else_ = skip 1 >> matchLowerName char }

                't' ->
                    oneOf
                        [ Exact "then" (found Then)
                        , Exact "type" (found Type)
                        ]
                        { else_ = skip 1 >> matchLowerName char }

                _ ->
                    if Char.isLower char then
                        skip 1 >> matchLowerName char

                    else if Char.isUpper char then
                        skip 1 >> matchUpperName char

                    else if Char.isDigit char then
                        \s ->
                            case matchNumber identity s of
                                Nothing ->
                                    -- Shouldn't happen
                                    ( Nothing, s )

                                Just result ->
                                    result

                    else if isOperatorChar char then
                        skip 1 >> matchOperator char

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
                            |> skip (String.length string)
                            |> then_

                    else
                        nope ()

                IsNegatedNumber ->
                    -- we're guaranteed we're sitting on `-`
                    case matchNumber negate (skip 1 state) of
                        Nothing ->
                            nope ()

                        Just result ->
                            result

                IsOperatorStartingWithEquals ->
                    let
                        ( finalOperator, state_ ) =
                            matchWhile isOperatorChar state
                    in
                    if String.length finalOperator == 1 then
                        nope ()

                    else
                        found (Operator finalOperator) state_


found : Token -> State -> ( Maybe TokenizeError, State )
found token state =
    ( Nothing, { state | tokens = token :: state.tokens } )


matchNumber : (Int -> Int) -> State -> Maybe ( Maybe TokenizeError, State )
matchNumber fn state =
    -- TODO 123.45
    -- TODO 1.3E-25
    -- TODO 0x13
    let
        ( finalNumber, state_ ) =
            matchWhile Char.isDigit state
    in
    case String.toInt finalNumber of
        Nothing ->
            {- Can happen eg. with `1 - 2` right after the "-": the " " is not
               a number so our matched numeric string is empty.
            -}
            Nothing

        Just int ->
            Just <| found (Int (fn int)) state_


matchLineComment : State -> ( Maybe TokenizeError, State )
matchLineComment state =
    ( Nothing, skipUntil (\c -> c == '\n') state )


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
                        skipUntilOneOf [ '-', '{' ] accState
                in
                case matchedChar of
                    Nothing ->
                        ranOut stateAfterSkip

                    Just '-' ->
                        case stateAfterSkip.program of
                            [] ->
                                ranOut stateAfterSkip

                            '}' :: _ ->
                                go (level - 1) (skip 1 stateAfterSkip)

                            _ ->
                                -- This `-` was not part of the ending `-}`
                                go level stateAfterSkip

                    Just '{' ->
                        case stateAfterSkip.program of
                            [] ->
                                ranOut stateAfterSkip

                            '-' :: _ ->
                                go (level + 1) (skip 1 stateAfterSkip)

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
    matchName LowerName first state


matchUpperName : Char -> State -> ( Maybe TokenizeError, State )
matchUpperName first state =
    matchName UpperName first state


matchName : (String -> Token) -> Char -> State -> ( Maybe TokenizeError, State )
matchName toToken first state =
    let
        ( finalName, state_ ) =
            matchWhile isIdentifierTailChar state
                |> Tuple.mapFirst (String.cons first)

        token =
            toToken finalName
    in
    ( Nothing, { state_ | tokens = token :: state_.tokens } )


matchOperator : Char -> State -> ( Maybe TokenizeError, State )
matchOperator char state =
    let
        ( finalOperator, state_ ) =
            matchWhile isOperatorChar state
                |> Tuple.mapFirst (String.cons char)
    in
    found (Operator finalOperator) state_


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


skipWhile : (Char -> Bool) -> State -> State
skipWhile pred state =
    case state.program of
        [] ->
            state

        x :: xs ->
            if pred x then
                skipWhile pred (skip 1 state)

            else
                state


skipUntil : (Char -> Bool) -> State -> State
skipUntil pred state =
    skipWhile (not << pred) state


skipUntilOneOf : List Char -> State -> ( Maybe Char, State )
skipUntilOneOf wantedChars state =
    case state.program of
        [] ->
            ( Nothing, state )

        nextChar :: restOfChars ->
            let
                go : List Char -> State -> ( Maybe Char, State )
                go accWantedChars accState =
                    case accWantedChars of
                        [] ->
                            {- Tried all wanted chars on the current char.
                               Move to the next one.
                            -}
                            skipUntilOneOf wantedChars (skip 1 accState)

                        c :: cs ->
                            if c == nextChar then
                                ( Just c, skip 1 accState )

                            else
                                go cs accState
            in
            go wantedChars state


matchWhile : (Char -> Bool) -> State -> ( String, State )
matchWhile pred state =
    let
        go : List Char -> State -> ( String, State )
        go accChars accState =
            case accState.program of
                [] ->
                    ( accChars |> List.reverse |> String.fromList, accState )

                c :: _ ->
                    if pred c then
                        go (c :: accChars) (skip 1 accState)

                    else
                        ( accChars |> List.reverse |> String.fromList, accState )
    in
    go [] state


skip : Int -> State -> State
skip n state =
    -- Doesn't check for \t
    if n <= 0 then
        state

    else
        case state.program of
            [] ->
                state

            c :: cs ->
                let
                    isNewline =
                        c == '\n'
                in
                skip (n - 1)
                    { state
                        | program = cs
                        , line =
                            if isNewline then
                                state.line + 1

                            else
                                state.line
                        , column =
                            if isNewline then
                                1

                            else
                                state.column + 1
                    }
