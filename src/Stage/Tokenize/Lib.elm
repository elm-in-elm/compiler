module Stage.Tokenize.Lib exposing
    ( State
    , next
    , skip, skipWhile, skipUntil, skipUntilOneOf
    , matchWhile, matchUntilOneOf
    )

{-|

@docs State


## Peeking

@docs next


## Skipping

@docs skip, skipWhile, skipUntil, skipUntilOneOf


## Matching

@docs matchWhile, matchUntilOneOf

-}


type alias State r =
    { r
        | program : List Char
        , line : Int
        , column : Int
    }


next : Int -> State r -> String
next n state =
    state.program
        |> List.take n
        |> String.fromList


skipWhile : (Char -> Bool) -> State r -> State r
skipWhile pred state =
    case state.program of
        [] ->
            state

        x :: xs ->
            if pred x then
                skipWhile pred (skip 1 state)

            else
                state


skipUntil : (Char -> Bool) -> State r -> State r
skipUntil pred state =
    skipWhile (not << pred) state


skipUntilOneOf : List Char -> State r -> ( Maybe Char, State r )
skipUntilOneOf wantedChars state =
    case state.program of
        [] ->
            ( Nothing, state )

        nextChar :: restOfChars ->
            let
                go : List Char -> State r -> ( Maybe Char, State r )
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


skip : Int -> State r -> State r
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


matchWhile : (Char -> Bool) -> State r -> ( String, State r )
matchWhile pred state =
    let
        go : List Char -> State r -> ( String, State r )
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


matchUntilOneOf : List Char -> State r -> ( Maybe ( Char, String ), State r )
matchUntilOneOf wantedChars state =
    let
        goOuter : List Char -> State r -> ( Maybe ( Char, String ), State r )
        goOuter accMatched accState =
            case accState.program of
                [] ->
                    ( Nothing, accState )

                nextChar :: restOfChars ->
                    let
                        goInner : List Char -> State r -> ( Maybe ( Char, String ), State r )
                        goInner accWantedChars accState_ =
                            case accWantedChars of
                                [] ->
                                    {- Tried all wanted chars on the current char.
                                       Move to the next one.
                                    -}
                                    goOuter (nextChar :: accMatched) (skip 1 accState_)

                                c :: cs ->
                                    if c == nextChar then
                                        ( Just
                                            ( c
                                            , accMatched
                                                |> List.reverse
                                                |> String.fromList
                                            )
                                        , skip 1 accState_
                                        )

                                    else
                                        goInner cs accState_
                    in
                    goInner wantedChars accState
    in
    goOuter [] state
