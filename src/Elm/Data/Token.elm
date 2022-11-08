module Elm.Data.Token exposing (Token(..))


type Token
    = -- parameterized
      LowerName String -- helloWorld
    | UpperName String -- HelloWorld
    | Int Int -- 123
    | Float Float -- 123.45
    | Char Char -- 'c'
    | String String -- "hello"
    | Operator String -- +
      -- keywords
    | Port -- port
    | Effect -- effect
    | Module -- module
    | Exposing -- exposing
    | Type -- type
    | Alias -- alias
    | Import -- import
    | If -- if
    | Then -- then
    | Else -- else
    | Let -- let
    | In -- in
    | Case -- case
    | Of -- of
      -- Symbols
    | Dot -- .
    | All -- (..)
    | Comma -- ,
    | As -- as
    | Equals -- =
    | Minus -- -
    | Backslash -- \
    | RightArrow -- ->
    | LeftParen -- (
    | RightParen -- )
    | LeftCurlyBracket -- {
    | RightCurlyBracket -- }
    | Colon -- :
    | Pipe -- |
    | Underscore -- _
