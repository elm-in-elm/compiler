module Elm.Data.Token exposing (Token, Type(..))


type alias Token =
    { line : Int -- 1-based
    , column : Int -- 1-based
    , type_ : Type
    }


type Type
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
