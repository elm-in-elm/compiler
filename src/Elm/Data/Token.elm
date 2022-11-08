module Elm.Data.Token exposing
    ( T(..)
    , Token
    , Type(..)
    , flatten
    , getString
    , getUpperName
    , tToString
    , typeToString
    )

-- TODO metadata for effect module


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
    | String String -- "hello", """hello"""
    | Operator String -- +
    | GlslShader String -- [glsl|...|]
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
    | As -- as
      -- Symbols
    | Dot -- .
    | All -- (..)
    | Comma -- ,
    | Equals -- =
    | Minus -- -
    | Backslash -- \
    | RightArrow -- ->
    | LeftParen -- (
    | RightParen -- )
    | LeftSquareBracket -- [
    | RightSquareBracket -- ]
    | LeftCurlyBracket -- {
    | RightCurlyBracket -- }
    | Colon -- :
    | Pipe -- |
    | Underscore -- _


{-| For easier specification of parameterized types
-}
type T
    = TLowerName
    | TUpperName
    | TInt
    | TFloat
    | TChar
    | TString
    | TOperator
    | TGlslShader
    | TOther


flatten : Type -> T
flatten type_ =
    case type_ of
        LowerName _ ->
            TLowerName

        UpperName _ ->
            TUpperName

        Int _ ->
            TInt

        Float _ ->
            TFloat

        Char _ ->
            TChar

        String _ ->
            TString

        Operator _ ->
            TOperator

        GlslShader _ ->
            TGlslShader

        _ ->
            TOther


getUpperName : Token -> Maybe String
getUpperName token =
    case token.type_ of
        UpperName name ->
            Just name

        _ ->
            Nothing


typeToString : Type -> String
typeToString t =
    case t of
        LowerName _ ->
            "LowerName"

        UpperName _ ->
            "UpperName"

        Int _ ->
            "Int"

        Float _ ->
            "Float"

        Char _ ->
            "Char"

        String _ ->
            "String"

        Operator _ ->
            "Operator"

        GlslShader _ ->
            "GlslShader"

        Port ->
            "Port"

        Effect ->
            "Effect"

        Module ->
            "Module"

        Exposing ->
            "Exposing"

        Type ->
            "Type"

        Alias ->
            "Alias"

        Import ->
            "Import"

        If ->
            "If"

        Then ->
            "Then"

        Else ->
            "Else"

        Let ->
            "Let"

        In ->
            "In"

        Case ->
            "Case"

        Of ->
            "Of"

        As ->
            "As"

        Dot ->
            "Dot"

        All ->
            "All"

        Comma ->
            "Comma"

        Equals ->
            "Equals"

        Minus ->
            "Minus"

        Backslash ->
            "Backslash"

        RightArrow ->
            "Right"

        LeftParen ->
            "Left"

        RightParen ->
            "Right"

        LeftSquareBracket ->
            "Left"

        RightSquareBracket ->
            "Right"

        LeftCurlyBracket ->
            "Left"

        RightCurlyBracket ->
            "Right"

        Colon ->
            "Colon"

        Pipe ->
            "Pipe"

        Underscore ->
            "Underscore"


tToString : T -> String
tToString t =
    case t of
        TLowerName ->
            "TLowerName"

        TUpperName ->
            "TUpperName"

        TInt ->
            "TInt"

        TFloat ->
            "TFloat"

        TChar ->
            "TChar"

        TString ->
            "TString"

        TOperator ->
            "TOperator"

        TGlslShader ->
            "TGlslShader"

        TOther ->
            "TOther"


getString : Type -> Maybe String
getString t =
    case t of
        LowerName str ->
            Just str

        UpperName str ->
            Just str

        Int _ ->
            Nothing

        Float _ ->
            Nothing

        Char _ ->
            Nothing

        String str ->
            Just str

        Operator str ->
            Just str

        GlslShader str ->
            Just str

        _ ->
            Nothing
