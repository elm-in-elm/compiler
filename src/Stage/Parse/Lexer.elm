module Stage.Parse.Lexer exposing (..)

import Elm.Data.Located as Located exposing (Located)
import Parser.Advanced as P exposing ((|.), (|=), Parser)
import Set


type LexItem
    = Sigil LexSigil
    | Token String
    | NumericLiteral String
    | TextLiteral LexLiteralType String
    | Whitespace Int
    | Newline Int
    | Comment LexCommentType String
    | Invalid String


type LexSigil
    = Bracket BracketType BracketTodoNameMe
    | Assign
    | Pipe
    | Comma
    | SingleDot
    | DoubleDot
    | ThinArrow
    | Backslash
    | Underscore
    | Colon
    | BinaryOperator LexBinaryOperator


type LexCommentType
    = LineComment
    | MutlilineComment
    | DocComment


type LexBinaryOperator
    = Add
    | Subtract
    | Multiply
    | Divide
    | Exponentiate
    | And
    | Or
    | Equals
    | GreaterThan
    | GreaterThanEquals
    | LessThan
    | LessThanEquals
    | Append


type BracketType
    = Round
    | Square
    | Curly


type BracketTodoNameMe
    = Open
    | Close


type LexLiteralType
    = StringL StringType
    | CharL


type StringType
    = Single
    | Triple


type LexProblem
    = ExpectingToken
    | ExpectingSigil
    | ExpectingLiteralStart LexLiteralType
    | ExpectingLiteralEnd LexLiteralType
    | ExpectingBackslash
    | ExpectingAnything
    | ExpectingWhitespace
    | ExpectingNewline
    | ExpectingLineComment
    | ExpectingNumericLiteral
    | ExpectingEscape
    | ExpectingEnd


type alias Parser_ a =
    Parser Never LexProblem a


toString : LexItem -> String
toString item =
    case item of
        Sigil (Bracket Round Open) ->
            "("

        Sigil (Bracket Round Close) ->
            ")"

        Sigil (Bracket Square Open) ->
            "["

        Sigil (Bracket Square Close) ->
            "]"

        Sigil (Bracket Curly Open) ->
            "{"

        Sigil (Bracket Curly Close) ->
            "}"

        Sigil Assign ->
            "="

        Sigil Pipe ->
            "|"

        Sigil Comma ->
            ","

        Sigil SingleDot ->
            "."

        Sigil DoubleDot ->
            ".."

        Sigil ThinArrow ->
            "->"

        Sigil Backslash ->
            "\\"

        Sigil Colon ->
            ":"

        Sigil Underscore ->
            "_"

        Sigil (BinaryOperator Add) ->
            "+"

        Sigil (BinaryOperator Subtract) ->
            "-"

        Sigil (BinaryOperator Multiply) ->
            "*"

        Sigil (BinaryOperator Divide) ->
            "/"

        Sigil (BinaryOperator Exponentiate) ->
            "^"

        Sigil (BinaryOperator And) ->
            "&&"

        Sigil (BinaryOperator Or) ->
            "||"

        Sigil (BinaryOperator Equals) ->
            "=="

        Sigil (BinaryOperator GreaterThan) ->
            ">"

        Sigil (BinaryOperator GreaterThanEquals) ->
            ">="

        Sigil (BinaryOperator LessThan) ->
            "<"

        Sigil (BinaryOperator LessThanEquals) ->
            "<="

        Sigil (BinaryOperator Append) ->
            "++"

        Token s ->
            s

        NumericLiteral s ->
            s

        TextLiteral ty s ->
            delimiterFor ty ++ s ++ delimiterFor ty

        Whitespace i ->
            String.repeat i " "

        Newline i ->
            "\n" ++ String.repeat i " "

        Comment LineComment s ->
            "//" ++ s

        Comment MutlilineComment s ->
            "{-" ++ s ++ "-}"

        Comment DocComment s ->
            "{-|" ++ s ++ "-}"

        Invalid s ->
            s


located : Parser_ p -> Parser_ (Located p)
located p =
    P.succeed
        (\( startRow, startCol ) value ( endRow, endCol ) ->
            Located.located
                { start = { row = startRow, col = startCol }
                , end = { row = endRow, col = endCol }
                }
                value
        )
        |= P.getPosition
        |= p
        |= P.getPosition


parser : Parser_ (List (Located LexItem))
parser =
    P.loop
        []
        (\reversed ->
            P.oneOf
                [ P.oneOf
                    ([ -- commentParser must come before sigil parser as the sigil
                       -- parser will try to interpret "--" as a sigil
                       P.variable
                        { start = Char.isAlphaNum
                        , inner = \c -> Char.isAlphaNum c || c == '_'
                        , reserved = Set.empty
                        , expecting = ExpectingToken
                        }
                        |> P.map Token
                     , numericLiteralParser
                        |> P.map NumericLiteral
                     , textLiteralParser
                        |> P.map
                            (\( ty, terminates, literalBody ) ->
                                if terminates then
                                    TextLiteral ty literalBody

                                else
                                    Invalid (delimiterFor ty ++ literalBody)
                            )
                     , commentParser
                        |> P.map (\( ty, commentBody ) -> Comment ty commentBody)
                     , sigilParser
                        |> P.map Sigil
                     , P.symbol (P.Token " " ExpectingWhitespace)
                        |> P.andThen (\() -> chompSpacesAndCount)
                        |> P.map (\count -> Whitespace (count + 1))
                     , P.oneOf
                        [ P.symbol (P.Token "\n\u{000D}" ExpectingNewline)
                        , P.symbol (P.Token "\n" ExpectingNewline)
                        ]
                        |> P.andThen (\() -> chompSpacesAndCount)
                        |> P.map Newline
                     ]
                        |> List.map (located >> P.map (\t -> P.Loop (t :: reversed)))
                    )
                , P.end ExpectingEnd
                    |> P.map (\() -> P.Done (List.reverse reversed))
                ]
        )


chompSpacesAndCount : Parser_ Int
chompSpacesAndCount =
    P.chompWhile (\c -> c == ' ')
        |> P.getChompedString
        |> P.map String.length


alphaOrNumOr_ : Char -> Bool
alphaOrNumOr_ c =
    Char.isAlphaNum c || c == '_'


textLiteralParser : Parser_ ( LexLiteralType, Bool, String )
textLiteralParser =
    P.oneOf
        [ -- order matters! We must try parsing a tripple delimited string first!
          delimitedLiteral (StringL Triple)
        , delimitedLiteral (StringL Single)
        , delimitedLiteral CharL
        ]


delimiterFor : LexLiteralType -> String
delimiterFor ty =
    case ty of
        StringL Single ->
            "\""

        StringL Triple ->
            "\"\"\""

        CharL ->
            "'"


commentParser : Parser_ ( LexCommentType, String )
commentParser =
    P.oneOf
        [ P.symbol (P.Token "--" ExpectingLineComment)
            |> P.andThen (\() -> P.chompWhile (\c -> c /= '\n') |> P.getChompedString)
            |> P.map (Tuple.pair LineComment)
        ]


delimitedLiteral : LexLiteralType -> Parser_ ( LexLiteralType, Bool, String )
delimitedLiteral ty =
    let
        delimiter =
            delimiterFor ty
    in
    P.succeed
        (\( body, terminates ) ->
            ( ty
            , terminates
            , if terminates then
                String.dropRight (String.length (delimiterFor ty)) body

              else
                body
            )
        )
        |. P.symbol (P.Token delimiter (ExpectingLiteralStart ty))
        |= P.mapChompedString
            Tuple.pair
            (P.loop ()
                (\() ->
                    P.oneOf
                        [ P.token (P.Token delimiter (ExpectingLiteralEnd ty))
                            |> P.map (\() -> P.Done True)
                        , P.token (P.Token "\\" ExpectingEscape)
                            |> P.andThen
                                (\() -> P.chompIf (\c -> True) ExpectingAnything)
                            |> P.map P.Loop
                        , P.chompIf (\_ -> True) ExpectingAnything
                            |> P.map P.Loop
                        , P.end ExpectingEnd
                            |> P.map (\() -> P.Done False)
                        ]
                )
            )


numericLiteralParser : Parser_ String
numericLiteralParser =
    P.getChompedString
        (P.oneOf
            [ P.chompIf Char.isDigit ExpectingNumericLiteral
                |> P.andThen (\() -> P.chompWhile (\c -> Char.isAlphaNum c || c == '_'))
            , P.backtrackable
                (P.succeed ()
                    |. P.chompIf (\c -> c == '-') ExpectingNumericLiteral
                    |. P.chompIf Char.isDigit ExpectingNumericLiteral
                    |. P.chompWhile (\c -> Char.isAlphaNum c || c == '_')
                )
            ]
        )


sigilParser : Parser_ LexSigil
sigilParser =
    P.oneOf
        [ -- Two character sigils (must come first)
          P.symbol (P.Token "&&" ExpectingSigil)
            |> P.map (\() -> BinaryOperator And)
        , P.symbol (P.Token "++" ExpectingSigil)
            |> P.map (\() -> BinaryOperator Append)
        , P.symbol (P.Token "==" ExpectingSigil)
            |> P.map (\() -> BinaryOperator Equals)
        , P.symbol (P.Token "||" ExpectingSigil)
            |> P.map (\() -> BinaryOperator Or)
        , P.symbol (P.Token ".." ExpectingSigil)
            |> P.map (\() -> DoubleDot)

        -- Single character sigils
        , P.symbol (P.Token "^" ExpectingSigil)
            |> P.map (\() -> BinaryOperator Exponentiate)
        , P.symbol (P.Token "\\" ExpectingSigil)
            |> P.map (\() -> Backslash)
        , P.symbol (P.Token "_" ExpectingSigil)
            |> P.map (\() -> Underscore)
        , P.symbol (P.Token "(" ExpectingSigil)
            |> P.map (\() -> Bracket Round Open)
        , P.symbol (P.Token ")" ExpectingSigil)
            |> P.map (\() -> Bracket Round Close)
        , P.symbol (P.Token ">" ExpectingSigil)
            |> P.map (\() -> BinaryOperator GreaterThan)
        , P.symbol (P.Token "<" ExpectingSigil)
            |> P.map (\() -> BinaryOperator LessThan)
        , P.symbol (P.Token ">=" ExpectingSigil)
            |> P.map (\() -> BinaryOperator GreaterThanEquals)
        , P.symbol (P.Token "<=" ExpectingSigil)
            |> P.map (\() -> BinaryOperator LessThanEquals)
        , P.symbol (P.Token "-" ExpectingSigil)
            |> P.map (\() -> BinaryOperator Subtract)
        , P.symbol (P.Token "+" ExpectingSigil)
            |> P.map (\() -> BinaryOperator Add)
        , P.symbol (P.Token "=" ExpectingSigil)
            |> P.map (\() -> Assign)
        , P.symbol (P.Token "/" ExpectingSigil)
            |> P.map (\() -> BinaryOperator Divide)
        , P.symbol (P.Token "*" ExpectingSigil)
            |> P.map (\() -> BinaryOperator Multiply)
        , P.symbol (P.Token "{" ExpectingSigil)
            |> P.map (\() -> Bracket Curly Open)
        , P.symbol (P.Token "[" ExpectingSigil)
            |> P.map (\() -> Bracket Square Open)
        , P.symbol (P.Token "}" ExpectingSigil)
            |> P.map (\() -> Bracket Curly Close)
        , P.symbol (P.Token "]" ExpectingSigil)
            |> P.map (\() -> Bracket Square Close)
        , P.symbol (P.Token ":" ExpectingSigil)
            |> P.map (\() -> Colon)
        , P.symbol (P.Token "," ExpectingSigil)
            |> P.map (\() -> Comma)
        , P.symbol (P.Token "." ExpectingSigil)
            |> P.map (\() -> SingleDot)
        , P.symbol (P.Token "|" ExpectingSigil)
            |> P.map (\() -> Pipe)
        ]
