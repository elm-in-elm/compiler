module Stage.Parse.Lexer exposing (..)

import Elm.Data.Located as Located exposing (Located)
import Elm.Data.Operator as Operator exposing (Operator)
import Parser.Advanced as P exposing ((|.), (|=), Parser)
import Set
import Stage.Parse.Token as Token


type LexItem
    = Sigil LexSigil
    | Identifier
        { qualifiers : List String
        , name : String
        }
    | Keyword Token.Keyword
    | NumericLiteral String
    | TextLiteral LexLiteralType String
    | Whitespace Int
    | Newlines (List Int) Int
    | Comment LexCommentType String
    | IdentifierWithTrailingDot
        { qualifiers : List String
        , name : String
        }
    | Invalid String


type LexSigil
    = Bracket BracketType BracketRole
    | Assign
    | Pipe
    | Comma
      -- TODO(harry): remove and replace by property-accessor etc.
    | SingleDot
    | DoubleDot
    | ThinArrow
    | Backslash
    | Underscore
    | Colon
    | Operator Operator


type LexCommentType
    = LineComment
    | MutlilineComment
    | DocComment


type BracketType
    = Round
    | Square
    | Curly


type BracketRole
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
    | ExpectingKeyword
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

        Sigil (Operator op) ->
            Operator.toString op

        Identifier { qualifiers, name } ->
            (qualifiers ++ [ name ])
                |> String.join "."

        Keyword k ->
            Token.keywordToString k

        NumericLiteral s ->
            s

        TextLiteral ty s ->
            delimiterFor ty ++ s ++ delimiterFor ty

        Whitespace i ->
            String.repeat i " "

        Newlines empties identationSpaces ->
            (empties
                |> List.map (\spacesInEmptyLine -> "\n" ++ String.repeat spacesInEmptyLine " ")
                |> String.join ""
            )
                ++ "\n"
                ++ String.repeat identationSpaces " "

        Comment LineComment s ->
            "//" ++ s

        Comment MutlilineComment s ->
            "{-" ++ s ++ "-}"

        Comment DocComment s ->
            "{-|" ++ s ++ "-}"

        IdentifierWithTrailingDot { qualifiers, name } ->
            (qualifiers ++ [ name, "" ])
                |> String.join "."

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
                       -- parser will try to interpret "--" as two "-" sigils.
                       identifierParser
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
                     , newlinesParser
                        |> P.map (\( emptyLines, indentation ) -> Newlines emptyLines indentation)
                     , P.getChompedString (P.chompIf (\_ -> True) ExpectingAnything)
                        |> P.map (\s -> Invalid s)
                     ]
                        |> List.map (located >> P.map (\t -> P.Loop (t :: reversed)))
                    )
                , P.end ExpectingEnd
                    |> P.map (\() -> P.Done (List.reverse reversed))
                ]
        )


identifierParser : Parser_ LexItem
identifierParser =
    let
        word =
            P.variable
                { start = Char.isAlpha
                , inner = \c -> Char.isAlphaNum c || c == '_'
                , reserved = Set.empty
                , expecting = ExpectingToken
                }

        loopHelp { reversedQualifiers, name } =
            P.oneOf
                [ P.succeed (\x -> x)
                    |. P.symbol (P.Token "." ExpectingSigil)
                    |= P.oneOf
                        [ word
                            |> P.map
                                (\new ->
                                    P.Loop
                                        { reversedQualifiers = name :: reversedQualifiers
                                        , name = new
                                        }
                                )
                        , { qualifiers = List.reverse reversedQualifiers
                          , name = name
                          }
                            |> IdentifierWithTrailingDot
                            |> P.Done
                            |> P.succeed
                        ]
                , { qualifiers = List.reverse reversedQualifiers
                  , name = name
                  }
                    |> Identifier
                    |> P.Done
                    |> P.succeed
                ]
    in
    P.oneOf
        [ P.token (P.Token "module" ExpectingKeyword)
            |> P.map (\() -> Keyword Token.Module)
        , P.token (P.Token "type" ExpectingKeyword)
            |> P.map (\() -> Keyword Token.Type)
        , P.token (P.Token "alias" ExpectingKeyword)
            |> P.map (\() -> Keyword Token.Alias)
        , P.token (P.Token "exposing" ExpectingKeyword)
            |> P.map (\() -> Keyword Token.Exposing)
        , P.token (P.Token "case" ExpectingKeyword)
            |> P.map (\() -> Keyword Token.Case)
        , P.token (P.Token "of" ExpectingKeyword)
            |> P.map (\() -> Keyword Token.Of)
        , P.token (P.Token "if" ExpectingKeyword)
            |> P.map (\() -> Keyword Token.If)
        , P.token (P.Token "then" ExpectingKeyword)
            |> P.map (\() -> Keyword Token.Then)
        , P.token (P.Token "else" ExpectingKeyword)
            |> P.map (\() -> Keyword Token.Else)
        , word
            |> P.andThen (\first -> P.loop { reversedQualifiers = [], name = first } loopHelp)
        ]


newlinesParser : Parser_ ( List Int, Int )
newlinesParser =
    let
        eolParser =
            P.oneOf
                [ P.symbol (P.Token "\n\u{000D}" ExpectingNewline)
                , P.symbol (P.Token "\n" ExpectingNewline)
                ]
    in
    eolParser
        |> P.andThen
            (\() ->
                P.loop
                    []
                    (\reversed ->
                        P.succeed
                            (\spacesOnThisLine isThisLineEmpty ->
                                if isThisLineEmpty then
                                    P.Loop (spacesOnThisLine :: reversed)

                                else
                                    P.Done ( List.reverse reversed, spacesOnThisLine )
                            )
                            |= chompSpacesAndCount
                            |= P.oneOf
                                [ eolParser
                                    |> P.map (\() -> True)
                                , P.succeed False
                                ]
                    )
            )


chompSpacesAndCount : Parser_ Int
chompSpacesAndCount =
    P.chompWhile (\c -> c == ' ')
        |> P.getChompedString
        |> P.map String.length


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
                                (\() -> P.chompIf (\_ -> True) ExpectingAnything)
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
    let
        isValidNumericLiteralChar c =
            Char.isAlphaNum c || c == '_' || c == '.'
    in
    P.getChompedString
        (P.oneOf
            [ P.chompIf Char.isDigit ExpectingNumericLiteral
                |> P.andThen (\() -> P.chompWhile isValidNumericLiteralChar)
            , P.backtrackable
                (P.succeed ()
                    |. P.chompIf (\c -> c == '-') ExpectingNumericLiteral
                    |. P.chompIf Char.isDigit ExpectingNumericLiteral
                    |. P.chompWhile isValidNumericLiteralChar
                )
            ]
        )


sigilParser : Parser_ LexSigil
sigilParser =
    P.oneOf
        [ -- Two character sigils (must come first)
          P.symbol (P.Token "&&" ExpectingSigil)
            |> P.map (\() -> Operator Operator.And)
        , P.symbol (P.Token "++" ExpectingSigil)
            |> P.map (\() -> Operator Operator.Append)
        , P.symbol (P.Token "::" ExpectingSigil)
            |> P.map (\() -> Operator Operator.Cons)
        , P.symbol (P.Token "==" ExpectingSigil)
            |> P.map (\() -> Operator Operator.Equals)
        , P.symbol (P.Token "||" ExpectingSigil)
            |> P.map (\() -> Operator Operator.Or)
        , P.symbol (P.Token ".." ExpectingSigil)
            |> P.map (\() -> DoubleDot)
        , P.symbol (P.Token "->" ExpectingSigil)
            |> P.map (\() -> ThinArrow)
        , P.symbol (P.Token ">=" ExpectingSigil)
            |> P.map (\() -> Operator Operator.GreaterThanEquals)
        , P.symbol (P.Token "<=" ExpectingSigil)
            |> P.map (\() -> Operator Operator.LessThanEquals)

        -- Single character sigils
        , P.symbol (P.Token "^" ExpectingSigil)
            |> P.map (\() -> Operator Operator.Exponentiate)
        , P.symbol (P.Token "\\" ExpectingSigil)
            |> P.map (\() -> Backslash)
        , P.symbol (P.Token "_" ExpectingSigil)
            |> P.map (\() -> Underscore)
        , P.symbol (P.Token "(" ExpectingSigil)
            |> P.map (\() -> Bracket Round Open)
        , P.symbol (P.Token ")" ExpectingSigil)
            |> P.map (\() -> Bracket Round Close)
        , P.symbol (P.Token ">" ExpectingSigil)
            |> P.map (\() -> Operator Operator.GreaterThan)
        , P.symbol (P.Token "<" ExpectingSigil)
            |> P.map (\() -> Operator Operator.LessThan)
        , P.symbol (P.Token "-" ExpectingSigil)
            |> P.map (\() -> Operator Operator.Subtract)
        , P.symbol (P.Token "+" ExpectingSigil)
            |> P.map (\() -> Operator Operator.Add)
        , P.symbol (P.Token "=" ExpectingSigil)
            |> P.map (\() -> Assign)
        , P.symbol (P.Token "/" ExpectingSigil)
            |> P.map (\() -> Operator Operator.Divide)
        , P.symbol (P.Token "*" ExpectingSigil)
            |> P.map (\() -> Operator Operator.Multiply)
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
