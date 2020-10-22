module Stage.Parse.Lexer exposing (..)

import Elm.Data.Located as Located exposing (Located)
import Elm.Data.Operator as Operator exposing (Operator)
import Parser.Advanced as P exposing ((|.), (|=), Parser)
import Set
import Stage.Parse.Token as Token


type LexItem
    = Token LexToken
    | Ignorable LexIgnorable
    | Invalid LexInvalid
    | Newlines (List Int) Int


type LexToken
    = Sigil LexSigil
    | Identifier
        { qualifiers : List Token.UpperCase
        , name : Token.Token
        }
    | RecordAccessorLiteral Token.LowerCase
    | RecordAccessorFunction Token.LowerCase
    | Keyword Token.Keyword
    | NumericLiteral String
    | TextLiteral LexLiteralType String


type LexIgnorable
    = Whitespace Int
    | Comment LexCommentType String


type LexInvalid
    = IdentifierWithTrailingDot
        { qualifiers : List Token.UpperCase
        , name : Token.UpperCase
        }
    | IllegalCharacter Char
    | OtherInvalid String


type LexSigil
    = Bracket BracketType BracketRole
    | Assign
    | Pipe
    | Comma
    | DoubleDot
    | ThinArrow
    | Backslash
    | Underscore
    | Colon
    | Operator Operator


type LexCommentType
    = LineComment
    | MultilineComment
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
        Token (Sigil (Bracket Round Open)) ->
            "("

        Token (Sigil (Bracket Round Close)) ->
            ")"

        Token (Sigil (Bracket Square Open)) ->
            "["

        Token (Sigil (Bracket Square Close)) ->
            "]"

        Token (Sigil (Bracket Curly Open)) ->
            "{"

        Token (Sigil (Bracket Curly Close)) ->
            "}"

        Token (Sigil Assign) ->
            "="

        Token (Sigil Pipe) ->
            "|"

        Token (Sigil Comma) ->
            ","

        Token (Sigil DoubleDot) ->
            ".."

        Token (Sigil ThinArrow) ->
            "->"

        Token (Sigil Backslash) ->
            "\\"

        Token (Sigil Colon) ->
            ":"

        Token (Sigil Underscore) ->
            "_"

        Token (Sigil (Operator op)) ->
            Operator.toString op

        Token (Identifier { qualifiers, name }) ->
            (List.map (\(Token.UpperCase s) -> s) qualifiers ++ [ Token.tokenToString name ])
                |> String.join "."

        Token (RecordAccessorLiteral (Token.LowerCase name)) ->
            "." ++ name

        Token (RecordAccessorFunction (Token.LowerCase name)) ->
            "." ++ name

        Token (Keyword k) ->
            Token.keywordToString k

        Token (NumericLiteral s) ->
            s

        Token (TextLiteral ty s) ->
            delimiterFor ty ++ s ++ delimiterFor ty

        Ignorable (Whitespace i) ->
            String.repeat i " "

        Newlines empties indentationSpaces ->
            (empties
                |> List.map (\spacesInEmptyLine -> "\n" ++ String.repeat spacesInEmptyLine " ")
                |> String.join ""
            )
                ++ "\n"
                ++ String.repeat indentationSpaces " "

        Ignorable (Comment LineComment s) ->
            "//" ++ s

        Ignorable (Comment MultilineComment s) ->
            "{-" ++ s ++ "-}"

        Ignorable (Comment DocComment s) ->
            "{-|" ++ s ++ "-}"

        Invalid (IdentifierWithTrailingDot { qualifiers, name }) ->
            ((qualifiers ++ [ name ])
                |> List.map (\(Token.UpperCase s) -> s)
                |> String.join "."
            )
                ++ "."

        Invalid (IllegalCharacter c) ->
            String.fromChar c

        Invalid (OtherInvalid s) ->
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
                    ([ -- 1. commentParser must come before sigil parser as the sigil
                       --    parser will try to interpret "--" as two "-" sigils.
                       -- 2. sigilParser must come before recordAccessorLiteralParser so that ".." is parsed as the
                       --    DoubleDot sigil.
                       commentParser
                        |> P.map (\( ty, commentBody ) -> Comment ty commentBody |> Ignorable)
                     , sigilParser
                        |> P.map (Sigil >> Token)
                     , identifierParser
                     , recordAccessorParser (List.head reversed |> Maybe.map Located.unwrap)
                     , numericLiteralParser
                        |> P.map (NumericLiteral >> Token)
                     , textLiteralParser
                        |> P.map
                            (\( ty, terminates, literalBody ) ->
                                if terminates then
                                    TextLiteral ty literalBody |> Token

                                else
                                    (delimiterFor ty ++ literalBody)
                                        |> OtherInvalid
                                        |> Invalid
                            )
                     , P.symbol (P.Token " " ExpectingWhitespace)
                        |> P.andThen (\() -> chompSpacesAndCount)
                        |> P.map (\count -> (count + 1) |> Whitespace |> Ignorable)
                     , newlinesParser
                        |> P.map (\( emptyLines, indentation ) -> Newlines emptyLines indentation)
                     , P.getChompedString (P.chompIf (\_ -> True) ExpectingAnything)
                        |> P.map
                            (\s ->
                                case String.toList s of
                                    [ c ] ->
                                        c |> IllegalCharacter |> Invalid

                                    _ ->
                                        Debug.todo "ICE here"
                            )
                     ]
                        |> List.map (located >> P.map (\t -> P.Loop (t :: reversed)))
                    )
                , P.end ExpectingEnd
                    |> P.map (\() -> P.Done (List.reverse reversed))
                ]
        )


upperCaseWord : Parser_ Token.UpperCase
upperCaseWord =
    P.variable
        { start = Char.isUpper
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = Set.empty
        , expecting = ExpectingToken
        }
        |> P.map Token.UpperCase


lowerCaseWord : Parser_ Token.LowerCase
lowerCaseWord =
    P.variable
        { start = Char.isLower
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = Set.empty
        , expecting = ExpectingToken
        }
        |> P.map Token.LowerCase


identifierParser : Parser_ LexItem
identifierParser =
    let
        loopHelp { reversedQualifiers, name } =
            P.oneOf
                [ P.succeed (\x -> x)
                    |. P.symbol (P.Token "." ExpectingSigil)
                    |= P.oneOf
                        [ upperCaseWord
                            |> P.map
                                (\new ->
                                    P.Loop
                                        { reversedQualifiers = name :: reversedQualifiers
                                        , name = new
                                        }
                                )
                        , lowerCaseWord
                            |> P.map
                                (\new ->
                                    { qualifiers = List.reverse (name :: reversedQualifiers)
                                    , name = Token.TokenLowerCase new
                                    }
                                        |> Identifier
                                        |> Token
                                        |> P.Done
                                )
                        , { qualifiers = List.reverse reversedQualifiers
                          , name = name
                          }
                            |> IdentifierWithTrailingDot
                            |> Invalid
                            |> P.Done
                            |> P.succeed
                        ]
                , { qualifiers = List.reverse reversedQualifiers
                  , name = Token.TokenUpperCase name
                  }
                    |> Identifier
                    |> Token
                    |> P.Done
                    |> P.succeed
                ]
    in
    P.oneOf
        [ P.token (P.Token "module" ExpectingKeyword)
            |> P.map
                (\() ->
                    Token.Module
                        |> Keyword
                        |> Token
                )
        , P.token (P.Token "type" ExpectingKeyword)
            |> P.map
                (\() ->
                    Token.Type
                        |> Keyword
                        |> Token
                )
        , P.token (P.Token "alias" ExpectingKeyword)
            |> P.map
                (\() ->
                    Token.Alias
                        |> Keyword
                        |> Token
                )
        , P.token (P.Token "exposing" ExpectingKeyword)
            |> P.map
                (\() ->
                    Token.Exposing
                        |> Keyword
                        |> Token
                )
        , P.token (P.Token "case" ExpectingKeyword)
            |> P.map
                (\() ->
                    Token.Case
                        |> Keyword
                        |> Token
                )
        , P.token (P.Token "of" ExpectingKeyword)
            |> P.map
                (\() ->
                    Token.Of
                        |> Keyword
                        |> Token
                )
        , P.token (P.Token "if" ExpectingKeyword)
            |> P.map
                (\() ->
                    Token.If
                        |> Keyword
                        |> Token
                )
        , P.token (P.Token "then" ExpectingKeyword)
            |> P.map
                (\() ->
                    Token.Then
                        |> Keyword
                        |> Token
                )
        , P.token (P.Token "else" ExpectingKeyword)
            |> P.map
                (\() ->
                    Token.Else
                        |> Keyword
                        |> Token
                )
        , upperCaseWord
            |> P.andThen (\first -> P.loop { reversedQualifiers = [], name = first } loopHelp)
        , lowerCaseWord
            |> P.map
                (\name ->
                    { name = Token.TokenLowerCase name
                    , qualifiers = []
                    }
                        |> Identifier
                        |> Token
                )
        ]


recordAccessorParser : Maybe LexItem -> Parser_ LexItem
recordAccessorParser previous =
    P.succeed (\x -> x)
        |. P.symbol (P.Token "." ExpectingSigil)
        |= P.oneOf
            [ lowerCaseWord
                |> P.map
                    (case previous of
                        Just (Token (Sigil (Bracket Round Close))) ->
                            RecordAccessorLiteral >> Token

                        Just (Token (Sigil (Bracket Curly Close))) ->
                            RecordAccessorLiteral >> Token

                        Just (Token (Identifier _)) ->
                            RecordAccessorLiteral >> Token

                        _ ->
                            RecordAccessorFunction >> Token
                    )
            , P.succeed ('.' |> IllegalCharacter |> Invalid)
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
        [ -- order matters! We must try parsing a triple delimited string first!
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
        , P.symbol (P.Token "|" ExpectingSigil)
            |> P.map (\() -> Pipe)
        ]
