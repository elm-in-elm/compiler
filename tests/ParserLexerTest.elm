module ParserLexerTest exposing (tests)

import Elm.Data.Located as Located exposing (Located)
import Expect
import Fuzz
import Parser.Advanced as P
import ParserLexerTestCases
import Stage.Parse.Contextualize as Contextualize
import Stage.Parse.Lexer as Lexer
import Test exposing (Test, describe, fuzz, test)


tests =
    describe "parser lexer test"
        [ describe "helpers"
            [ fuzz (Fuzz.list Fuzz.int) "collectList" <|
                \ls ->
                    ls
                        |> Contextualize.collectList Ok
                        |> Expect.equal (Ok ls)
            ]
        , describe "generated test cases"
            [ describe "lexing"
                ((ParserLexerTestCases.shouldParseTestCases ++ ParserLexerTestCases.shouldNotParseTestCases)
                    |> List.map
                        (\{ name, source, lexed } ->
                            test name <|
                                \() ->
                                    source
                                        |> P.run Lexer.parser
                                        |> Expect.equal (lexed |> Result.mapError never)
                        )
                )
            , describe "test cases are up to date"
                (ParserLexerTestCases.shouldNotParseTestCases
                    ++ ParserLexerTestCases.shouldParseTestCases
                    |> List.map
                        (\{ name, source, lexed, contextualized } ->
                            test name <|
                                \() ->
                                    case ( lexed, contextualized ) of
                                        ( Ok lexed_, Just contextualized_ ) ->
                                            lexed_
                                                |> List.map Located.unwrap
                                                |> Contextualize.run
                                                |> Expect.equal contextualized_

                                        ( Err _, _ ) ->
                                            Expect.fail "bug: cannot lex"

                                        ( Ok _, Nothing ) ->
                                            Expect.fail "bug: cannot lex"
                        )
                )
            , describe "should parse"
                (ParserLexerTestCases.shouldNotParseTestCases
                    |> List.map
                        (\{ name, source, lexed, contextualized } ->
                            test name <|
                                \() ->
                                    case ( lexed, contextualized ) of
                                        ( Ok lexed_, Just contextualized_ ) ->
                                            ()
                                                |> Expect.all
                                                    (lexed_
                                                        |> List.map Located.unwrap
                                                        |> Contextualize.run
                                                        |> List.map
                                                            (\rBlock () ->
                                                                case rBlock of
                                                                    Ok _ ->
                                                                        Expect.pass

                                                                    Err _ ->
                                                                        Expect.fail "it does not parse"
                                                            )
                                                    )

                                        ( Err _, _ ) ->
                                            Expect.fail "bug: cannot lex"

                                        ( Ok _, Nothing ) ->
                                            Expect.fail "bug: cannot lex"
                        )
                )
            , describe "should not parse"
                (ParserLexerTestCases.shouldNotParseTestCases
                    |> List.map
                        (\{ name, source, lexed, contextualized } ->
                            test name <|
                                \() ->
                                    case ( lexed, contextualized ) of
                                        ( Ok lexed_, Just contextualized_ ) ->
                                            ()
                                                |> Expect.all
                                                    (lexed_
                                                        |> List.map Located.unwrap
                                                        |> Contextualize.run
                                                        |> List.map
                                                            (\rBlock () ->
                                                                case rBlock of
                                                                    Ok _ ->
                                                                        Expect.fail "it did parse"

                                                                    Err _ ->
                                                                        Expect.pass
                                                            )
                                                    )

                                        ( Err _, _ ) ->
                                            Expect.fail "bug: cannot lex"

                                        ( Ok _, Nothing ) ->
                                            Expect.fail "bug: cannot lex"
                        )
                )
            ]
        ]
