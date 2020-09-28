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
            , describe "should parse"
                (ParserLexerTestCases.shouldParseTestCases
                    |> List.filterMap
                        (\{ name, source, lexed, contextualized } ->
                            Maybe.map2
                                (\contextualized_ lexed_ ->
                                    test name <|
                                        \() ->
                                            lexed_
                                                |> List.map Located.unwrap
                                                |> Contextualize.run
                                                |> Expect.equal contextualized_
                                )
                                contextualized
                                (Result.toMaybe lexed)
                        )
                )
            , describe "should not parse"
                (ParserLexerTestCases.shouldNotParseTestCases
                    |> List.filterMap
                        (\{ name, source, lexed, contextualized } ->
                            Maybe.map2
                                (\contextualized_ lexed_ ->
                                    test name <|
                                        \() ->
                                            lexed_
                                                |> List.map Located.unwrap
                                                |> Contextualize.run
                                                |> Expect.equal contextualized_
                                )
                                contextualized
                                (Result.toMaybe lexed)
                        )
                )
            ]
        ]
