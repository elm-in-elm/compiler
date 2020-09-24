module ParserLexerTest exposing (tests)

import Elm.Data.Located as Located exposing (Located)
import Expect
import Parser.Advanced as P
import ParserLexerTestCases
import Stage.Parse.Contextualize as Contextualize
import Stage.Parse.Lexer as Lexer
import Test exposing (Test, describe, test)


tests =
    describe "parser lexer test cases"
        (ParserLexerTestCases.testCases
            |> List.map
                (\{ name, source, lexed, contextualized } ->
                    describe name
                        ([ Just
                            (test "lexing" <|
                                \() ->
                                    source
                                        |> P.run Lexer.parser
                                        |> Expect.equal lexed
                            )
                         , contextualized
                            |> Maybe.map
                                (\contextualized_ ->
                                    test "parsing" <|
                                        \() ->
                                            source
                                                |> P.run Lexer.parser
                                                |> Result.map (List.map Located.unwrap >> Contextualize.run)
                                                |> Expect.equal (Ok contextualized_)
                                )
                         ]
                            |> List.filterMap (\x -> x)
                        )
                )
        )
