module TokenizeTest exposing (suite)

import Elm.Data.Token exposing (Token(..))
import Expect
import Stage.Tokenize
import Test exposing (Test)


suite : Test
suite =
    let
        runTest : ( String, String, Maybe (List Token) ) -> Test
        runTest ( description, input, output ) =
            Test.test description <|
                \() ->
                    input
                        |> Stage.Tokenize.tokenize
                        |> Result.toMaybe
                        |> Expect.equal output
    in
    Test.describe "Stage.Tokenize.tokenize"
        (List.map runTest
            [ ( "lowerName", "helloWorld", Just [ LowerName "helloWorld" ] )
            , ( "upperName", "HelloWorld", Just [ UpperName "HelloWorld" ] )
            , ( "int positive", "123", Just [ Int 123 ] )
            , ( "int negative", "-123", Just [ Int -123 ] )
            , ( "hex int positive", "0x123", Just [ Int 291 ] )
            , ( "hex int negative", "-0x123", Just [ Int -291 ] )
            , ( "float positive", "123.45", Just [ Float 123.45 ] )
            , ( "float negative", "-123.45", Just [ Float -123.45 ] )
            , ( "float positive scientific-positive", "1.2e3", Just [ Float 1200 ] )
            , ( "float negative scientific-positive", "-1.2e3", Just [ Float -1200 ] )
            , ( "float positive scientific-negative", "1.2e-3", Just [ Float 0.0012 ] )
            , ( "float negative scientific-negative", "-1.2e-3", Just [ Float -0.0012 ] )
            , ( "hex int edge case", "0x5e3", Just [ Int 1507 ] )
            , ( "char", "'a'", Just [ Char 'a' ] )
            , ( "multiple things in a char", "'ab'", Nothing )
            , ( "string", "\"a\"", Just [ String "a" ] )
            , ( "multiline string", "\"\"\"a\n  b\"\"\"", Just [ String "a\n  b" ] )
            , ( "operator +", "+", Just [ Operator "+" ] )
            , ( "operator ==", "==", Just [ Operator "==" ] )
            , ( "operator == with numbers", "1 == 2", Just [ Int 1, Operator "==", Int 2 ] )

            -- keywords
            , ( "port", "port", Just [ Port ] )
            , ( "effect", "effect", Just [ Effect ] )
            , ( "module", "module", Just [ Module ] )
            , ( "exposing", "exposing", Just [ Exposing ] )
            , ( "type", "type", Just [ Type ] )
            , ( "alias", "alias", Just [ Alias ] )
            , ( "import", "import", Just [ Import ] )
            , ( "if", "if", Just [ If ] )
            , ( "then", "then", Just [ Then ] )
            , ( "else", "else", Just [ Else ] )
            , ( "let", "let", Just [ Let ] )
            , ( "in", "in", Just [ In ] )
            , ( "case", "case", Just [ Case ] )
            , ( "of", "of", Just [ Of ] )

            -- symbols
            , ( "dot", ".", Just [ Dot ] )
            , ( "dot in a module name", "My.Module", Just [ UpperName "My", Dot, UpperName "Module" ] )
            , ( "all", "(..)", Just [ All ] )
            , ( "all next to module..exposing", "module Main exposing (..)", Just [ Module, UpperName "Main", Exposing, All ] )
            , ( "all next to import..exposing", "import Main exposing (..)", Just [ Import, UpperName "Main", Exposing, All ] )
            , ( "all next to import type..exposing", "import Main exposing (Foo(..))", Just [ Import, UpperName "Main", Exposing, LeftParen, UpperName "Foo", All, RightParen ] )
            , ( "comma", ",", Just [ Comma ] )
            , ( "comma in tuple", "(a,b)", Just [ LeftParen, LowerName "a", Comma, LowerName "b", RightParen ] )
            , ( "as", "as", Just [ As ] )
            , ( "equals", "=", Just [ Equals ] )
            , ( "equals in declaration", "foo = 1", Just [ LowerName "foo", Equals, Int 1 ] )
            , ( "minus", "-", Just [ Minus ] )
            , ( "minus with two numbers", "1 - 2", Just [ Int 1, Minus, Int 2 ] )
            , ( "backslash", "\\", Just [ Backslash ] )
            , ( "backslash in a lambda", "\\x -> x", Just [ Backslash, LowerName "x", RightArrow, LowerName "x" ] )
            , ( "left paren", "(", Just [ LeftParen ] )
            , ( "right paren", ")", Just [ RightParen ] )
            , ( "left and right paren in an unit", "()", Just [ LeftParen, RightParen ] )
            , ( "left curly bracket", "{", Just [ LeftCurlyBracket ] )
            , ( "right curly bracket", "}", Just [ RightCurlyBracket ] )
            , ( "curly brackets in empty record", "{}", Just [ LeftCurlyBracket, RightCurlyBracket ] )
            , ( "colon", ":", Just [ Colon ] )
            , ( "colon in type", "x : Int", Just [ LowerName "x", Colon, UpperName "Int" ] )
            , ( "pipe", "|", Just [ Pipe ] )
            , ( "pipe in custom type", "type X = A | B", Just [ Type, UpperName "X", Equals, UpperName "A", Pipe, UpperName "B" ] )
            , ( "underscore", "_", Just [ Underscore ] )
            , ( "underscore in lambda", "\\_ -> 1", Just [ Backslash, Underscore, RightArrow, Int 1 ] )
            ]
        )
