module ParserTest exposing
    ( exposingList
    , expr
    , imports
    , moduleDeclaration
    , moduleName
    )

import Dict
import Elm.AST.Frontend as Frontend
import Elm.AST.Frontend.Unwrapped exposing (Expr(..))
import Elm.Compiler.Error exposing (ParseContext, ParseProblem)
import Elm.Compiler.Stage.Parse.Parser as Parser
import Elm.Data.Exposing exposing (ExposedItem(..), Exposing(..))
import Elm.Data.Module exposing (ModuleType(..))
import Elm.Data.ModuleName as ModuleName exposing (ModuleName)
import Elm.Data.VarName as VarName exposing (VarName)
import Expect exposing (Expectation)
import Parser.Advanced as P
import Result.Extra
import Test exposing (Test, describe, test)


moduleDeclaration : Test
moduleDeclaration =
    let
        runTest ( description, input, output ) =
            test description <|
                \() ->
                    input
                        |> P.run Parser.moduleDeclaration
                        |> Result.toMaybe
                        |> Expect.equal output
    in
    describe "Stage.Parse.Parser.moduleDeclaration"
        [ describe "general"
            (List.map runTest
                [ ( "works with simple module name"
                  , "module Foo exposing (..)"
                  , Just ( PlainModule, "Foo", ExposingAll )
                  )
                , ( "works with nested module name"
                  , "module Foo.Bar exposing (..)"
                  , Just ( PlainModule, "Foo.Bar", ExposingAll )
                  )
                , ( "works with even more nested module name"
                  , "module Foo.Bar.Baz.Quux exposing (..)"
                  , Just ( PlainModule, "Foo.Bar.Baz.Quux", ExposingAll )
                  )
                , ( "allows multiple spaces between the `module` keyword and the module name"
                  , "module  Foo exposing (..)"
                  , Just ( PlainModule, "Foo", ExposingAll )
                  )
                , ( "allows multiple spaces between the module name and the `exposing` keyword"
                  , "module Foo  exposing (..)"
                  , Just ( PlainModule, "Foo", ExposingAll )
                  )
                , ( "allows a newline between the module name and the `exposing` keyword"
                  , "module Foo\nexposing (..)"
                  , Just ( PlainModule, "Foo", ExposingAll )
                  )
                , ( "allows multiple spaces between the `exposing` keyword and the exposing list"
                  , "module Foo exposing  (..)"
                  , Just ( PlainModule, "Foo", ExposingAll )
                  )
                , ( "allows a newline between the `exposing` keyword and the exposing list"
                  , "module Foo exposing\n(..)"
                  , Just ( PlainModule, "Foo", ExposingAll )
                  )
                , ( "doesn't work without something after the `exposing` keyword"
                  , "module Foo exposing"
                  , Nothing
                  )
                ]
            )
        , describe "plain module"
            (List.map runTest
                [ ( "simply works"
                  , "module Foo exposing (..)"
                  , Just ( PlainModule, "Foo", ExposingAll )
                  )
                ]
            )
        , describe "port module"
            (List.map runTest
                [ ( "simply works"
                  , "port module Foo exposing (..)"
                  , Just ( PortModule, "Foo", ExposingAll )
                  )
                ]
            )
        ]


exposingList : Test
exposingList =
    let
        runTest ( description, input, output ) =
            test description <|
                \() ->
                    input
                        |> P.run Parser.exposingList
                        |> Result.toMaybe
                        |> Expect.equal output
    in
    describe "Stage.Parse.Parser.exposingList"
        [ describe "exposing all"
            (List.map runTest
                [ ( "simply works"
                  , "(..)"
                  , Just ExposingAll
                  )
                , ( "doesn't work with spaces inside the parens"
                  , "( .. )"
                  , Nothing
                  )
                ]
            )
        , describe "exposing some"
            [ describe "general"
                (List.map runTest
                    [ ( "can't be empty"
                      , "()"
                      , Nothing
                      )
                    , ( "works with spaces between items"
                      , "(foo, bar)"
                      , Just
                            (ExposingSome
                                [ ExposedValue "foo"
                                , ExposedValue "bar"
                                ]
                            )
                      )
                    , ( "works with even more spaces between items"
                      , "(foo  ,  bar)"
                      , Just
                            (ExposingSome
                                [ ExposedValue "foo"
                                , ExposedValue "bar"
                                ]
                            )
                      )
                    , ( "works with mixed values"
                      , "(foo, Bar, Baz(..))"
                      , Just
                            (ExposingSome
                                [ ExposedValue "foo"
                                , ExposedType "Bar"
                                , ExposedTypeAndAllConstructors "Baz"
                                ]
                            )
                      )
                    , ( "allows for newline"
                      , "(foo\n,bar)"
                      , Just
                            (ExposingSome
                                [ ExposedValue "foo"
                                , ExposedValue "bar"
                                ]
                            )
                      )
                    ]
                )
            , describe "values"
                (List.map runTest
                    [ ( "works with a value"
                      , "(foo)"
                      , Just (ExposingSome [ ExposedValue "foo" ])
                      )
                    ]
                )
            , describe "types"
                (List.map runTest
                    [ ( "works with exposed type"
                      , "(Foo)"
                      , Just (ExposingSome [ ExposedType "Foo" ])
                      )
                    ]
                )
            , describe "types with all constructors"
                (List.map runTest
                    [ ( "works with exposed type and all constructors"
                      , "(Foo(..))"
                      , Just (ExposingSome [ ExposedTypeAndAllConstructors "Foo" ])
                      )
                    , ( "doesn't allow spaces between the module name and the double period list"
                      , "(Foo (..))"
                      , Nothing
                      )
                    , ( "doesn't allow spaces inside the double period list"
                      , "(Foo( .. ))"
                      , Nothing
                      )
                    , ( "doesn't allow only some constructors exposed"
                      , "(Foo(Bar))"
                      , Nothing
                      )
                    ]
                )
            ]
        ]


imports : Test
imports =
    let
        runTest ( description, input, output ) =
            test description <|
                \() ->
                    input
                        |> P.run Parser.imports
                        |> Result.toMaybe
                        |> Expect.equal output
    in
    describe "Stage.Parse.Parser.imports"
        [ describe "general"
            (List.map runTest
                [ ( "allows for multiple modifiers"
                  , "import Foo as F exposing (..)"
                  , Just
                        (Dict.fromList
                            [ ( "Foo"
                              , { moduleName = "Foo"
                                , as_ = Just "F"
                                , exposing_ = Just ExposingAll
                                }
                              )
                            ]
                        )
                  )
                , ( "allows for multiple spaces"
                  , "import   Foo   as   F   exposing   (..)"
                  , Just
                        (Dict.fromList
                            [ ( "Foo"
                              , { moduleName = "Foo"
                                , as_ = Just "F"
                                , exposing_ = Just ExposingAll
                                }
                              )
                            ]
                        )
                  )
                , ( "allows for multiple imports"
                  , "import Foo\nimport Bar"
                  , Just
                        (Dict.fromList
                            [ ( "Foo"
                              , { moduleName = "Foo"
                                , as_ = Nothing
                                , exposing_ = Nothing
                                }
                              )
                            , ( "Bar"
                              , { moduleName = "Bar"
                                , as_ = Nothing
                                , exposing_ = Nothing
                                }
                              )
                            ]
                        )
                  )
                , ( "allows for multiple newlines between imports"
                  , "import Foo\n\nimport Bar"
                  , Just
                        (Dict.fromList
                            [ ( "Foo"
                              , { moduleName = "Foo"
                                , as_ = Nothing
                                , exposing_ = Nothing
                                }
                              )
                            , ( "Bar"
                              , { moduleName = "Bar"
                                , as_ = Nothing
                                , exposing_ = Nothing
                                }
                              )
                            ]
                        )
                  )
                , ( "doesn't allow for lower-case import"
                  , "import foo"
                  , Nothing
                  )
                ]
            )
        , describe "simple"
            (List.map runTest
                [ ( "simply works"
                  , "import Foo"
                  , Just
                        (Dict.fromList
                            [ ( "Foo"
                              , { moduleName = "Foo"
                                , as_ = Nothing
                                , exposing_ = Nothing
                                }
                              )
                            ]
                        )
                  )
                ]
            )
        , describe "as"
            (List.map runTest
                [ ( "simply works"
                  , "import Foo as F"
                  , Just
                        (Dict.fromList
                            [ ( "Foo"
                              , { moduleName = "Foo"
                                , as_ = Just "F"
                                , exposing_ = Nothing
                                }
                              )
                            ]
                        )
                  )
                , ( "doesn't work with lowercase alias"
                  , "import Foo as f"
                  , Nothing
                  )
                , ( "doesn't work with dot-separated alias"
                  , "import Foo as X.B"
                  , Nothing
                  )
                ]
            )
        , describe "exposing"
            (List.map runTest
                [ ( "simply works"
                  , "import Foo exposing (bar, Baz, Quux(..))"
                  , Just
                        (Dict.fromList
                            [ ( "Foo"
                              , { moduleName = "Foo"
                                , as_ = Nothing
                                , exposing_ =
                                    Just
                                        (ExposingSome
                                            [ ExposedValue "bar"
                                            , ExposedType "Baz"
                                            , ExposedTypeAndAllConstructors "Quux"
                                            ]
                                        )
                                }
                              )
                            ]
                        )
                  )
                , ( "doesn't work without something after the `exposing` keyword"
                  , "import Foo exposing"
                  , Nothing
                  )
                ]
            )
        ]


moduleName : Test
moduleName =
    let
        runTest ( description, input, output ) =
            test description <|
                \() ->
                    input
                        |> P.run Parser.moduleName
                        |> Result.toMaybe
                        |> Expect.equal output
    in
    describe "Stage.Parse.Parser.moduleName"
        (List.map runTest
            [ ( "works with simple module name"
              , "Foo"
              , Just "Foo"
              )
            , ( "doesn't work with lower-case name"
              , "foo"
              , Nothing
              )
            , ( "doesn't work with dot at the end"
              , "Foo."
              , Nothing
              )
            , ( "works with dotted module name"
              , "Foo.Bar"
              , Just "Foo.Bar"
              )
            , ( "works with doubly-dotted module name"
              , "Foo.Bar.Baz"
              , Just "Foo.Bar.Baz"
              )
            , ( "doesn't work with lower-case letter after the dot"
              , "Foo.bar"
              , Nothing
              )
            ]
        )


singleQuote : String -> String
singleQuote txt =
    "'" ++ txt ++ "'"


doubleQuote : String -> String
doubleQuote txt =
    "\"" ++ txt ++ "\""


tripleQuote : String -> String
tripleQuote txt =
    "\"\"\"" ++ txt ++ "\"\"\""


expr : Test
expr =
    let
        runSection ( description, tests ) =
            describe description
                (List.map runTest tests)

        runTest ( description, input, output ) =
            test description <|
                \() ->
                    input
                        |> P.run Parser.expr
                        |> Result.map Frontend.unwrap
                        |> expectEqualParseResult input output
    in
    describe "Stage.Parse.Parser.expr"
        (List.map runSection
            [ ( "lambda"
              , [ ( "works with single argument"
                  , "\\x -> x + 1"
                  , Just
                        (Lambda
                            { arguments = [ "x" ]
                            , body =
                                Plus
                                    (Argument "x")
                                    (Int 1)
                            }
                        )
                  )
                , ( "works with multiple arguments"
                  , "\\x y -> x + y"
                  , Just
                        (Lambda
                            { arguments =
                                [ "x"
                                , "y"
                                ]
                            , body =
                                Plus
                                    (Argument "x")
                                    (Argument "y")
                            }
                        )
                  )
                ]
              )
            , ( "call"
              , [ ( "simple"
                  , "fn 1"
                  , Just
                        (Call
                            { fn = Var { name = "fn", module_ = Nothing }
                            , argument = Int 1
                            }
                        )
                  )
                , ( "with var"
                  , "fn arg"
                  , Just
                        (Call
                            { fn = Var { name = "fn", module_ = Nothing }
                            , argument = Var { name = "arg", module_ = Nothing }
                            }
                        )
                  )
                , ( "multiple"
                  , "fn arg1 arg2"
                  , Just
                        (Call
                            { fn =
                                Call
                                    { fn = Var { name = "fn", module_ = Nothing }
                                    , argument = Var { name = "arg1", module_ = Nothing }
                                    }
                            , argument = Var { name = "arg2", module_ = Nothing }
                            }
                        )
                  )
                , ( "space not needed if parenthesized arg"
                  , "fn(arg1)"
                  , Just
                        (Call
                            { fn = Var { name = "fn", module_ = Nothing }
                            , argument = Var { name = "arg1", module_ = Nothing }
                            }
                        )
                  )
                ]
              )
            , ( "if"
              , [ ( "with one space"
                  , "if 1 then 2 else 3"
                  , Just
                        (If
                            { test = Int 1
                            , then_ = Int 2
                            , else_ = Int 3
                            }
                        )
                  )
                , ( "with multiple spaces"
                  , "if   1   then   2   else   3"
                  , Just
                        (If
                            { test = Int 1
                            , then_ = Int 2
                            , else_ = Int 3
                            }
                        )
                  )
                ]
              )
            , ( "literal int"
              , [ ( "positive"
                  , "123"
                  , Just (Int 123)
                  )
                , ( "zero"
                  , "0"
                  , Just (Int 0)
                  )
                , ( "negative zero"
                  , "-0"
                  , Just (Int (negate 0))
                  )
                , ( "hexadecimal int"
                  , "0x123abc"
                  , Just (Int 1194684)
                  )
                , ( "hexadecimal int - uppercase"
                  , "0x789DEF"
                  , Just (Int 7904751)
                  )
                , ( "negative int"
                  , "-42"
                  , Just (Int -42)
                  )
                , ( "negative hexadecimal"
                  , "-0x123abc"
                  , Just (Int -1194684)
                  )
                ]
              )
            , ( "literal float"
              , [ ( "positive"
                  , "12.3"
                  , Just (Float 12.3)
                  )
                , ( "zero"
                  , "0.0"
                  , Just (Float 0)
                  )
                , ( "negative zero"
                  , "-0.0"
                  , Just (Float (negate 0))
                  )
                , ( "negative float"
                  , "-4.2"
                  , Just (Float -4.2)
                  )
                , ( "Scientific notation"
                  , "5.12e2"
                  , Just (Float 512)
                  )
                , ( "Uppercase scientific notation"
                  , "5.12E2"
                  , Just (Float 512)
                  )
                , ( "Negative scientific notation"
                  , "-5.12e2"
                  , Just (Float -512)
                  )
                , ( "Negative exponent"
                  , "5e-2"
                  , Just (Float 0.05)
                  )
                ]
              )
            , ( "literal char"
              , [ ( "number"
                  , "'1'"
                  , Just (Char '1')
                  )
                , ( "space"
                  , "' '"
                  , Just (Char ' ')
                  )
                , ( "newline shouldn't work"
                  , "'\n'"
                  , Nothing
                  )
                , ( "letter lowercase"
                  , "'a'"
                  , Just (Char 'a')
                  )
                , ( "letter uppercase"
                  , "'A'"
                  , Just (Char 'A')
                  )

                -- https://github.com/elm/compiler/blob/dcbe51fa22879f83b5d94642e117440cb5249bb1/compiler/src/Parse/String.hs#L279-L285
                , ( "escape backslash"
                  , singleQuote "\\\\"
                  , Just (Char '\\')
                  )
                , ( "escape n"
                  , singleQuote "\\n"
                  , Just (Char '\n')
                  )
                , ( "escape r"
                  , singleQuote "\\r"
                  , Just (Char '\u{000D}')
                  )
                , ( "escape t"
                  , singleQuote "\\t"
                  , Just (Char '\t')
                  )
                , ( "double quote"
                  , singleQuote "\""
                  , Just (Char '"')
                    -- " (for vscode-elm bug)
                  )
                , ( "single quote"
                  , singleQuote "\\'"
                  , Just (Char '\'')
                  )
                , ( "emoji"
                  , singleQuote "😃"
                  , Just (Char '😃')
                  )
                , ( "escaped unicode code point"
                  , singleQuote "\\u{1F648}"
                  , Just (Char '🙈')
                  )
                ]
              )
            , ( "literal string"
              , [ ( "empty"
                  , doubleQuote ""
                  , Just (String "")
                  )
                , ( "one space"
                  , doubleQuote " "
                  , Just (String " ")
                  )
                , ( "newline shouldn't work"
                  , doubleQuote "\n"
                  , Nothing
                  )
                , ( "two numbers"
                  , doubleQuote "42"
                  , Just (String "42")
                  )
                , ( "single quote"
                  , doubleQuote "'"
                  , Just (String "'")
                  )
                , ( "double quote"
                  , doubleQuote "\\\""
                  , Just (String "\"")
                  )
                , ( "escape backslash"
                  , doubleQuote "\\\\"
                  , Just (String "\\")
                  )
                , ( "escape n"
                  , doubleQuote "\\n"
                  , Just (String "\n")
                  )
                , ( "escape r"
                  , doubleQuote "\\r"
                  , Just (String "\u{000D}")
                  )
                , ( "escape t"
                  , doubleQuote "\\t"
                  , Just (String "\t")
                  )
                , ( "emoji"
                  , doubleQuote "😃"
                  , Just (String "😃")
                  )
                , ( "escaped unicode code point"
                  , doubleQuote "\\u{1F648}"
                  , Just (String "🙈")
                  )
                , ( "combo of escapes and chars"
                  , doubleQuote "\\u{1F648}\\n\\r\\t\\\\abc123"
                  , Just (String "🙈\n\u{000D}\t\\abc123")
                  )
                ]
              )
            , ( "literal multiline string"
              , [ ( "empty"
                  , tripleQuote ""
                  , Just (String "")
                  )
                , ( "one space"
                  , tripleQuote " "
                  , Just (String " ")
                  )
                , ( "newline"
                  , tripleQuote "\n"
                  , Just (String "\n")
                  )
                , ( "two numbers"
                  , tripleQuote "42"
                  , Just (String "42")
                  )
                , ( "single quote"
                  , tripleQuote "'"
                  , Just (String "'")
                  )
                , ( "double quote"
                  , tripleQuote " \" "
                  , Just (String " \" ")
                  )
                , ( "escape backslash"
                  , tripleQuote "\\\\"
                  , Just (String "\\")
                  )
                , ( "escape n"
                  , tripleQuote "\\n"
                  , Just (String "\n")
                  )
                , ( "escape r"
                  , tripleQuote "\\r"
                  , Just (String "\u{000D}")
                  )
                , ( "escape t"
                  , tripleQuote "\\t"
                  , Just (String "\t")
                  )
                , ( "emoji"
                  , tripleQuote "😃"
                  , Just (String "😃")
                  )
                , ( "escaped unicode code point"
                  , tripleQuote "\\u{1F648}"
                  , Just (String "🙈")
                  )
                , ( "combo of escapes, newlines, and chars"
                  , tripleQuote "\\u{1F648}\\n\n\n\\r\\t\\\\abc123"
                  , Just (String "🙈\n\n\n\u{000D}\t\\abc123")
                  )
                ]
              )
            , ( "literal bool"
              , [ ( "True"
                  , "True"
                  , Just (Bool True)
                  )
                , ( "False"
                  , "False"
                  , Just (Bool False)
                  )
                ]
              )
            , ( "let"
              , [ ( "one liner"
                  , "let x = 1 in 2"
                  , Just
                        (Let
                            { bindings =
                                [ { name = "x"
                                  , body = Int 1
                                  }
                                ]
                            , body = Int 2
                            }
                        )
                  )
                , ( "one binding, generous whitespace"
                  , "let\n  x =\n      1\nin\n  2"
                  , Just
                        (Let
                            { bindings =
                                [ { name = "x"
                                  , body = Int 1
                                  }
                                ]
                            , body = Int 2
                            }
                        )
                  )
                ]
              )
            , ( "list"
              , [ ( "empty list"
                  , "[]"
                  , Just (List [])
                  )
                , ( "empty list with inner spaces"
                  , "[  ]"
                  , Just (List [])
                  )
                , ( "single item in list"
                  , "[1]"
                  , Just (List [ Int 1 ])
                  )
                , ( "single item in list with inner spaces"
                  , "[ 1 ]"
                  , Just (List [ Int 1 ])
                  )
                , ( "simple list"
                  , "[1,2,3]"
                  , Just
                        (List
                            [ Int 1
                            , Int 2
                            , Int 3
                            ]
                        )
                  )
                , ( "simple list with inner spaces"
                  , "[ 1,  2  , 3 ]"
                  , Just
                        (List
                            [ Int 1
                            , Int 2
                            , Int 3
                            ]
                        )
                  )
                , ( "list concat"
                  , "[] ++ []"
                  , Just (ListConcat (List []) (List []))
                  )
                , ( "list concat did not mess up the simple addition"
                  , "1 + 2"
                  , Just (Plus (Int 1) (Int 2))
                  )
                ]
              )
            , ( "unit"
              , [ ( "simple case"
                  , "()"
                  , Just Unit
                  )
                ]
              )
            , ( "tuple"
              , [ ( "without spaces"
                  , "(1,2)"
                  , Just
                        (Tuple
                            (Int 1)
                            (Int 2)
                        )
                  )
                , ( "with inner spaces"
                  , "( 3 , 4 )"
                  , Just
                        (Tuple
                            (Int 3)
                            (Int 4)
                        )
                  )
                , ( "nested tuple"
                  , "(5,(6,7))"
                  , Just
                        (Tuple
                            (Int 5)
                            (Tuple (Int 6)
                                (Int 7)
                            )
                        )
                  )
                ]
              )
            , ( "tuple3"
              , [ ( "without spaces"
                  , "(1,2,3)"
                  , Just
                        (Tuple3
                            (Int 1)
                            (Int 2)
                            (Int 3)
                        )
                  )
                , ( "with inner spaces"
                  , "( 4 , 5 , 6 )"
                  , Just
                        (Tuple3
                            (Int 4)
                            (Int 5)
                            (Int 6)
                        )
                  )
                ]
              )
            , ( "cons"
              , [ ( "simple case"
                  , "1 :: []"
                  , Just
                        (Cons
                            (Int 1)
                            (List [])
                        )
                  )
                , ( "multiple values case"
                  , "1 :: 2 :: []"
                  , Just
                        (Cons
                            (Int 1)
                            (Cons
                                (Int 2)
                                (List [])
                            )
                        )
                  )
                , ( "no spaces"
                  , "1::[]"
                  , Just
                        (Cons
                            (Int 1)
                            (List [])
                        )
                  )
                , ( "multiple spaces"
                  , "1    ::      []"
                  , Just
                        (Cons
                            (Int 1)
                            (List [])
                        )
                  )
                ]
              )
            ]
        )


expectEqualParseResult :
    String
    -> Maybe a
    -> Result (List (P.DeadEnd ParseContext ParseProblem)) a
    -> Expectation
expectEqualParseResult input expected actual =
    case ( actual, expected ) of
        ( Err _, Nothing ) ->
            Expect.pass

        ( Err deadEnds, Just _ ) ->
            Expect.fail
                (String.join "\n"
                    (input
                        :: "===>"
                        :: "Err"
                        :: List.map deadEndToString deadEnds
                    )
                )

        ( Ok actual_, Nothing ) ->
            Expect.fail
                (String.join "\n"
                    (input
                        :: "===> should have failed but parsed into ==>"
                        :: "Ok"
                        :: [ "    " ++ Debug.toString actual_ ]
                    )
                )

        ( Ok actual_, Just expected_ ) ->
            actual_
                |> Expect.equal expected_


deadEndToString : P.DeadEnd ParseContext ParseProblem -> String
deadEndToString deadEnd =
    let
        metadata =
            "("
                ++ String.fromInt (deadEnd.row - 1)
                ++ ","
                ++ String.fromInt (deadEnd.col - 1)
                ++ ") "
                ++ Debug.toString deadEnd.problem
    in
    String.join "\n    "
        ("\n"
            :: metadata
            :: "---- with context stack ----"
            :: List.map contextToString deadEnd.contextStack
        )


contextToString : { row : Int, col : Int, context : ParseContext } -> String
contextToString context =
    "("
        ++ String.fromInt (context.row - 1)
        ++ ","
        ++ String.fromInt (context.col - 1)
        ++ ") "
        ++ Debug.toString context.context
