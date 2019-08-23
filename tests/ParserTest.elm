module ParserTest exposing
    ( exposingList
    , expr
    , imports
    , moduleDeclaration
    , moduleName
    )

import AST.Common.Literal exposing (Literal(..))
import AST.Frontend as Frontend
import AST.Frontend.Unwrapped exposing (Expr(..))
import AssocList as Dict
import Data.Exposing exposing (ExposedItem(..), Exposing(..))
import Data.Module exposing (ModuleType(..))
import Data.ModuleName as ModuleName exposing (ModuleName)
import Data.VarName as VarName exposing (VarName)
import Error exposing (ParseContext, ParseProblem)
import Expect exposing (Expectation)
import Parser.Advanced as P
import Result.Extra
import Stage.Parse.Parser
import Test exposing (Test, describe, test)
import TestHelpers exposing (module_, var)


moduleDeclaration : Test
moduleDeclaration =
    let
        runTest ( description, input, output ) =
            test description <|
                \() ->
                    input
                        |> P.run Stage.Parse.Parser.moduleDeclaration
                        |> Result.toMaybe
                        |> Expect.equal output
    in
    describe "Stage.Parse.Parser.moduleDeclaration"
        [ describe "general"
            (List.map runTest
                [ ( "works with simple module name"
                  , "module Foo exposing (..)"
                  , Just ( PlainModule, module_ "Foo", ExposingAll )
                  )
                , ( "works with nested module name"
                  , "module Foo.Bar exposing (..)"
                  , Just ( PlainModule, module_ "Foo.Bar", ExposingAll )
                  )
                , ( "works with even more nested module name"
                  , "module Foo.Bar.Baz.Quux exposing (..)"
                  , Just ( PlainModule, module_ "Foo.Bar.Baz.Quux", ExposingAll )
                  )
                , ( "allows multiple spaces between the `module` keyword and the module name"
                  , "module  Foo exposing (..)"
                  , Just ( PlainModule, module_ "Foo", ExposingAll )
                  )
                , ( "allows multiple spaces between the module name and the `exposing` keyword"
                  , "module Foo  exposing (..)"
                  , Just ( PlainModule, module_ "Foo", ExposingAll )
                  )
                , ( "allows a newline between the module name and the `exposing` keyword"
                  , "module Foo\nexposing (..)"
                  , Just ( PlainModule, module_ "Foo", ExposingAll )
                  )
                , ( "allows multiple spaces between the `exposing` keyword and the exposing list"
                  , "module Foo exposing  (..)"
                  , Just ( PlainModule, module_ "Foo", ExposingAll )
                  )
                , ( "allows a newline between the `exposing` keyword and the exposing list"
                  , "module Foo exposing\n(..)"
                  , Just ( PlainModule, module_ "Foo", ExposingAll )
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
                  , Just ( PlainModule, module_ "Foo", ExposingAll )
                  )
                ]
            )
        , describe "port module"
            (List.map runTest
                [ ( "simply works"
                  , "port module Foo exposing (..)"
                  , Just ( PortModule, module_ "Foo", ExposingAll )
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
                        |> P.run Stage.Parse.Parser.exposingList
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
                                [ ExposedValue (VarName.fromString "foo")
                                , ExposedValue (VarName.fromString "bar")
                                ]
                            )
                      )
                    , ( "works with even more spaces between items"
                      , "(foo  ,  bar)"
                      , Just
                            (ExposingSome
                                [ ExposedValue (VarName.fromString "foo")
                                , ExposedValue (VarName.fromString "bar")
                                ]
                            )
                      )
                    , ( "works with mixed values"
                      , "(foo, Bar, Baz(..))"
                      , Just
                            (ExposingSome
                                [ ExposedValue (VarName.fromString "foo")
                                , ExposedType (VarName.fromString "Bar")
                                , ExposedTypeAndAllConstructors (VarName.fromString "Baz")
                                ]
                            )
                      )
                    , ( "allows for newline"
                      , "(foo\n,bar)"
                      , Just
                            (ExposingSome
                                [ ExposedValue (VarName.fromString "foo")
                                , ExposedValue (VarName.fromString "bar")
                                ]
                            )
                      )
                    ]
                )
            , describe "values"
                (List.map runTest
                    [ ( "works with a value"
                      , "(foo)"
                      , Just (ExposingSome [ ExposedValue (VarName.fromString "foo") ])
                      )
                    ]
                )
            , describe "types"
                (List.map runTest
                    [ ( "works with exposed type"
                      , "(Foo)"
                      , Just (ExposingSome [ ExposedType (VarName.fromString "Foo") ])
                      )
                    ]
                )
            , describe "types with all constructors"
                (List.map runTest
                    [ ( "works with exposed type and all constructors"
                      , "(Foo(..))"
                      , Just (ExposingSome [ ExposedTypeAndAllConstructors (VarName.fromString "Foo") ])
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
                        |> P.run Stage.Parse.Parser.imports
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
                            [ ( module_ "Foo"
                              , { moduleName = module_ "Foo"
                                , as_ = Just (module_ "F")
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
                            [ ( module_ "Foo"
                              , { moduleName = module_ "Foo"
                                , as_ = Just (module_ "F")
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
                            [ ( module_ "Foo"
                              , { moduleName = module_ "Foo"
                                , as_ = Nothing
                                , exposing_ = Nothing
                                }
                              )
                            , ( module_ "Bar"
                              , { moduleName = module_ "Bar"
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
                            [ ( module_ "Foo"
                              , { moduleName = module_ "Foo"
                                , as_ = Nothing
                                , exposing_ = Nothing
                                }
                              )
                            , ( module_ "Bar"
                              , { moduleName = module_ "Bar"
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
                            [ ( module_ "Foo"
                              , { moduleName = module_ "Foo"
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
                            [ ( module_ "Foo"
                              , { moduleName = module_ "Foo"
                                , as_ = Just (module_ "F")
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
                            [ ( module_ "Foo"
                              , { moduleName = module_ "Foo"
                                , as_ = Nothing
                                , exposing_ =
                                    Just
                                        (ExposingSome
                                            [ ExposedValue (VarName.fromString "bar")
                                            , ExposedType (VarName.fromString "Baz")
                                            , ExposedTypeAndAllConstructors (VarName.fromString "Quux")
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
                        |> P.run Stage.Parse.Parser.moduleName
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
                        |> P.run Stage.Parse.Parser.expr
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
                            { arguments = [ var "x" ]
                            , body =
                                Plus
                                    (Argument (var "x"))
                                    (Literal (Int 1))
                            }
                        )
                  )
                , ( "works with multiple arguments"
                  , "\\x y -> x + y"
                  , Just
                        (Lambda
                            { arguments =
                                [ var "x"
                                , var "y"
                                ]
                            , body =
                                Plus
                                    (Argument (var "x"))
                                    (Argument (var "y"))
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
                            { fn = Var { name = var "fn", qualifier = Nothing }
                            , argument = Literal (Int 1)
                            }
                        )
                  )
                , ( "with var"
                  , "fn arg"
                  , Just
                        (Call
                            { fn = Var { name = var "fn", qualifier = Nothing }
                            , argument = Var { name = var "arg", qualifier = Nothing }
                            }
                        )
                  )
                , ( "multiple"
                  , "fn arg1 arg2"
                  , Just
                        (Call
                            { fn =
                                Call
                                    { fn = Var { name = var "fn", qualifier = Nothing }
                                    , argument = Var { name = var "arg1", qualifier = Nothing }
                                    }
                            , argument = Var { name = var "arg2", qualifier = Nothing }
                            }
                        )
                  )
                , ( "space not needed if parenthesized arg"
                  , "fn(arg1)"
                  , Just
                        (Call
                            { fn = Var { name = var "fn", qualifier = Nothing }
                            , argument = Var { name = var "arg1", qualifier = Nothing }
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
                            { test = Literal (Int 1)
                            , then_ = Literal (Int 2)
                            , else_ = Literal (Int 3)
                            }
                        )
                  )
                , ( "with multiple spaces"
                  , "if   1   then   2   else   3"
                  , Just
                        (If
                            { test = Literal (Int 1)
                            , then_ = Literal (Int 2)
                            , else_ = Literal (Int 3)
                            }
                        )
                  )
                ]
              )
            , ( "literal int"
              , [ ( "positive"
                  , "123"
                  , Just (Literal (Int 123))
                  )
                , ( "zero"
                  , "0"
                  , Just (Literal (Int 0))
                  )
                , ( "negative zero"
                  , "-0"
                  , Just (Literal (Int (negate 0)))
                  )
                , ( "hexadecimal int"
                  , "0x123abc"
                  , Just (Literal (Int 1194684))
                  )
                , ( "hexadecimal int - uppercase"
                  , "0x789DEF"
                  , Just (Literal (Int 7904751))
                  )
                , ( "negative int"
                  , "-42"
                  , Just (Literal (Int -42))
                  )
                , ( "negative hexadecimal"
                  , "-0x123abc"
                  , Just (Literal (Int -1194684))
                  )
                ]
              )
            , ( "literal float"
              , [ ( "positive"
                  , "12.3"
                  , Just (Literal (Float 12.3))
                  )
                , ( "zero"
                  , "0.0"
                  , Just (Literal (Float 0))
                  )
                , ( "negative zero"
                  , "-0.0"
                  , Just (Literal (Float (negate 0)))
                  )
                , ( "negative float"
                  , "-4.2"
                  , Just (Literal (Float -4.2))
                  )
                , ( "Scientific notation"
                  , "5.12e2"
                  , Just (Literal (Float 512))
                  )
                , ( "Uppercase scientific notation"
                  , "5.12E2"
                  , Just (Literal (Float 512))
                  )
                , ( "Negative scientific notation"
                  , "-5.12e2"
                  , Just (Literal (Float -512))
                  )
                , ( "Negative exponent"
                  , "5e-2"
                  , Just (Literal (Float 0.05))
                  )
                ]
              )
            , ( "literal char"
              , [ ( "number"
                  , "'1'"
                  , Just (Literal (Char '1'))
                  )
                , ( "space"
                  , "' '"
                  , Just (Literal (Char ' '))
                  )
                , ( "newline shouldn't work"
                  , "'\n'"
                  , Nothing
                  )
                , ( "letter lowercase"
                  , "'a'"
                  , Just (Literal (Char 'a'))
                  )
                , ( "letter uppercase"
                  , "'A'"
                  , Just (Literal (Char 'A'))
                  )

                -- https://github.com/elm/compiler/blob/dcbe51fa22879f83b5d94642e117440cb5249bb1/compiler/src/Parse/String.hs#L279-L285
                , ( "escape backslash"
                  , singleQuote "\\\\"
                  , Just (Literal (Char '\\'))
                  )
                , ( "escape n"
                  , singleQuote "\\n"
                  , Just (Literal (Char '\n'))
                  )
                , ( "escape r"
                  , singleQuote "\\r"
                  , Just (Literal (Char '\u{000D}'))
                  )
                , ( "escape t"
                  , singleQuote "\\t"
                  , Just (Literal (Char '\t'))
                  )
                , ( "double quote"
                  , singleQuote "\""
                  , Just (Literal (Char '"'))
                    -- " (for vscode-elm bug)
                  )
                , ( "single quote"
                  , singleQuote "\\'"
                  , Just (Literal (Char '\''))
                  )
                , ( "emoji"
                  , singleQuote "😃"
                  , Just (Literal (Char '😃'))
                  )
                , ( "escaped unicode code point"
                  , singleQuote "\\u{1F648}"
                  , Just (Literal (Char '🙈'))
                  )
                ]
              )
            , ( "literal string"
              , [ ( "empty"
                  , doubleQuote ""
                  , Just (Literal (String ""))
                  )
                , ( "one space"
                  , doubleQuote " "
                  , Just (Literal (String " "))
                  )
                , ( "newline shouldn't work"
                  , doubleQuote "\n"
                  , Nothing
                  )
                , ( "two numbers"
                  , doubleQuote "42"
                  , Just (Literal (String "42"))
                  )
                , ( "single quote"
                  , doubleQuote "'"
                  , Just (Literal (String "'"))
                  )
                , ( "double quote"
                  , doubleQuote "\\\""
                  , Just (Literal (String "\""))
                  )
                , ( "escape backslash"
                  , doubleQuote "\\\\"
                  , Just (Literal (String "\\"))
                  )
                , ( "escape n"
                  , doubleQuote "\\n"
                  , Just (Literal (String "\n"))
                  )
                , ( "escape r"
                  , doubleQuote "\\r"
                  , Just (Literal (String "\u{000D}"))
                  )
                , ( "escape t"
                  , doubleQuote "\\t"
                  , Just (Literal (String "\t"))
                  )
                , ( "emoji"
                  , doubleQuote "😃"
                  , Just (Literal (String "😃"))
                  )
                , ( "escaped unicode code point"
                  , doubleQuote "\\u{1F648}"
                  , Just (Literal (String "🙈"))
                  )
                , ( "combo of escapes and chars"
                  , doubleQuote "\\u{1F648}\\n\\r\\t\\\\abc123"
                  , Just (Literal (String "🙈\n\u{000D}\t\\abc123"))
                  )
                ]
              )
            , ( "literal multiline string"
              , [ ( "empty"
                  , tripleQuote ""
                  , Just (Literal (String ""))
                  )
                , ( "one space"
                  , tripleQuote " "
                  , Just (Literal (String " "))
                  )
                , ( "newline"
                  , tripleQuote "\n"
                  , Just (Literal (String "\n"))
                  )
                , ( "two numbers"
                  , tripleQuote "42"
                  , Just (Literal (String "42"))
                  )
                , ( "single quote"
                  , tripleQuote "'"
                  , Just (Literal (String "'"))
                  )
                , ( "double quote"
                  , tripleQuote " \" "
                  , Just (Literal (String " \" "))
                  )
                , ( "escape backslash"
                  , tripleQuote "\\\\"
                  , Just (Literal (String "\\"))
                  )
                , ( "escape n"
                  , tripleQuote "\\n"
                  , Just (Literal (String "\n"))
                  )
                , ( "escape r"
                  , tripleQuote "\\r"
                  , Just (Literal (String "\u{000D}"))
                  )
                , ( "escape t"
                  , tripleQuote "\\t"
                  , Just (Literal (String "\t"))
                  )
                , ( "emoji"
                  , tripleQuote "😃"
                  , Just (Literal (String "😃"))
                  )
                , ( "escaped unicode code point"
                  , tripleQuote "\\u{1F648}"
                  , Just (Literal (String "🙈"))
                  )
                , ( "combo of escapes, newlines, and chars"
                  , tripleQuote "\\u{1F648}\\n\n\n\\r\\t\\\\abc123"
                  , Just (Literal (String "🙈\n\n\n\u{000D}\t\\abc123"))
                  )
                ]
              )
            , ( "literal bool"
              , [ ( "True"
                  , "True"
                  , Just (Literal (Bool True))
                  )
                , ( "False"
                  , "False"
                  , Just (Literal (Bool False))
                  )
                ]
              )
            , ( "let"
              , [ ( "one liner"
                  , "let x = 1 in 2"
                  , Just
                        (Let
                            { bindings =
                                [ { name = var "x"
                                  , body = Literal (Int 1)
                                  }
                                ]
                            , body = Literal (Int 2)
                            }
                        )
                  )
                , ( "one binding, generous whitespace"
                  , "let\n  x =\n      1\nin\n  2"
                  , Just
                        (Let
                            { bindings =
                                [ { name = var "x"
                                  , body = Literal (Int 1)
                                  }
                                ]
                            , body = Literal (Int 2)
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
                  , Just (List [ Literal (Int 1) ])
                  )
                , ( "single item in list with inner spaces"
                  , "[ 1 ]"
                  , Just (List [ Literal (Int 1) ])
                  )
                , ( "simple list"
                  , "[1,2,3]"
                  , Just
                        (List
                            [ Literal (Int 1)
                            , Literal (Int 2)
                            , Literal (Int 3)
                            ]
                        )
                  )
                , ( "simple list with inner spaces"
                  , "[ 1,  2  , 3 ]"
                  , Just
                        (List
                            [ Literal (Int 1)
                            , Literal (Int 2)
                            , Literal (Int 3)
                            ]
                        )
                  )
                , ( "list concat"
                  , "[] ++ []"
                  , Just (ListConcat (List []) (List []))
                  )
                , ( "list concat did not mess up the simple addition"
                  , "1 + 2"
                  , Just (Plus (Literal <| Int 1) (Literal <| Int 2))
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
                            (Literal (Int 1))
                            (Literal (Int 2))
                        )
                  )
                , ( "with inner spaces"
                  , "( 3 , 4 )"
                  , Just
                        (Tuple
                            (Literal (Int 3))
                            (Literal (Int 4))
                        )
                  )
                , ( "nested tuple"
                  , "(5,(6,7))"
                  , Just
                        (Tuple
                            (Literal (Int 5))
                            (Tuple (Literal (Int 6))
                                (Literal (Int 7))
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
                            (Literal (Int 1))
                            (Literal (Int 2))
                            (Literal (Int 3))
                        )
                  )
                , ( "with inner spaces"
                  , "( 4 , 5 , 6 )"
                  , Just
                        (Tuple3
                            (Literal (Int 4))
                            (Literal (Int 5))
                            (Literal (Int 6))
                        )
                  )
                ]
              )
            , ( "cons"
              , [ ( "simple case"
                  , "1 :: []"
                  , Just
                        (Cons
                            (Literal (Int 1))
                            (List [])
                        )
                  )
                , ( "multiple values case"
                  , "1 :: 2 :: []"
                  , Just
                        (Cons
                            (Literal (Int 1))
                            (Cons
                                (Literal (Int 2))
                                (List [])
                            )
                        )
                  )
                , ( "no spaces"
                  , "1::[]"
                  , Just
                        (Cons
                            (Literal (Int 1))
                            (List [])
                        )
                  )
                , ( "multiple spaces"
                  , "1    ::      []"
                  , Just
                        (Cons
                            (Literal (Int 1))
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
