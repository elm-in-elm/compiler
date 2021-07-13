module ParserTest exposing
    ( customTypeDeclaration
    , exposingList
    , expr
    , imports
    , moduleDeclaration
    , moduleName
    , portDeclaration
    , typeAliasDeclaration
    , type_
    , valueDeclaration
    )

import Dict
import Elm.AST.Frontend as Frontend
import Elm.AST.Frontend.Unwrapped exposing (Expr(..), Pattern(..))
import Elm.Compiler.Error exposing (ParseContext, ParseProblem)
import Elm.Data.Declaration as Declaration exposing (DeclarationBody)
import Elm.Data.Exposing exposing (ExposedItem(..), Exposing(..))
import Elm.Data.Module exposing (ModuleType(..))
import Elm.Data.Qualifiedness exposing (PossiblyQualified(..))
import Elm.Data.Type.Concrete as ConcreteType exposing (ConcreteType)
import Elm.Data.TypeAnnotation exposing (TypeAnnotation)
import Expect exposing (Expectation)
import OurExtras.String as String
import Parser.Advanced as P
import Stage.Parse.Parser
import Test exposing (Test, describe, test)


moduleDeclaration : Test
moduleDeclaration =
    let
        runTest ( description, input, output ) =
            test description <|
                \() ->
                    input
                        |> P.run Stage.Parse.Parser.moduleDeclaration
                        |> expectEqualParseResult input output
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
                , ( "doesn't allow a newline between the `module` keyword and the module name"
                  , """
                    module
                    Foo exposing (..)
                    """
                        |> String.multilineInput
                  , Nothing
                  )
                , ( "allows a newline and space between the `module` keyword and the module name"
                  , """
                    module
                     Foo exposing (..)
                    """
                        |> String.multilineInput
                  , Just ( PlainModule, "Foo", ExposingAll )
                  )
                , ( "allows multiple spaces between the module name and the `exposing` keyword"
                  , "module Foo  exposing (..)"
                  , Just ( PlainModule, "Foo", ExposingAll )
                  )
                , ( "doesn't allow a newline between the module name and the `exposing` keyword"
                  , """
                    module Foo
                    exposing (..)
                    """
                        |> String.multilineInput
                  , Nothing
                  )
                , ( "allows newline and space between the module name and the `exposing` keyword"
                  , """
                    module Foo
                     exposing (..)
                    """
                        |> String.multilineInput
                  , Just ( PlainModule, "Foo", ExposingAll )
                  )
                , ( "allows multiple spaces between the `exposing` keyword and the exposing list"
                  , "module Foo exposing  (..)"
                  , Just ( PlainModule, "Foo", ExposingAll )
                  )
                , ( "doesn't allow a newline between the `exposing` keyword and the exposing list"
                  , """
                    module Foo exposing
                    (..)
                    """
                        |> String.multilineInput
                  , Nothing
                  )
                , ( "allows a newline and space between the `exposing` keyword and the exposing list"
                  , """
                    module Foo exposing
                     (..)
                    """
                        |> String.multilineInput
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
                        |> P.run Stage.Parse.Parser.exposingList
                        |> expectEqualParseResult input output
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
                        |> P.run Stage.Parse.Parser.imports
                        |> expectEqualParseResult input output
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
                        |> P.run Stage.Parse.Parser.moduleName
                        |> expectEqualParseResult input output
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
                            { arguments = [ "x" ]
                            , body =
                                Plus
                                    (Argument "x")
                                    (Int 1)
                            }
                        )
                  )
                , ( "multiline"
                  , """
                    \\x ->
                        x + 1
                    """
                        |> String.multilineInput
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
                            { arguments = [ "x", "y" ]
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
                            { fn = Var { name = "fn", qualifiedness = PossiblyQualified Nothing }
                            , argument = Int 1
                            }
                        )
                  )
                , ( "with var"
                  , "fn arg"
                  , Just
                        (Call
                            { fn = Var { name = "fn", qualifiedness = PossiblyQualified Nothing }
                            , argument = Var { name = "arg", qualifiedness = PossiblyQualified Nothing }
                            }
                        )
                  )
                , ( "multiple"
                  , "fn arg1 arg2"
                  , Just
                        (Call
                            { fn =
                                Call
                                    { fn = Var { name = "fn", qualifiedness = PossiblyQualified Nothing }
                                    , argument = Var { name = "arg1", qualifiedness = PossiblyQualified Nothing }
                                    }
                            , argument = Var { name = "arg2", qualifiedness = PossiblyQualified Nothing }
                            }
                        )
                  )
                , ( "space not needed if parenthesized arg"
                  , "fn(arg1)"
                  , Just
                        (Call
                            { fn = Var { name = "fn", qualifiedness = PossiblyQualified Nothing }
                            , argument = Var { name = "arg1", qualifiedness = PossiblyQualified Nothing }
                            }
                        )
                  )
                , ( "multiline"
                  , """
                    fn
                        arg1
                        arg2
                    """
                        |> String.multilineInput
                  , Just
                        (Call
                            { fn =
                                Call
                                    { fn = Var { name = "fn", qualifiedness = PossiblyQualified Nothing }
                                    , argument = Var { name = "arg1", qualifiedness = PossiblyQualified Nothing }
                                    }
                            , argument = Var { name = "arg2", qualifiedness = PossiblyQualified Nothing }
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
                , ( "multiline"
                  , """
                    if 1 then
                        2

                    else
                        3

                    """
                        |> String.multilineInput
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
                  , Just (HexInt 1194684)
                  )
                , ( "hexadecimal int - uppercase"
                  , "0x789DEF"
                  , Just (HexInt 7904751)
                  )
                , ( "hexadecimal int - mixed case"
                  , "0x789dEf"
                  , Just (HexInt 7904751)
                  )
                , ( "negative int"
                  , "-42"
                  , Just (Int -42)
                  )
                , ( "negative hexadecimal"
                  , "-0x123abc"
                  , Just (HexInt -1194684)
                  )
                , ( "starting with zero disallowed"
                  , "0123"
                  , Nothing
                  )
                , ( "e is interpreted as hex 14, not scientific notation"
                  , "0xABCe5"
                  , Just (HexInt 703717)
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
                  , "5e2"
                  , Just (Float 500)
                  )
                , ( "Scientific notation with dot"
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
                , ( "Exponent with explicit plus sign"
                  , "5e+2"
                  , Just (Float 500)
                  )
                , ( "Uppercase E and exponent with explicit plus sign"
                  , "5E+2"
                  , Just (Float 500)
                  )
                , ( "Negative exponent"
                  , "5e-2"
                  , Just (Float 0.05)
                  )
                , ( "Zero - exhibit 1"
                  , "0.0e5"
                  , Just (Float 0)
                  )
                , ( "Zero - exhibit 2"
                  , "0e5"
                  , Nothing
                  )
                , ( "starting with dot disallowed"
                  , ".123"
                  , Nothing
                  )
                , ( "ending with dot disallowed"
                  , "123."
                  , {- TODO perhaps make another test suite especially for
                       `number` that also tests which cases return which errors
                    -}
                    Nothing
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
                  , """
                    let
                      x =
                          1
                    in
                      2
                    """
                        |> String.multilineInput
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
                , ( "doesn't allow bindings on the same indentation level as `let`"
                  , """
                    let
                    x = 1
                    in
                      2
                    """
                        |> String.multilineInput
                  , Nothing
                  )
                , ( "allows result expr on the same indentation level as `let`"
                  , """
                    let
                     x = 1
                    in
                    2
                    """
                        |> String.multilineInput
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
                , ( "multiple bindings"
                  , """
                    let
                      x = 1
                      y = 2
                    in
                    3
                    """
                        |> String.multilineInput
                  , Just
                        (Let
                            { bindings =
                                [ { name = "x"
                                  , body = Int 1
                                  }
                                , { name = "y"
                                  , body = Int 2
                                  }
                                ]
                            , body = Int 3
                            }
                        )
                  )
                , ( "doesn't allow bindings to have different indentation from each other"
                  , """
                    let
                      x = 1
                       y = 2
                    in
                      3
                    """
                        |> String.multilineInput
                  , Nothing
                  )
                , ( "doesn't allow bindings to have different indentation from each other - the other way"
                  , """
                    let
                       x = 1
                      y = 2
                    in
                      3
                    """
                        |> String.multilineInput
                  , Nothing
                  )
                , ( "one binding that's used in the body"
                  , """
                    let
                      x = 2
                    in
                      1 + x
                    """
                        |> String.multilineInput
                  , Just
                        (Let
                            { bindings =
                                [ { name = "x"
                                  , body = Int 2
                                  }
                                ]
                            , body =
                                Plus
                                    (Int 1)
                                    (Argument "x")
                            }
                        )
                  )
                , ( "two bindings where one is dependent on the other"
                  , """
                    let
                      x = 2
                      y = x + 1
                    in
                      42
                    """
                        |> String.multilineInput
                  , Just
                        (Let
                            { bindings =
                                [ { name = "x"
                                  , body = Int 2
                                  }
                                , { name = "y"
                                  , body =
                                        Plus
                                            (Argument "x")
                                            (Int 1)
                                  }
                                ]
                            , body = Int 42
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
                , ( "multiline"
                  , """
                    [ 1
                    , 2
                    , 3
                    ]
                    """
                        |> String.multilineInput
                  , Just
                        (List
                            [ Int 1
                            , Int 2
                            , Int 3
                            ]
                        )
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
            , ( "bin operator"
              , [ ( "combining binop with fn call - issue #75"
                  , "f a b c + 5"
                  , Just
                        (Plus
                            (Call
                                { fn =
                                    Call
                                        { fn =
                                            Call
                                                { fn = Var { name = "f", qualifiedness = PossiblyQualified Nothing }
                                                , argument = Var { name = "a", qualifiedness = PossiblyQualified Nothing }
                                                }
                                        , argument = Var { name = "b", qualifiedness = PossiblyQualified Nothing }
                                        }
                                , argument = Var { name = "c", qualifiedness = PossiblyQualified Nothing }
                                }
                            )
                            (Int 5)
                        )
                  )
                ]
              )
            , ( "record"
              , [ ( "empty record"
                  , "{}"
                  , Just (Record [])
                  )
                , ( "empty record with spaces"
                  , "{   }"
                  , Just (Record [])
                  )
                , ( "one field record"
                  , "{ a = 42 }"
                  , Just (Record [ { name = "a", body = Int 42 } ])
                  )
                , ( "one field record without spaces"
                  , "{a=42}"
                  , Just (Record [ { name = "a", body = Int 42 } ])
                  )
                , ( "two fields record"
                  , """{ a = 42, b = "hello" }"""
                  , Just
                        (Record
                            [ { name = "a", body = Int 42 }
                            , { name = "b", body = String "hello" }
                            ]
                        )
                  )
                , ( "multiline"
                  , """
                    { a = 42
                    , b = "hello" 
                    }
                    """
                  , Just
                        (Record
                            [ { name = "a", body = Int 42 }
                            , { name = "b", body = String "hello" }
                            ]
                        )
                  )
                ]
              )
            , ( "case"
              , [ ( "simple case"
                  , "case True of _->True"
                  , Just
                        (Case (Bool True)
                            [ { pattern = PAnything, body = Bool True }
                            ]
                        )
                  )
                , ( "multiline case"
                  , """
                    case 21 of
                        31 -> True
                        5 -> True
                        0xABC -> True
                        _ -> False
                    """
                        |> String.multilineInput
                  , Just
                        (Case (Int 21)
                            [ { pattern = PInt 31, body = Bool True }
                            , { pattern = PInt 5, body = Bool True }
                            , { pattern = PHexInt 2748, body = Bool True }
                            , { pattern = PAnything, body = Bool False }
                            ]
                        )
                  )
                , ( "complex case"
                  , """
                    case arg of
                        ('c', 23) ->
                            True
                        ("string") ->
                            True
                        ((arg1, arg2), 435.4) ->
                            False
                        [_, 45, (67.7)] ->
                            False
                        fst :: snd :: tail ->
                            False
                        ({ count } as alias1) as alias2 ->
                            False
                    """
                        |> String.multilineInput
                  , Just
                        (Case (Var { name = "arg", qualifiedness = PossiblyQualified Nothing })
                            [ { pattern = PTuple (PChar 'c') (PInt 23)
                              , body = Bool True
                              }
                            , { pattern = PString "string", body = Bool True }
                            , { pattern =
                                    PTuple
                                        (PTuple (PVar "arg1") (PVar "arg2"))
                                        (PFloat 435.4)
                              , body = Bool False
                              }
                            , { pattern = PList [ PAnything, PInt 45, PFloat 67.7 ]
                              , body = Bool False
                              }
                            , { pattern =
                                    PCons (PVar "fst")
                                        (PCons (PVar "snd") (PVar "tail"))
                              , body = Bool False
                              }
                            , { pattern =
                                    PAlias
                                        (PAlias (PRecord [ "count" ]) "alias1")
                                        "alias2"
                              , body = Bool False
                              }
                            ]
                        )
                  )
                ]
              )
            , ( "constructor values"
              , [ ( "Nothing"
                  , "Nothing"
                  , Just
                        (ConstructorValue
                            { qualifiedness = PossiblyQualified Nothing
                            , name = "Nothing"
                            }
                        )
                  )
                , ( "Just"
                  , "Just ()"
                  , Just
                        (Call
                            { fn =
                                ConstructorValue
                                    { qualifiedness = PossiblyQualified Nothing
                                    , name = "Just"
                                    }
                            , argument =
                                Unit
                            }
                        )
                  )
                , ( "qualified"
                  , "Maybe.Nothing"
                  , Just
                        (ConstructorValue
                            { qualifiedness = PossiblyQualified (Just "Maybe")
                            , name = "Nothing"
                            }
                        )
                  )
                , ( "More args"
                  , "Heap 1 2"
                  , Just
                        (Call
                            { fn =
                                Call
                                    { fn =
                                        ConstructorValue
                                            { qualifiedness = PossiblyQualified Nothing
                                            , name = "Heap"
                                            }
                                    , argument = Int 1
                                    }
                            , argument = Int 2
                            }
                        )
                  )
                , ( "Nested"
                  , "Heap (Just 3)"
                  , Just
                        (Call
                            { fn =
                                ConstructorValue
                                    { qualifiedness = PossiblyQualified Nothing
                                    , name = "Heap"
                                    }
                            , argument =
                                Call
                                    { fn =
                                        ConstructorValue
                                            { qualifiedness = PossiblyQualified Nothing
                                            , name = "Just"
                                            }
                                    , argument = Int 3
                                    }
                            }
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
                    [ input, "===> should have failed but parsed into ==>", "Ok", "    " ++ Debug.toString actual_ ]
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


type_ : Test
type_ =
    let
        runTest : ( String, String, ConcreteType PossiblyQualified ) -> Test
        runTest ( description, input, output ) =
            test description <|
                \() ->
                    input
                        |> P.run Stage.Parse.Parser.type_
                        |> expectEqualParseResult input (Just output)
    in
    describe "Stage.Parse.Parser.type_"
        (List.map runTest
            [ ( "int", "Int", ConcreteType.Int )
            , ( "unit", "()", ConcreteType.Unit )
            , ( "type var a", "a", ConcreteType.TypeVar "a" )
            , ( "function"
              , "Int -> ()"
              , ConcreteType.Function
                    { from = ConcreteType.Int
                    , to = ConcreteType.Unit
                    }
              )
            , ( "multiple-arg function"
              , "Int -> () -> Char"
              , ConcreteType.Function
                    { from = ConcreteType.Int
                    , to =
                        ConcreteType.Function
                            { from = ConcreteType.Unit
                            , to = ConcreteType.Char
                            }
                    }
              )
            , ( "float", "Float", ConcreteType.Float )
            , ( "char", "Char", ConcreteType.Char )
            , ( "string", "String", ConcreteType.String )
            , ( "bool", "Bool", ConcreteType.Bool )
            , ( "list", "List ()", ConcreteType.List ConcreteType.Unit )
            , ( "tuple"
              , "(Int, String)"
              , ConcreteType.Tuple
                    ConcreteType.Int
                    ConcreteType.String
              )
            , ( "tuple with different whitespace"
              , "( Int,String )"
              , ConcreteType.Tuple
                    ConcreteType.Int
                    ConcreteType.String
              )
            , ( "tuple with yet different whitespace"
              , "( Int ,  String )"
              , ConcreteType.Tuple
                    ConcreteType.Int
                    ConcreteType.String
              )
            , ( "tuple3"
              , "(Int, String, Bool)"
              , ConcreteType.Tuple3
                    ConcreteType.Int
                    ConcreteType.String
                    ConcreteType.Bool
              )
            , ( "custom type or alias"
              , "NonemptyList"
              , ConcreteType.UserDefinedType
                    { qualifiedness = PossiblyQualified Nothing
                    , name = "NonemptyList"
                    , args = []
                    }
              )
            , ( "parametric type"
              , "Maybe a"
              , ConcreteType.UserDefinedType
                    { qualifiedness = PossiblyQualified Nothing
                    , name = "Maybe"
                    , args = [ ConcreteType.TypeVar "a" ]
                    }
              )
            , ( "qualified custom type"
              , "Foo.Bar"
              , ConcreteType.UserDefinedType
                    { qualifiedness = PossiblyQualified (Just "Foo")
                    , name = "Bar"
                    , args = []
                    }
              )
            , ( "qualified custom type with params"
              , "Foo.Bar a Int"
              , ConcreteType.UserDefinedType
                    { qualifiedness = PossiblyQualified (Just "Foo")
                    , name = "Bar"
                    , args =
                        [ ConcreteType.TypeVar "a"
                        , ConcreteType.Int
                        ]
                    }
              )
            , ( "empty record"
              , "{}"
              , ConcreteType.Record Dict.empty
              )
            , ( "empty record with whitespace"
              , "{ }"
              , ConcreteType.Record Dict.empty
              )
            , ( "record with one field"
              , "{ x : Int }"
              , ConcreteType.Record <|
                    Dict.fromList
                        [ ( "x", ConcreteType.Int ) ]
              )
            , ( "record with two fields"
              , "{ x : Int, y : String }"
              , ConcreteType.Record <|
                    Dict.fromList
                        [ ( "x", ConcreteType.Int )
                        , ( "y", ConcreteType.String )
                        ]
              )
            , ( "multiline record"
              , """
                { x : Int
                , y : String 
                }
                """
                    |> String.multilineInput
              , ConcreteType.Record <|
                    Dict.fromList
                        [ ( "x", ConcreteType.Int )
                        , ( "y", ConcreteType.String )
                        ]
              )
            , ( "parenthesized type"
              , "(Int)"
              , ConcreteType.Int
              )
            , ( "parenthesized type with whitespace"
              , "( Int )"
              , ConcreteType.Int
              )
            ]
        )


valueDeclaration : Test
valueDeclaration =
    let
        runTest : ( String, String, Maybe ( String, DeclarationBody Expr TypeAnnotation PossiblyQualified ) ) -> Test
        runTest ( description, input, output ) =
            test description <|
                \() ->
                    input
                        |> P.run Stage.Parse.Parser.valueDeclaration
                        |> Result.map
                            (Tuple.mapSecond
                                (Declaration.mapBody
                                    Frontend.unwrap
                                    identity
                                    identity
                                )
                            )
                        |> expectEqualParseResult input output
    in
    -- TODO add various whitespace behavior tests
    describe "Stage.Parse.Parser.valueDeclaration" <|
        List.map runTest
            [ ( "simple without annotation"
              , "x = ()"
              , Just
                    ( "x"
                    , Declaration.Value
                        { expression = Unit
                        , typeAnnotation = Nothing
                        }
                    )
              )
            , ( "simple with annotation"
              , """
                y : ()
                y = ()
                """
                    |> String.multilineInput
              , Just
                    ( "y"
                    , Declaration.Value
                        { expression = Unit
                        , typeAnnotation =
                            Just
                                { varName = "y"
                                , type_ = ConcreteType.Unit
                                }
                        }
                    )
              )
            , ( "user defined type with argument - newline+space"
              , """
                x : Foo
                 ()
                x = ()
                """
                    |> String.multilineInput
              , Just
                    ( "x"
                    , Declaration.Value
                        { expression = Unit
                        , typeAnnotation =
                            Just
                                { varName = "x"
                                , type_ =
                                    ConcreteType.UserDefinedType
                                        { qualifiedness = PossiblyQualified Nothing
                                        , name = "Foo"
                                        , args = [ ConcreteType.Unit ]
                                        }
                                }
                        }
                    )
              )
            , ( "user defined type with argument - newline only"
              , """
                x : Foo
                ()
                x = ()
                """
                    |> String.multilineInput
              , Nothing
              )
            , ( "type on a newline+space"
              , """
                x :
                 ()
                x = ()
                """
                    |> String.multilineInput
              , Just
                    ( "x"
                    , Declaration.Value
                        { expression = Unit
                        , typeAnnotation =
                            Just
                                { varName = "x"
                                , type_ = ConcreteType.Unit
                                }
                        }
                    )
              )
            , ( "type on a newline without space"
              , """
                x :
                ()
                x = ()
                """
                    |> String.multilineInput
              , Nothing
              )
            ]


typeAliasDeclaration : Test
typeAliasDeclaration =
    let
        runTest : ( String, String, Maybe ( String, DeclarationBody Frontend.LocatedExpr TypeAnnotation PossiblyQualified ) ) -> Test
        runTest ( description, input, output ) =
            test description <|
                \() ->
                    input
                        |> P.run Stage.Parse.Parser.typeAliasDeclaration
                        |> expectEqualParseResult input output
    in
    describe "Stage.Parse.Parser.typeAliasDeclaration" <|
        List.map runTest <|
            [ ( "simple"
              , "type alias Foo = ()"
              , Just
                    ( "Foo"
                    , Declaration.TypeAlias
                        { parameters = []
                        , definition = ConcreteType.Unit
                        }
                    )
              )
            , ( "with params"
              , "type alias Bar a = ()"
              , Just
                    ( "Bar"
                    , Declaration.TypeAlias
                        { parameters = [ "a" ]
                        , definition = ConcreteType.Unit
                        }
                    )
              )
            , ( "a bit more advanced"
              , "type alias Foo = Maybe Int"
              , Just
                    ( "Foo"
                    , Declaration.TypeAlias
                        { parameters = []
                        , definition =
                            ConcreteType.UserDefinedType
                                { qualifiedness = PossiblyQualified Nothing
                                , name = "Maybe"
                                , args = [ ConcreteType.Int ]
                                }
                        }
                    )
              )
            , {- TODO create integration test that this fails
                 (`a` on right must be present on the left too)
              -}
              ( "to something that itself has params"
              , "type alias Foo = Maybe a"
              , Just
                    ( "Foo"
                    , Declaration.TypeAlias
                        { parameters = []
                        , definition =
                            ConcreteType.UserDefinedType
                                { qualifiedness = PossiblyQualified Nothing
                                , name = "Maybe"
                                , args = [ ConcreteType.TypeVar "a" ]
                                }
                        }
                    )
              )
            , ( "params on both sides"
              , "type alias Foo a = Maybe a"
              , Just
                    ( "Foo"
                    , Declaration.TypeAlias
                        { parameters = [ "a" ]
                        , definition =
                            ConcreteType.UserDefinedType
                                { qualifiedness = PossiblyQualified Nothing
                                , name = "Maybe"
                                , args = [ ConcreteType.TypeVar "a" ]
                                }
                        }
                    )
              )
            ]


customTypeDeclaration : Test
customTypeDeclaration =
    let
        runTest : ( String, String, Maybe ( String, DeclarationBody Frontend.LocatedExpr TypeAnnotation PossiblyQualified ) ) -> Test
        runTest ( description, input, output ) =
            test description <|
                \() ->
                    input
                        |> P.run Stage.Parse.Parser.customTypeDeclaration
                        |> expectEqualParseResult input output
    in
    describe "Stage.Parse.Parser.customTypeDeclaration" <|
        List.map runTest <|
            [ ( "simple"
              , "type Foo = Bar"
              , Just
                    ( "Foo"
                    , Declaration.CustomType
                        { parameters = []
                        , constructors =
                            ( { name = "Bar"
                              , arguments = []
                              }
                            , []
                            )
                        }
                    )
              )
            , ( "with params"
              , "type Bar a = Baz"
              , Just
                    ( "Bar"
                    , Declaration.CustomType
                        { parameters = [ "a" ]
                        , constructors =
                            ( { name = "Baz"
                              , arguments = []
                              }
                            , []
                            )
                        }
                    )
              )
            , ( "a bit more advanced"
              , "type Foo = Bar Int"
              , Just
                    ( "Foo"
                    , Declaration.CustomType
                        { parameters = []
                        , constructors =
                            ( { name = "Bar"
                              , arguments = [ ConcreteType.Int ]
                              }
                            , []
                            )
                        }
                    )
              )
            , {- TODO create integration test that this fails
                 (`a` on right must be present on the left too)
              -}
              ( "to something that itself has parameters"
              , "type Foo = Bar a"
              , Just
                    ( "Foo"
                    , Declaration.CustomType
                        { parameters = []
                        , constructors =
                            ( { name = "Bar"
                              , arguments = [ ConcreteType.TypeVar "a" ]
                              }
                            , []
                            )
                        }
                    )
              )
            , ( "params on both sides"
              , "type Foo a = Bar (Maybe a)"
              , Just
                    ( "Foo"
                    , Declaration.CustomType
                        { parameters = [ "a" ]
                        , constructors =
                            ( { name = "Bar"
                              , arguments =
                                    [ ConcreteType.UserDefinedType
                                        { qualifiedness = PossiblyQualified Nothing
                                        , name = "Maybe"
                                        , args = [ ConcreteType.TypeVar "a" ]
                                        }
                                    ]
                              }
                            , []
                            )
                        }
                    )
              )
            , ( "multiple constructors"
              , "type Foo = Bar | Baz"
              , Just
                    ( "Foo"
                    , Declaration.CustomType
                        { parameters = []
                        , constructors =
                            ( { name = "Bar"
                              , arguments = []
                              }
                            , [ { name = "Baz"
                                , arguments = []
                                }
                              ]
                            )
                        }
                    )
              )
            ]


portDeclaration : Test
portDeclaration =
    let
        runTest : ( String, String, Maybe ( String, DeclarationBody Frontend.LocatedExpr TypeAnnotation PossiblyQualified ) ) -> Test
        runTest ( description, input, output ) =
            test description <|
                \() ->
                    input
                        |> P.run Stage.Parse.Parser.portDeclaration
                        |> expectEqualParseResult input output

        outgoing : Maybe ( String, DeclarationBody Frontend.LocatedExpr TypeAnnotation PossiblyQualified )
        outgoing =
            Just
                ( "foo"
                , Declaration.Port
                    (ConcreteType.Function
                        { from = ConcreteType.String
                        , to =
                            ConcreteType.UserDefinedType
                                { qualifiedness = PossiblyQualified Nothing
                                , name = "Cmd"
                                , args = [ ConcreteType.TypeVar "msg" ]
                                }
                        }
                    )
                )
    in
    describe "Stage.Parse.Parser.portDeclaration" <|
        List.map runTest <|
            [ ( "outgoing"
              , "port foo : String -> Cmd msg"
              , outgoing
              )
            , ( "incoming"
              , "port bar : (String -> msg) -> Sub msg"
              , Just
                    ( "bar"
                    , Declaration.Port
                        (ConcreteType.Function
                            { from =
                                ConcreteType.Function
                                    { from = ConcreteType.String
                                    , to = ConcreteType.TypeVar "msg"
                                    }
                            , to =
                                ConcreteType.UserDefinedType
                                    { qualifiedness = PossiblyQualified Nothing
                                    , name = "Sub"
                                    , args = [ ConcreteType.TypeVar "msg" ]
                                    }
                            }
                        )
                    )
              )
            , ( "wacky multiline"
              , """
                port
                 foo
                 :
                 String -> Cmd msg
                """
                    |> String.multilineInput
              , outgoing
              )
            ]
