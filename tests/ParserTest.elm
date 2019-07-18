module ParserTest exposing
    ( dependencies
    , exposingList
    , expr
    , moduleDeclaration
    , moduleName
    )

import AST.Common.Literal exposing (Literal(..))
import AST.Common.Located as Located exposing (Located, located)
import AST.Frontend exposing (Expr(..), LocatedExpr)
import Common
import Common.Types
    exposing
        ( ExposedItem(..)
        , Exposing(..)
        , ModuleName(..)
        , ModuleType(..)
        , VarName(..)
        )
import Dict.Any
import Error exposing (ParseContext, ParseProblem)
import Expect exposing (Expectation)
import Parser.Advanced as P
import Result.Extra
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
                        |> Result.toMaybe
                        |> Expect.equal output
    in
    describe "Stage.Parse.Parser.moduleDeclaration"
        [ describe "general"
            (List.map runTest
                [ ( "works with simple module name"
                  , "module Foo exposing (..)"
                  , Just ( PlainModule, ModuleName "Foo", ExposingAll )
                  )
                , ( "works with nested module name"
                  , "module Foo.Bar exposing (..)"
                  , Just ( PlainModule, ModuleName "Foo.Bar", ExposingAll )
                  )
                , ( "works with even more nested module name"
                  , "module Foo.Bar.Baz.Quux exposing (..)"
                  , Just ( PlainModule, ModuleName "Foo.Bar.Baz.Quux", ExposingAll )
                  )
                , ( "allows multiple spaces between the `module` keyword and the module name"
                  , "module  Foo exposing (..)"
                  , Just ( PlainModule, ModuleName "Foo", ExposingAll )
                  )
                , ( "allows multiple spaces between the module name and the `exposing` keyword"
                  , "module Foo  exposing (..)"
                  , Just ( PlainModule, ModuleName "Foo", ExposingAll )
                  )
                , ( "allows a newline between the module name and the `exposing` keyword"
                  , "module Foo\nexposing (..)"
                  , Just ( PlainModule, ModuleName "Foo", ExposingAll )
                  )
                , ( "allows multiple spaces between the `exposing` keyword and the exposing list"
                  , "module Foo exposing  (..)"
                  , Just ( PlainModule, ModuleName "Foo", ExposingAll )
                  )
                , ( "allows a newline between the `exposing` keyword and the exposing list"
                  , "module Foo exposing\n(..)"
                  , Just ( PlainModule, ModuleName "Foo", ExposingAll )
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
                  , Just ( PlainModule, ModuleName "Foo", ExposingAll )
                  )
                ]
            )
        , describe "port module"
            (List.map runTest
                [ ( "simply works"
                  , "port module Foo exposing (..)"
                  , Just ( PortModule, ModuleName "Foo", ExposingAll )
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


dependencies : Test
dependencies =
    let
        runTest ( description, input, output ) =
            test description <|
                \() ->
                    input
                        |> P.run Stage.Parse.Parser.dependencies
                        |> Result.toMaybe
                        |> Expect.equal output
    in
    describe "Stage.Parse.Parser.dependencies"
        [ describe "general"
            (List.map runTest
                [ ( "allows for multiple modifiers"
                  , "import Foo as F exposing (..)"
                  , Just
                        (Dict.Any.fromList Common.moduleNameToString
                            [ ( ModuleName "Foo"
                              , { moduleName = ModuleName "Foo"
                                , as_ = Just (ModuleName "F")
                                , exposing_ = Just ExposingAll
                                }
                              )
                            ]
                        )
                  )
                , ( "allows for multiple spaces"
                  , "import   Foo   as   F   exposing   (..)"
                  , Just
                        (Dict.Any.fromList Common.moduleNameToString
                            [ ( ModuleName "Foo"
                              , { moduleName = ModuleName "Foo"
                                , as_ = Just (ModuleName "F")
                                , exposing_ = Just ExposingAll
                                }
                              )
                            ]
                        )
                  )
                , ( "allows for multiple imports"
                  , "import Foo\nimport Bar"
                  , Just
                        (Dict.Any.fromList Common.moduleNameToString
                            [ ( ModuleName "Foo"
                              , { moduleName = ModuleName "Foo"
                                , as_ = Nothing
                                , exposing_ = Nothing
                                }
                              )
                            , ( ModuleName "Bar"
                              , { moduleName = ModuleName "Bar"
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
                        (Dict.Any.fromList Common.moduleNameToString
                            [ ( ModuleName "Foo"
                              , { moduleName = ModuleName "Foo"
                                , as_ = Nothing
                                , exposing_ = Nothing
                                }
                              )
                            , ( ModuleName "Bar"
                              , { moduleName = ModuleName "Bar"
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
                        (Dict.Any.fromList Common.moduleNameToString
                            [ ( ModuleName "Foo"
                              , { moduleName = ModuleName "Foo"
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
                        (Dict.Any.fromList Common.moduleNameToString
                            [ ( ModuleName "Foo"
                              , { moduleName = ModuleName "Foo"
                                , as_ = Just (ModuleName "F")
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
                        (Dict.Any.fromList Common.moduleNameToString
                            [ ( ModuleName "Foo"
                              , { moduleName = ModuleName "Foo"
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
                        |> expectEqualParseResult input output
    in
    describe "Stage.Parse.Parser.expr"
        (List.map runSection
            [ ( "lambda"
              , [ ( "works with single argument"
                  , "\\x -> x + 1"
                  , Just (located { end = { col = 12, row = 1 }, start = { col = 1, row = 1 } } (Lambda { arguments = [ VarName "x" ], body = located { end = { col = 12, row = 1 }, start = { col = 7, row = 1 } } (Plus (located { end = { col = 8, row = 1 }, start = { col = 7, row = 1 } } (Argument (VarName "x"))) (located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Literal (Int 1)))) }))
                  )
                , ( "works with multiple arguments"
                  , "\\x y -> x + y"
                  , Just (located { end = { col = 14, row = 1 }, start = { col = 1, row = 1 } } (Lambda { arguments = [ VarName "x", VarName "y" ], body = located { end = { col = 14, row = 1 }, start = { col = 9, row = 1 } } (Plus (located { end = { col = 10, row = 1 }, start = { col = 9, row = 1 } } (Argument (VarName "x"))) (located { end = { col = 14, row = 1 }, start = { col = 13, row = 1 } } (Argument (VarName "y")))) }))
                  )
                ]
              )
            , ( "call"
              , [ ( "simple"
                  , "fn 1"
                  , Just (located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Call { argument = located { end = { col = 5, row = 1 }, start = { col = 4, row = 1 } } (Literal (Int 1)), fn = located { end = { col = 3, row = 1 }, start = { col = 1, row = 1 } } (Var { name = VarName "fn", qualifier = Nothing }) }))
                  )
                , ( "with var"
                  , "fn arg"
                  , Just (located { end = { col = 7, row = 1 }, start = { col = 1, row = 1 } } (Call { argument = located { end = { col = 7, row = 1 }, start = { col = 4, row = 1 } } (Var { name = VarName "arg", qualifier = Nothing }), fn = located { end = { col = 3, row = 1 }, start = { col = 1, row = 1 } } (Var { name = VarName "fn", qualifier = Nothing }) }))
                  )
                , ( "multiple"
                  , "fn arg1 arg2"
                  , Just (located { end = { col = 13, row = 1 }, start = { col = 1, row = 1 } } (Call { argument = located { end = { col = 13, row = 1 }, start = { col = 9, row = 1 } } (Var { name = VarName "arg2", qualifier = Nothing }), fn = located { end = { col = 8, row = 1 }, start = { col = 1, row = 1 } } (Call { argument = located { end = { col = 8, row = 1 }, start = { col = 4, row = 1 } } (Var { name = VarName "arg1", qualifier = Nothing }), fn = located { end = { col = 3, row = 1 }, start = { col = 1, row = 1 } } (Var { name = VarName "fn", qualifier = Nothing }) }) }))
                  )
                , ( "space not needed if parenthesized arg"
                  , "fn(arg1)"
                  , Just (located { end = { col = 8, row = 1 }, start = { col = 1, row = 1 } } (Call { argument = located { end = { col = 8, row = 1 }, start = { col = 4, row = 1 } } (Var { name = VarName "arg1", qualifier = Nothing }), fn = located { end = { col = 3, row = 1 }, start = { col = 1, row = 1 } } (Var { name = VarName "fn", qualifier = Nothing }) }))
                  )
                ]
              )
            , ( "if"
              , [ ( "with one space"
                  , "if 1 then 2 else 3"
                  , Just (located { end = { col = 19, row = 1 }, start = { col = 1, row = 1 } } (If { else_ = located { end = { col = 19, row = 1 }, start = { col = 18, row = 1 } } (Literal (Int 3)), test = located { end = { col = 5, row = 1 }, start = { col = 4, row = 1 } } (Literal (Int 1)), then_ = located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Literal (Int 2)) }))
                  )
                , ( "with multiple spaces"
                  , "if   1   then   2   else   3"
                  , Just (located { end = { col = 29, row = 1 }, start = { col = 1, row = 1 } } (If { else_ = located { end = { col = 29, row = 1 }, start = { col = 28, row = 1 } } (Literal (Int 3)), test = located { end = { col = 7, row = 1 }, start = { col = 6, row = 1 } } (Literal (Int 1)), then_ = located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Literal (Int 2)) }))
                  )
                ]
              )
            , ( "literal int"
              , [ ( "positive"
                  , "123"
                  , Just (located { end = { col = 4, row = 1 }, start = { col = 1, row = 1 } } (Literal (Int 123)))
                  )
                , ( "zero"
                  , "0"
                  , Just (located { end = { col = 2, row = 1 }, start = { col = 1, row = 1 } } (Literal (Int 0)))
                  )
                , ( "hexadecimal int"
                  , "0x123abc"
                  , Just (located { end = { col = 9, row = 1 }, start = { col = 1, row = 1 } } (Literal (Int 1194684)))
                  )
                , ( "hexadecimal int - uppercase"
                  , "0x789DEF"
                  , Just (located { end = { col = 9, row = 1 }, start = { col = 1, row = 1 } } (Literal (Int 7904751)))
                  )
                , ( "negative int"
                  , "-42"
                  , Just (located { end = { col = 4, row = 1 }, start = { col = 1, row = 1 } } (Literal (Int -42)))
                  )
                , ( "negative hexadecimal"
                  , "-0x123abc"
                  , Just (located { end = { col = 10, row = 1 }, start = { col = 1, row = 1 } } (Literal (Int -1194684)))
                  )
                ]
              )
            , ( "literal float"
              , [ ( "positive"
                  , "12.3"
                  , Just (located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Literal (Float 12.3)))
                  )
                , ( "zero"
                  , "0.0"
                  , Just (located { end = { col = 4, row = 1 }, start = { col = 1, row = 1 } } (Literal (Float 0)))
                  )
                , ( "negative float"
                  , "-4.2"
                  , Just (located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Literal (Float -4.2)))
                  )
                , ( "Scientific notation"
                  , "5.12e2"
                  , Just (located { end = { col = 7, row = 1 }, start = { col = 1, row = 1 } } (Literal (Float 512)))
                  )
                , ( "Uppercase scientific notation"
                  , "5.12E2"
                  , Just (located { end = { col = 7, row = 1 }, start = { col = 1, row = 1 } } (Literal (Float 512)))
                  )
                , ( "Negative scientific notation"
                  , "-5.12e2"
                  , Just (located { end = { col = 8, row = 1 }, start = { col = 1, row = 1 } } (Literal (Float -512)))
                  )
                , ( "Negative exponent"
                  , "5e-2"
                  , Just (located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Literal (Float 0.05)))
                  )
                ]
              )
            , ( "literal char"
              , [ ( "number"
                  , "'1'"
                  , Just (located { end = { col = 4, row = 1 }, start = { col = 1, row = 1 } } (Literal (Char '1')))
                  )
                , ( "space"
                  , "' '"
                  , Just (located { end = { col = 4, row = 1 }, start = { col = 1, row = 1 } } (Literal (Char ' ')))
                  )
                , ( "newline shouldn't work"
                  , "'\n'"
                  , Nothing
                  )
                , ( "letter lowercase"
                  , "'a'"
                  , Just (located { end = { col = 4, row = 1 }, start = { col = 1, row = 1 } } (Literal (Char 'a')))
                  )
                , ( "letter uppercase"
                  , "'A'"
                  , Just (located { end = { col = 4, row = 1 }, start = { col = 1, row = 1 } } (Literal (Char 'A')))
                  )

                -- https://github.com/elm/compiler/blob/dcbe51fa22879f83b5d94642e117440cb5249bb1/compiler/src/Parse/String.hs#L279-L285
                , ( "escape backslash"
                  , singleQuote "\\\\"
                  , Just (located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Literal (Char '\\')))
                  )
                , ( "escape n"
                  , singleQuote "\\n"
                  , Just (located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Literal (Char '\n')))
                  )
                , ( "escape r"
                  , singleQuote "\\r"
                  , Just (located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Literal (Char '\u{000D}')))
                  )
                , ( "escape t"
                  , singleQuote "\\t"
                  , Just (located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Literal (Char '\t')))
                  )
                , ( "double quote"
                  , singleQuote "\""
                  , Just (located { end = { col = 4, row = 1 }, start = { col = 1, row = 1 } } (Literal (Char '"')))
                    -- " (for vscode-elm bug)
                  )
                , ( "single quote"
                  , singleQuote "\\'"
                  , Just (located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Literal (Char '\'')))
                  )
                , ( "emoji"
                  , singleQuote "😃"
                  , Just (located { end = { col = 4, row = 1 }, start = { col = 1, row = 1 } } (Literal (Char '😃')))
                  )
                , ( "escaped unicode code point"
                  , singleQuote "\\u{1F648}"
                  , Just (located { end = { col = 12, row = 1 }, start = { col = 1, row = 1 } } (Literal (Char '🙈')))
                  )
                ]
              )
            , ( "literal string"
              , [ ( "empty"
                  , doubleQuote ""
                  , Just (located { end = { col = 3, row = 1 }, start = { col = 1, row = 1 } } (Literal (String "")))
                  )
                , ( "one space"
                  , doubleQuote " "
                  , Just (located { end = { col = 4, row = 1 }, start = { col = 1, row = 1 } } (Literal (String " ")))
                  )
                , ( "newline shouldn't work"
                  , doubleQuote "\n"
                  , Nothing
                  )
                , ( "two numbers"
                  , doubleQuote "42"
                  , Just (located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Literal (String "42")))
                  )
                , ( "single quote"
                  , doubleQuote "'"
                  , Just (located { end = { col = 4, row = 1 }, start = { col = 1, row = 1 } } (Literal (String "'")))
                  )
                , ( "double quote"
                  , doubleQuote "\\\""
                  , Just (located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Literal (String "\"")))
                  )
                , ( "escape backslash"
                  , doubleQuote "\\\\"
                  , Just (located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Literal (String "\\")))
                  )
                , ( "escape n"
                  , doubleQuote "\\n"
                  , Just (located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Literal (String "\n")))
                  )
                , ( "escape r"
                  , doubleQuote "\\r"
                  , Just (located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Literal (String "\u{000D}")))
                  )
                , ( "escape t"
                  , doubleQuote "\\t"
                  , Just (located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Literal (String "\t")))
                  )
                , ( "emoji"
                  , doubleQuote "😃"
                  , Just (located { end = { col = 4, row = 1 }, start = { col = 1, row = 1 } } (Literal (String "😃")))
                  )
                , ( "escaped unicode code point"
                  , doubleQuote "\\u{1F648}"
                  , Just (located { end = { col = 12, row = 1 }, start = { col = 1, row = 1 } } (Literal (String "🙈")))
                  )
                , ( "combo of escapes and chars"
                  , doubleQuote "\\u{1F648}\\n\\r\\t\\\\abc123"
                  , Just (located { end = { col = 26, row = 1 }, start = { col = 1, row = 1 } } (Literal (String "🙈\n\u{000D}\t\\abc123")))
                  )
                ]
              )
            , ( "literal multiline string"
              , [ ( "empty"
                  , tripleQuote ""
                  , Just (located { end = { col = 7, row = 1 }, start = { col = 1, row = 1 } } (Literal (String "")))
                  )
                , ( "one space"
                  , tripleQuote " "
                  , Just (located { end = { col = 8, row = 1 }, start = { col = 1, row = 1 } } (Literal (String " ")))
                  )
                , ( "newline"
                  , tripleQuote "\n"
                  , Just (located { end = { col = 4, row = 2 }, start = { col = 1, row = 1 } } (Literal (String "\n")))
                  )
                , ( "two numbers"
                  , tripleQuote "42"
                  , Just (located { end = { col = 9, row = 1 }, start = { col = 1, row = 1 } } (Literal (String "42")))
                  )
                , ( "single quote"
                  , tripleQuote "'"
                  , Just (located { end = { col = 8, row = 1 }, start = { col = 1, row = 1 } } (Literal (String "'")))
                  )
                , ( "double quote"
                  , tripleQuote " \" "
                  , Just (located { end = { col = 10, row = 1 }, start = { col = 1, row = 1 } } (Literal (String " \" ")))
                  )
                , ( "escape backslash"
                  , tripleQuote "\\\\"
                  , Just (located { end = { col = 9, row = 1 }, start = { col = 1, row = 1 } } (Literal (String "\\")))
                  )
                , ( "escape n"
                  , tripleQuote "\\n"
                  , Just (located { end = { col = 9, row = 1 }, start = { col = 1, row = 1 } } (Literal (String "\n")))
                  )
                , ( "escape r"
                  , tripleQuote "\\r"
                  , Just (located { end = { col = 9, row = 1 }, start = { col = 1, row = 1 } } (Literal (String "\u{000D}")))
                  )
                , ( "escape t"
                  , tripleQuote "\\t"
                  , Just (located { end = { col = 9, row = 1 }, start = { col = 1, row = 1 } } (Literal (String "\t")))
                  )
                , ( "emoji"
                  , tripleQuote "😃"
                  , Just (located { end = { col = 8, row = 1 }, start = { col = 1, row = 1 } } (Literal (String "😃")))
                  )
                , ( "escaped unicode code point"
                  , tripleQuote "\\u{1F648}"
                  , Just (located { end = { col = 16, row = 1 }, start = { col = 1, row = 1 } } (Literal (String "🙈")))
                  )
                , ( "combo of escapes, newlines, and chars"
                  , tripleQuote "\\u{1F648}\\n\n\n\\r\\t\\\\abc123"
                  , Just (located { end = { col = 16, row = 3 }, start = { col = 1, row = 1 } } (Literal (String "🙈\n\n\n\u{000D}\t\\abc123")))
                  )
                ]
              )
            , ( "literal bool"
              , [ ( "True"
                  , "True"
                  , Just (located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Literal (Bool True)))
                  )
                , ( "False"
                  , "False"
                  , Just (located { end = { col = 6, row = 1 }, start = { col = 1, row = 1 } } (Literal (Bool False)))
                  )
                ]
              )
            , ( "let"
              , [ ( "one liner"
                  , "let x = 1 in 2"
                  , Just (located { end = { col = 15, row = 1 }, start = { col = 1, row = 1 } } (Let { bindings = [ { body = located { end = { col = 10, row = 1 }, start = { col = 9, row = 1 } } (Literal (Int 1)), name = VarName "x" } ], body = located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Literal (Int 2)) }))
                  )
                , ( "one binding, generous whitespace"
                  , "let\n  x =\n      1\nin\n  2"
                  , Just (located { end = { col = 4, row = 5 }, start = { col = 1, row = 1 } } (Let { bindings = [ { body = located { end = { col = 8, row = 3 }, start = { col = 7, row = 3 } } (Literal (Int 1)), name = VarName "x" } ], body = located { end = { col = 4, row = 5 }, start = { col = 3, row = 5 } } (Literal (Int 2)) }))
                  )
                ]
              )
            , ( "list"
              , [ ( "empty list"
                  , "[]"
                  , Just (located { end = { col = 3, row = 1 }, start = { col = 1, row = 1 } } (List []))
                  )
                , ( "empty list with inner spaces"
                  , "[  ]"
                  , Just (located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (List []))
                  )
                , ( "single item in list"
                  , "[1]"
                  , Just (located { end = { col = 4, row = 1 }, start = { col = 1, row = 1 } } (List [ located { end = { col = 3, row = 1 }, start = { col = 2, row = 1 } } (Literal (Int 1)) ]))
                  )
                , ( "single item in list with inner spaces"
                  , "[ 1 ]"
                  , Just (located { end = { col = 6, row = 1 }, start = { col = 1, row = 1 } } (List [ located { end = { col = 4, row = 1 }, start = { col = 3, row = 1 } } (Literal (Int 1)) ]))
                  )
                , ( "simple list"
                  , "[1,2,3]"
                  , Just (located { end = { col = 8, row = 1 }, start = { col = 1, row = 1 } } (List [ located { end = { col = 3, row = 1 }, start = { col = 2, row = 1 } } (Literal (Int 1)), located { end = { col = 5, row = 1 }, start = { col = 4, row = 1 } } (Literal (Int 2)), located { end = { col = 7, row = 1 }, start = { col = 6, row = 1 } } (Literal (Int 3)) ]))
                  )
                , ( "simple list with inner spaces"
                  , "[ 1,  2  , 3 ]"
                  , Just (located { end = { col = 15, row = 1 }, start = { col = 1, row = 1 } } (List [ located { end = { col = 4, row = 1 }, start = { col = 3, row = 1 } } (Literal (Int 1)), located { end = { col = 8, row = 1 }, start = { col = 7, row = 1 } } (Literal (Int 2)), located { end = { col = 13, row = 1 }, start = { col = 12, row = 1 } } (Literal (Int 3)) ]))
                  )
                ]
              )
            , ( "unit"
              , [ ( "simple case"
                  , "()"
                  , Just (located { end = { col = 3, row = 1 }, start = { col = 1, row = 1 } } Unit)
                  )
                ]
              )
            , ( "tuple"
              , [ ( "without spaces"
                  , "(1,1)"
                  , Just (located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Tuple (located { end = { col = 3, row = 1 }, start = { col = 1, row = 1 } } (Literal (Int 1))) (located { end = { col = 3, row = 1 }, start = { col = 1, row = 1 } } (Literal (Int 1)))))
                  )
                , ( "with inner spaces"
                  , "( 1 , 1 )"
                  , Just (located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Tuple (located { end = { col = 3, row = 1 }, start = { col = 1, row = 1 } } (Literal (Int 1))) (located { end = { col = 3, row = 1 }, start = { col = 1, row = 1 } } (Literal (Int 1)))))
                  )
                ]
              )
            , ( "tuple3"
              , [ ( "without spaces"
                  , "(1,2,3)"
                    -- Todo: await parser to ensure located is correct
                  , Just (located { end = { col = 7, row = 1 }, start = { col = 1, row = 1 } } (Tuple3 (located { end = { col = 3, row = 1 }, start = { col = 1, row = 1 } } (Literal (Int 1))) (located { end = { col = 3, row = 1 }, start = { col = 1, row = 1 } } (Literal (Int 2))) (located { end = { col = 3, row = 1 }, start = { col = 1, row = 1 } } (Literal (Int 2)))))
                  )
                , ( "with inner spaces"
                  , "( 1 , 2 , 3 )"
                    -- Todo: await parser to ensure located is correct
                  , Just (located { end = { col = 7, row = 1 }, start = { col = 1, row = 1 } } (Tuple3 (located { end = { col = 3, row = 1 }, start = { col = 1, row = 1 } } (Literal (Int 1))) (located { end = { col = 3, row = 1 }, start = { col = 1, row = 1 } } (Literal (Int 2))) (located { end = { col = 3, row = 1 }, start = { col = 1, row = 1 } } (Literal (Int 2)))))
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
