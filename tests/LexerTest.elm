module LexerTest exposing (..)

import Dict
import Elm.AST.Frontend as Frontend
import Elm.AST.Frontend.Unwrapped exposing (Expr(..), Pattern(..))
import Elm.Data.Declaration as Declaration exposing (DeclarationBody)
import Elm.Data.Exposing exposing (ExposedItem(..), Exposing(..))
import Elm.Data.Located as Located
import Elm.Data.Module exposing (ModuleType(..))
import Elm.Data.Qualifiedness exposing (PossiblyQualified(..))
import Elm.Data.Type.Concrete as ConcreteType exposing (ConcreteType)
import Elm.Data.TypeAnnotation exposing (TypeAnnotation)
import Expect exposing (Expectation)
import OurExtras.String as String
import Parser.Advanced as P
import Stage.Parse.Lexer exposing (..)
import String.Extra as String
import Test exposing (Test, describe, test)


runTest ( description, input ) =
    test description <|
        \() ->
            input
                -- |> P.run ((located (Stage.Parse.Lexer.literalParser
                --         |> P.map (\( ty, literalBody ) -> Literal ty literalBody  )))
                --           |> P.map List.singleton)
                |> P.run Stage.Parse.Lexer.parser
                |> Expect.all
                    [ Result.map (List.map (Located.unwrap >> toString) >> String.join "")
                        >> Expect.equal (Ok input)
                    , Result.map
                        (List.filterMap
                            (\item ->
                                case Located.unwrap item of
                                    Invalid s ->
                                        Just s

                                    _ ->
                                        Nothing
                            )
                        )
                        >> Expect.equal (Ok [])
                    ]


moduleDeclaration : Test
moduleDeclaration =
    describe "Stage.Parse.Parser.moduleDeclaration"
        [ describe "general"
            (List.map runTest
                [ ( "works with simple module name"
                  , "module Foo exposing (..)"
                    -- , [ Token "module"
                    --   , Whitespace 1
                    --   , Token "Foo"
                    --   , Whitespace 1
                    --   , Token "exposing"
                    --   , Whitespace 1
                    --   , Sigil (Bracket Round Open)
                    --   , Sigil DoubleDot
                    --   , Sigil (Bracket Round Close)
                    --   ]
                  )
                , ( "works with nested module name"
                  , "module Foo.Bar exposing (..)"
                  )
                , ( "works with even more nested module name"
                  , "module Foo.Bar.Baz.Quux exposing (..)"
                  )
                , ( "allows multiple spaces between the `module` keyword and the module name"
                  , "module  Foo exposing (..)"
                  )
                , ( "doesn't allow a newline between the `module` keyword and the module name"
                  , """
                    module
                    Foo exposing (..)
                    """
                        |> String.unindent
                        |> String.removeNewlinesAtEnds
                  )
                , ( "allows a newline and space between the `module` keyword and the module name"
                  , """
                    module
                     Foo exposing (..)
                    """
                        |> String.unindent
                        |> String.removeNewlinesAtEnds
                  )
                , ( "allows multiple spaces between the module name and the `exposing` keyword"
                  , "module Foo  exposing (..)"
                  )
                , ( "doesn't allow a newline between the module name and the `exposing` keyword"
                  , """
                    module Foo
                    exposing (..)
                    """
                        |> String.unindent
                        |> String.removeNewlinesAtEnds
                  )
                , ( "allows newline and space between the module name and the `exposing` keyword"
                  , """
                    module Foo
                     exposing (..)
                    """
                        |> String.unindent
                        |> String.removeNewlinesAtEnds
                  )
                , ( "allows multiple spaces between the `exposing` keyword and the exposing list"
                  , "module Foo exposing  (..)"
                  )
                , ( "doesn't allow a newline between the `exposing` keyword and the exposing list"
                  , """
                    module Foo exposing
                    (..)
                    """
                        |> String.unindent
                        |> String.removeNewlinesAtEnds
                  )
                , ( "allows a newline and space between the `exposing` keyword and the exposing list"
                  , """
                    module Foo exposing
                     (..)
                    """
                        |> String.unindent
                        |> String.removeNewlinesAtEnds
                  )
                , ( "doesn't work without something after the `exposing` keyword"
                  , "module Foo exposing"
                  )
                ]
            )
        , describe "plain module"
            (List.map runTest
                [ ( "simply works"
                  , "module Foo exposing (..)"
                  )
                ]
            )
        , describe "port module"
            (List.map runTest
                [ ( "simply works"
                  , "port module Foo exposing (..)"
                  )
                ]
            )
        ]


exposingList : Test
exposingList =
    describe "Stage.Parse.Parser.exposingList"
        [ describe "exposing all"
            (List.map runTest
                [ ( "simply works"
                  , "(..)"
                  )
                , ( "doesn't work with spaces inside the parens"
                  , "( .. )"
                  )
                ]
            )
        , describe "exposing some"
            [ describe "general"
                (List.map runTest
                    [ ( "can't be empty"
                      , "()"
                      )
                    , ( "works with spaces between items"
                      , "(foo, bar)"
                      )
                    , ( "works with even more spaces between items"
                      , "(foo  ,  bar)"
                      )
                    , ( "works with mixed values"
                      , "(foo, Bar, Baz(..))"
                      )
                    , ( "allows for newline"
                      , "(foo\n,bar)"
                      )
                    ]
                )
            , describe "values"
                (List.map runTest
                    [ ( "works with a value"
                      , "(foo)"
                      )
                    ]
                )
            , describe "types"
                (List.map runTest
                    [ ( "works with exposed type"
                      , "(Foo)"
                      )
                    ]
                )
            , describe "types with all constructors"
                (List.map runTest
                    [ ( "works with exposed type and all constructors"
                      , "(Foo(..))"
                      )
                    , ( "doesn't allow spaces between the module name and the double period list"
                      , "(Foo (..))"
                      )
                    , ( "doesn't allow spaces inside the double period list"
                      , "(Foo( .. ))"
                      )
                    , ( "doesn't allow only some constructors exposed"
                      , "(Foo(Bar))"
                      )
                    ]
                )
            ]
        ]


imports : Test
imports =
    describe "Stage.Parse.Parser.imports"
        [ describe "general"
            (List.map runTest
                [ ( "allows for multiple modifiers"
                  , "import Foo as F exposing (..)"
                  )
                , ( "allows for multiple spaces"
                  , "import   Foo   as   F   exposing   (..)"
                  )
                , ( "allows for multiple imports"
                  , "import Foo\nimport Bar"
                  )
                , ( "allows for multiple newlines between imports"
                  , "import Foo\n\nimport Bar"
                  )
                , ( "doesn't allow for lower-case import"
                  , "import foo"
                  )
                ]
            )
        , describe "simple"
            (List.map runTest
                [ ( "simply works"
                  , "import Foo"
                  )
                ]
            )
        , describe "as"
            (List.map runTest
                [ ( "simply works"
                  , "import Foo as F"
                  )
                , ( "doesn't work with lowercase alias"
                  , "import Foo as f"
                  )
                , ( "doesn't work with dot-separated alias"
                  , "import Foo as X.B"
                  )
                ]
            )
        , describe "exposing"
            (List.map runTest
                [ ( "simply works"
                  , "import Foo exposing (bar, Baz, Quux(..))"
                  )
                , ( "doesn't work without something after the `exposing` keyword"
                  , "import Foo exposing"
                  )
                ]
            )
        ]


moduleName : Test
moduleName =
    describe "Stage.Parse.Parser.moduleName"
        (List.map runTest
            [ ( "works with simple module name"
              , "Foo"
              )
            , ( "doesn't work with lower-case name"
              , "foo"
              )
            , ( "doesn't work with dot at the end"
              , "Foo."
              )
            , ( "works with dotted module name"
              , "Foo.Bar"
              )
            , ( "works with doubly-dotted module name"
              , "Foo.Bar.Baz"
              )
            , ( "doesn't work with lower-case letter after the dot"
              , "Foo.bar"
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
    in
    describe "Stage.Parse.Parser.expr"
        (List.map runSection
            [ ( "lambda"
              , [ ( "works with single argument"
                  , "\\x -> x + 1"
                  )
                , ( "multiline"
                  , """
                    \\x ->
                        x + 1
                    """
                        |> String.unindent
                        |> String.removeNewlinesAtEnds
                  )
                , ( "works with multiple arguments"
                  , "\\x y -> x + y"
                  )
                ]
              )
            , ( "call"
              , [ ( "simple"
                  , "fn 1"
                  )
                , ( "with var"
                  , "fn arg"
                  )
                , ( "multiple"
                  , "fn arg1 arg2"
                  )
                , ( "space not needed if parenthesized arg"
                  , "fn(arg1)"
                  )
                , ( "multiline"
                  , """
                    fn
                        arg1
                        arg2
                    """
                        |> String.unindent
                        |> String.removeNewlinesAtEnds
                  )
                ]
              )
            , ( "if"
              , [ ( "with one space"
                  , "if 1 then 2 else 3"
                  )
                , ( "with multiple spaces"
                  , "if   1   then   2   else   3"
                  )
                , ( "multiline"
                  , """
                    if 1 then
                        2
                    else
                        3
                    """
                        |> String.unindent
                        |> String.removeNewlinesAtEnds
                  )
                ]
              )
            , ( "literal int"
              , [ ( "positive"
                  , "123"
                  )
                , ( "zero"
                  , "0"
                  )
                , ( "negative zero"
                  , "-0"
                  )
                , ( "hexadecimal int"
                  , "0x123abc"
                  )
                , ( "hexadecimal int - uppercase"
                  , "0x789DEF"
                  )
                , ( "hexadecimal int - mixed case"
                  , "0x789dEf"
                  )
                , ( "negative int"
                  , "-42"
                  )
                , ( "negative hexadecimal"
                  , "-0x123abc"
                  )
                , ( "starting with zero disallowed"
                  , "0123"
                  )
                , ( "e is interpreted as hex 14, not scientific notation"
                  , "0xABCe5"
                  )
                ]
              )
            , ( "literal float"
              , [ ( "positive"
                  , "12.3"
                  )
                , ( "zero"
                  , "0.0"
                  )
                , ( "negative zero"
                  , "-0.0"
                  )
                , ( "negative float"
                  , "-4.2"
                  )
                , ( "Scientific notation"
                  , "5e2"
                  )
                , ( "Scientific notation with dot"
                  , "5.12e2"
                  )
                , ( "Uppercase scientific notation"
                  , "5.12E2"
                  )
                , ( "Negative scientific notation"
                  , "-5.12e2"
                  )
                , ( "Exponent with explicit plus sign"
                  , "5e+2"
                  )
                , ( "Uppercase E and exponent with explicit plus sign"
                  , "5E+2"
                  )
                , ( "Negative exponent"
                  , "5e-2"
                  )
                , ( "Zero - exhibit 1"
                  , "0.0e5"
                  )
                , ( "Zero - exhibit 2"
                  , "0e5"
                  )
                , ( "starting with dot disallowed"
                  , ".123"
                  )
                , ( "ending with dot disallowed"
                  , "123."
                  )
                ]
              )
            , ( "literal char"
              , [ ( "number"
                  , "'1'"
                  )
                , ( "space"
                  , "' '"
                  )
                , ( "newline shouldn't work"
                  , "'\n'"
                  )
                , ( "letter lowercase"
                  , "'a'"
                  )
                , ( "letter uppercase"
                  , "'A'"
                  )

                -- https://github.com/elm/compiler/blob/dcbe51fa22879f83b5d94642e117440cb5249bb1/compiler/src/Parse/String.hs#L279-L285
                , ( "escape backslash"
                  , singleQuote "\\\\"
                  )
                , ( "escape n"
                  , singleQuote "\\n"
                  )
                , ( "escape r"
                  , singleQuote "\\r"
                  )
                , ( "escape t"
                  , singleQuote "\\t"
                  )
                , ( "double quote"
                  , singleQuote "\""
                  )
                , ( "single quote"
                  , singleQuote "\\'"
                  )
                , ( "emoji"
                  , singleQuote "😃"
                  )
                , ( "escaped unicode code point"
                  , singleQuote "\\u{1F648}"
                  )
                ]
              )
            , ( "literal string"
              , [ ( "empty"
                  , doubleQuote ""
                  )
                , ( "one space"
                  , doubleQuote " "
                  )
                , ( "newline shouldn't work"
                  , doubleQuote "\n"
                  )
                , ( "two numbers"
                  , doubleQuote "42"
                  )
                , ( "single quote"
                  , doubleQuote "'"
                  )
                , ( "double quote"
                  , doubleQuote "\\\""
                  )
                , ( "escape backslash"
                  , doubleQuote "\\\\"
                  )
                , ( "escape n"
                  , doubleQuote "\\n"
                  )
                , ( "escape r"
                  , doubleQuote "\\r"
                  )
                , ( "escape t"
                  , doubleQuote "\\t"
                  )
                , ( "emoji"
                  , doubleQuote "😃"
                  )
                , ( "escaped unicode code point"
                  , doubleQuote "\\u{1F648}"
                  )
                , ( "combo of escapes and chars"
                  , doubleQuote "\\u{1F648}\\n\\r\\t\\\\abc123"
                  )
                ]
              )
            , ( "literal multiline string"
              , [ ( "empty"
                  , tripleQuote ""
                  )
                , ( "one space"
                  , tripleQuote " "
                  )
                , ( "newline"
                  , tripleQuote "\n"
                  )
                , ( "two numbers"
                  , tripleQuote "42"
                  )
                , ( "single quote"
                  , tripleQuote "'"
                  )
                , ( "double quote"
                  , tripleQuote " \" "
                  )
                , ( "escape backslash"
                  , tripleQuote "\\\\"
                  )
                , ( "escape n"
                  , tripleQuote "\\n"
                  )
                , ( "escape r"
                  , tripleQuote "\\r"
                  )
                , ( "escape t"
                  , tripleQuote "\\t"
                  )
                , ( "emoji"
                  , tripleQuote "😃"
                  )
                , ( "escaped unicode code point"
                  , tripleQuote "\\u{1F648}"
                  )
                , ( "combo of escapes, newlines, and chars"
                  , tripleQuote "\\u{1F648}\\n\n\n\\r\\t\\\\abc123"
                  )
                ]
              )
            , ( "literal bool"
              , [ ( "True"
                  , "True"
                  )
                , ( "False"
                  , "False"
                  )
                ]
              )
            , ( "let"
              , [ ( "one liner"
                  , "let x = 1 in 2"
                  )
                , ( "one binding, generous whitespace"
                  , """
                    let
                      x =
                          1
                    in
                      2
                    """
                        |> String.unindent
                        |> String.removeNewlinesAtEnds
                  )
                , ( "doesn't allow bindings on the same indentation level as `let`"
                  , """
                    let
                    x = 1
                    in
                      2
                    """
                        |> String.unindent
                        |> String.removeNewlinesAtEnds
                  )
                , ( "allows result expr on the same indentation level as `let`"
                  , """
                    let
                     x = 1
                    in
                    2
                    """
                        |> String.unindent
                        |> String.removeNewlinesAtEnds
                  )
                , ( "multiple bindings"
                  , """
                    let
                      x = 1
                      y = 2
                    in
                    3
                    """
                        |> String.unindent
                        |> String.removeNewlinesAtEnds
                  )
                , ( "doesn't allow bindings to have different indentation from each other"
                  , """
                    let
                      x = 1
                       y = 2
                    in
                      3
                    """
                        |> String.unindent
                        |> String.removeNewlinesAtEnds
                  )
                , ( "doesn't allow bindings to have different indentation from each other - the other way"
                  , """
                    let
                       x = 1
                      y = 2
                    in
                      3
                    """
                        |> String.unindent
                        |> String.removeNewlinesAtEnds
                  )
                , ( "one binding that's used in the body"
                  , """
                    let
                      x = 2
                    in
                      1 + x
                    """
                        |> String.unindent
                        |> String.removeNewlinesAtEnds
                  )
                , ( "two bindings where one is dependent on the other"
                  , """
                    let
                      x = 2
                      y = x + 1
                    in
                      42
                    """
                        |> String.unindent
                        |> String.removeNewlinesAtEnds
                  )
                ]
              )
            , ( "list"
              , [ ( "empty list"
                  , "[]"
                  )
                , ( "empty list with inner spaces"
                  , "[  ]"
                  )
                , ( "single item in list"
                  , "[1]"
                  )
                , ( "single item in list with inner spaces"
                  , "[ 1 ]"
                  )
                , ( "simple list"
                  , "[1,2,3]"
                  )
                , ( "simple list with inner spaces"
                  , "[ 1,  2  , 3 ]"
                  )
                , ( "list concat"
                  , "[] ++ []"
                  )
                , ( "multiline"
                  , """
                    [ 1
                    , 2
                    , 3
                    ]
                    """
                        |> String.unindent
                        |> String.removeNewlinesAtEnds
                  )
                ]
              )
            , ( "unit"
              , [ ( "simple case"
                  , "()"
                  )
                ]
              )
            , ( "tuple"
              , [ ( "without spaces"
                  , "(1,2)"
                  )
                , ( "with inner spaces"
                  , "( 3 , 4 )"
                  )
                , ( "nested tuple"
                  , "(5,(6,7))"
                  )
                ]
              )
            , ( "tuple3"
              , [ ( "without spaces"
                  , "(1,2,3)"
                  )
                , ( "with inner spaces"
                  , "( 4 , 5 , 6 )"
                  )
                ]
              )
            , ( "cons"
              , [ ( "simple case"
                  , "1 :: []"
                  )
                , ( "multiple values case"
                  , "1 :: 2 :: []"
                  )
                , ( "no spaces"
                  , "1::[]"
                  )
                , ( "multiple spaces"
                  , "1    ::      []"
                  )
                ]
              )
            , ( "record"
              , [ ( "empty record"
                  , "{}"
                  )
                , ( "empty record with spaces"
                  , "{   }"
                  )
                , ( "one field record"
                  , "{ a = 42 }"
                  )
                , ( "one field record without spaces"
                  , "{a=42}"
                  )
                , ( "two fields record"
                  , """{ a = 42, b = "hello" }"""
                  )
                , ( "multiline"
                  , """
                    { a = 42
                    , b = "hello"
                    }
                    """
                  )
                ]
              )
            , ( "case"
              , [ ( "simple case"
                  , "case True of _->True"
                  )
                , ( "multiline case"
                  , """
                    case 21 of
                        31 -> True
                        5 -> True
                        0xABC -> True
                        _ -> False
                    """
                        |> String.unindent
                        |> String.removeNewlinesAtEnds
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
                        |> String.unindent
                        |> String.removeNewlinesAtEnds
                  )
                ]
              )
            ]
        )


expectEqualParseResult :
    String
    -> a
    -> Result (List (P.DeadEnd Never LexProblem)) a
    -> Expectation
expectEqualParseResult input expected actual =
    case actual of
        Err deadEnds ->
            Expect.fail
                (String.join "\n"
                    (input
                        :: "===>"
                        :: "Err"
                        :: List.map deadEndToString deadEnds
                    )
                )

        Ok actual_ ->
            actual_
                |> Expect.equal expected


deadEndToString : P.DeadEnd Never LexProblem -> String
deadEndToString deadEnd =
    "\n("
        ++ String.fromInt (deadEnd.row - 1)
        ++ ","
        ++ String.fromInt (deadEnd.col - 1)
        ++ ") "
        ++ Debug.toString deadEnd.problem



type_ : Test
type_ =
    describe "Stage.Parse.Parser.type_"
        (List.map runTest
            [ ( "int", "Int")
            , ( "unit", "()")
            , ( "type var a", "a")
            , ( "function"
              , "Int -> ()"
              )
            , ( "multiple-arg function"
              , "Int -> () -> Char"
              )
            , ( "float", "Float")
            , ( "char", "Char")
            , ( "string", "String")
            , ( "bool", "Bool")
            , ( "list", "List ()")
            , ( "tuple"
              , "(Int, String)"
            )
            , ( "tuple with different whitespace"
              , "( Int,String )"
              )
            , ( "tuple with yet different whitespace"
              , "( Int ,  String )"
              )
            , ( "tuple3"
              , "(Int, String, Bool)"
              )
            , ( "custom type or alias"
              , "NonemptyList"
              )
            , ( "parametric type"
              , "Maybe a"
              )
            , ( "qualified custom type"
              , "Foo.Bar"
              )
            , ( "qualified custom type with params"
              , "Foo.Bar a Int"
              )
            , ( "empty record"
              , "{}"
              )
            , ( "empty record with whitespace"
              , "{ }"
              )
            , ( "record with one field"
              , "{ x : Int }"
              )
            , ( "record with two fields"
              , "{ x : Int, y : String }"
              )
            , ( "multiline record"
              , """
                { x : Int
                , y : String
                }
                """
                    |> String.unindent
                    |> String.removeNewlinesAtEnds
              )
            , ( "parenthesized type"
              , "(Int)"
              )
            , ( "parenthesized type with whitespace"
              , "( Int )"
              )
            ]
        )
valueDeclaration : Test
valueDeclaration =
    -- TODO add various whitespace behavior tests
    describe "Stage.Parse.Parser.valueDeclaration" <|
        List.map runTest
            [ ( "simple without annotation"
              , "x = ()"
              )
            , ( "simple with annotation"
              , """
                y : ()
                y = ()
                """
                    |> String.unindent
                    |> String.removeNewlinesAtEnds
              )
            , ( "user defined type with argument - newline+space"
              , """
                x : Foo
                 ()
                x = ()
                """
                    |> String.unindent
                    |> String.removeNewlinesAtEnds
              )
            , ( "user defined type with argument - newline only"
              , """
                x : Foo
                ()
                x = ()
                """
                    |> String.unindent
                    |> String.removeNewlinesAtEnds
              )
            , ( "type on a newline+space"
              , """
                x :
                 ()
                x = ()
                """
                    |> String.unindent
                    |> String.removeNewlinesAtEnds
              )
            , ( "type on a newline without space"
              , """
                x :
                ()
                x = ()
                """
                    |> String.unindent
                    |> String.removeNewlinesAtEnds
              )
            ]
typeAliasDeclaration : Test
typeAliasDeclaration =
    describe "Stage.Parse.Parser.typeAliasDeclaration" <|
        List.map runTest <|
            [ ( "simple"
              , "type alias Foo = ()"
              )
            , ( "with params"
              , "type alias Bar a = ()"
              )
            , ( "a bit more advanced"
              , "type alias Foo = Maybe Int"
              )
            , {- TODO create integration test that this fails
                 (`a` on right must be present on the left too)
              -}
              ( "to something that itself has params"
              , "type alias Foo = Maybe a"
              )
            , ( "params on both sides"
              , "type alias Foo a = Maybe a"
              )
            ]
customTypeDeclaration : Test
customTypeDeclaration =
    describe "Stage.Parse.Parser.customTypeDeclaration" <|
        List.map runTest <|
            [ ( "simple"
              , "type Foo = Bar"
              )
            , ( "with params"
              , "type Bar a = Baz"
              )
            , ( "a bit more advanced"
              , "type Foo = Bar Int"
              )
            , {- TODO create integration test that this fails
                 (`a` on right must be present on the left too)
              -}
              ( "to something that itself has parameters"
              , "type Foo = Bar a"
              )
            , ( "params on both sides"
              , "type Foo a = Bar (Maybe a)"
              )
            , ( "multiple constructors"
              , "type Foo = Bar | Baz"
              )
            ]
portDeclaration : Test
portDeclaration =
    describe "Stage.Parse.Parser.portDeclaration" <|
        List.map runTest <|
            [ ( "outgoing"
              , "port foo : String -> Cmd msg"
              )
            , ( "incoming"
              , "port bar : (String -> msg) -> Sub msg"
              )
            , ( "wacky multiline"
              , """
                port
                 foo
                 :
                 String -> Cmd msg
                """
                    |> String.unindent
                    |> String.removeNewlinesAtEnds
              )
            ]
