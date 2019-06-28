module EmitTest exposing (javascript)

import AST.Common.Literal exposing (Literal(..))
import AST.Common.Type as Type
import AST.Typed as Typed exposing (Expr_(..))
import Common
import Common.Types
    exposing
        ( ModuleName(..)
        , TopLevelDeclaration
        , VarName(..)
        )
import Dict.Any
import Expect exposing (Expectation)
import Stage.Emit.JavaScript as JS
import Test exposing (Test, describe, test, todo)


javascript : Test
javascript =
    describe "Stage.Emit.JavaScript"
        [ let
            runTest : ( String, Typed.Expr_, String ) -> Test
            runTest ( description, input, output ) =
                -- We give the tested expressions a type (shouldn't matter which one as `JS.emitExpr` ignores types)
                test description <|
                    \() ->
                        input
                            |> typed
                            |> JS.emitExpr
                            |> Expect.equal output

            typed : Typed.Expr_ -> Typed.Expr
            typed expr =
                ( expr, Type.Int )

            typedInt : Int -> Typed.Expr
            typedInt int =
                typed (Literal (Int int))
          in
          describe "emitExpr_"
            [ describe "Int"
                (List.map runTest
                    [ ( "positive int", Literal (Int 42), "42" )
                    , ( "negative int", Literal (Int -998), "-998" )
                    ]
                )
            , describe "Char"
                (List.map runTest
                    [ ( "simple ASCII", Literal (Char 'x'), "\"x\"" )
                    , ( "Unicode", Literal (Char '😊'), "\"😊\"" )
                    ]
                )
            , describe "String"
                (List.map runTest
                    [ ( "simple string", Literal (String "hello world"), "\"hello world\"" )
                    , ( "string with newlines", Literal (String "abc\ndef"), "\"abc\ndef\"" )
                    ]
                )
            , describe "Bool"
                (List.map runTest
                    [ ( "true", Literal (Bool True), "true" )
                    , ( "false", Literal (Bool False), "false" )
                    ]
                )
            , describe "Var"
                (List.map runTest
                    [ ( "simple", Var { qualifier = ModuleName "Foo", name = VarName "bar" }, "Foo$bar" )
                    , ( "nested", Var { qualifier = ModuleName "Foo.Bar", name = VarName "baz" }, "Foo$Bar$baz" )
                    ]
                )
            , describe "Argument"
                (List.map runTest
                    [ ( "simple", Argument (VarName "foo"), "foo" )
                    ]
                )
            , describe "Plus"
                (List.map runTest
                    -- We need to give the child `Expr`s a type too
                    [ ( "simple", Plus (typedInt 1) (typedInt 2), "(1 + 2)" )
                    , ( "nested", Plus (typedInt 1) (typed (Plus (typedInt 2) (typedInt 3))), "(1 + (2 + 3))" )
                    ]
                )
            , describe "Lambda"
                (List.map runTest
                    [ ( "simple"
                      , Lambda
                            { argument = VarName "x"
                            , argumentId = 0
                            , body = typedInt 1
                            }
                      , "((x) => 1)"
                      )
                    ]
                )
            , describe "Call"
                (List.map runTest
                    [ ( "simple"
                      , Call
                            { fn = typed (Var { qualifier = ModuleName "Basics", name = VarName "negate" })
                            , argument = typedInt 1
                            }
                      , "(Basics$negate(1))"
                      )
                    , ( "with lambda"
                      , Call
                            { fn =
                                typed
                                    (Lambda
                                        { argument = VarName "x"
                                        , argumentId = 0
                                        , body = typedInt 2
                                        }
                                    )
                            , argument = typedInt 1
                            }
                      , "(((x) => 2)(1))"
                      )
                    ]
                )
            , describe "If"
                (List.map runTest
                    [ ( "simple - true"
                      , If
                            { test = typed (Literal (Bool True))
                            , then_ = typedInt 1
                            , else_ = typedInt 2
                            }
                      , "(true ? 1 : 2)"
                      )
                    , ( "simple - false"
                      , If
                            { test = typed (Literal (Bool False))
                            , then_ = typedInt 1
                            , else_ = typedInt 2
                            }
                      , "(false ? 1 : 2)"
                      )
                    , ( "with fn call"
                      , If
                            { test =
                                typed
                                    (Call
                                        { fn = typed (Var { qualifier = ModuleName "String", name = VarName "isEmpty" })
                                        , argument = typed (Literal (String "foo"))
                                        }
                                    )
                            , then_ = typedInt 1
                            , else_ = typedInt 2
                            }
                      , """((String$isEmpty("foo")) ? 1 : 2)"""
                      )
                    ]
                )
            , describe "Let"
                (List.map runTest
                    [ ( "one binding"
                      , Let
                            { bindings =
                                Dict.Any.singleton
                                    (VarName "x")
                                    { name = VarName "x"
                                    , body = typedInt 2
                                    }
                                    Common.varNameToString
                            , body = typedInt 1
                            }
                      , "((() => {const x = 2; return 1;})())"
                      )
                    , ( "two bindings"
                      , Let
                            { bindings =
                                Dict.Any.fromList
                                    Common.varNameToString
                                    [ ( VarName "x"
                                      , { name = VarName "x"
                                        , body = typedInt 2
                                        }
                                      )
                                    , ( VarName "y"
                                      , { name = VarName "y"
                                        , body = typedInt 3
                                        }
                                      )
                                    ]
                            , body = typedInt 1
                            }
                      , "((() => {const x = 2; const y = 3; return 1;})())"
                      )
                    ]
                )
            ]
        , let
            runTest : ( String, TopLevelDeclaration Typed.Expr, String ) -> Test
            runTest ( description, input, output ) =
                test description <|
                    \() ->
                        input
                            |> JS.emitTopLevelDeclaration
                            |> Expect.equal output
          in
          describe "emitTopLevelDeclaration"
            (List.map runTest
                [ ( "simple"
                  , { module_ = ModuleName "Foo"
                    , name = VarName "bar"
                    , body = ( Literal (Int 1), Type.Int )
                    }
                  , "const Foo$bar = 1;"
                  )
                ]
            )
        ]
