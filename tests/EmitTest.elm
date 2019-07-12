module EmitTest exposing (javascript)

import AST.Common.Literal exposing (Literal(..))
import AST.Common.Located as Located exposing (Located)
import AST.Common.Type as Type
import AST.Typed as Typed exposing (Expr_(..), LocatedExpr)
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


typedHelp : Typed.Expr -> Typed.LocatedExpr
typedHelp expr =
    Located.located
        -- position do not matters in emit
        { start = { row = 0, col = 0 }, end = { row = 0, col = 0 } }
        expr


typed : Typed.Expr_ -> Typed.LocatedExpr
typed expr =
    Located.located
        -- position do not matters in emit
        { start = { row = 0, col = 0 }, end = { row = 0, col = 0 } }
        ( expr, Type.Int )


typedInt : Int -> Typed.LocatedExpr
typedInt int =
    typedHelp ( Literal (Int int), Type.Int )


typedBool : Bool -> Typed.LocatedExpr
typedBool bool =
    typedHelp ( Literal (Bool bool), Type.Bool )


typedString : String -> Typed.LocatedExpr
typedString str =
    typedHelp ( Literal (String str), Type.String )


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
          in
          describe "emitExpr_"
            [ describe "Int"
                (List.map runTest
                    [ ( "positive int", Literal (Int 42), "42" )
                    , ( "negative int", Literal (Int -998), "-998" )
                    ]
                )

            -- See https://ellie-app.com/62Ydd5JYgxca1
            , describe "Float"
                (List.map runTest
                    [ ( "positive float", Literal (Float 12.3), "12.3" )
                    , ( "negative float", Literal (Float -12.3), "-12.3" )
                    , ( "positive zero float", Literal (Float 0.0), "0" )
                    , ( "negative zero float", Literal (Float -0.0), "0" )
                    , ( "positive infitiny", Literal (Float (1 / 0.0)), "Infinity" )
                    , ( "negative infitiny", Literal (Float (1 / -0.0)), "-Infinity" )
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

            -- this [ _ , _ ] format will eventually have to change to something like List$fromArray([...]) when we add (::), (++), etc
            , describe "List"
                (List.map runTest
                    [ ( "empty list"
                      , List []
                      , "[]"
                      )
                    , ( "single item in list"
                      , List [ typedInt 1 ]
                      , "[1]"
                      )
                    , ( "simple list"
                      , List [ typedInt 1, typedInt 2, typedInt 3 ]
                      , "[1, 2, 3]"
                      )
                    ]
                )
            , runTest
                ( "Unit"
                , Unit
                , """{type: "unit"}"""
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
            , describe "Tuple"
                (List.map runTest
                    [ ( "simple tuple", Tuple (typedInt 1) (typedInt 2), "[1,2]" )
                    , ( "mixed tuple", Tuple (typedInt 1) (typedString "hello"), "[1,\"hello\"]" )
                    , ( "nested tuple", Tuple (typedInt 1) (typed (Tuple (typedInt 2) (typedInt 3))), "[1,[2,3]]" )
                    ]
                )
            , describe "Tuple3"
                (List.map runTest
                    [ ( "simple tuple3", Tuple3 (typedInt 1) (typedInt 2) (typedInt 3), "[1,2,3]" )
                    , ( "mixed tuple3", Tuple3 (typedInt 1) (typedString "hello") (typedBool True), "[1,\"hello\",true]" )
                    , ( "nested tuple3"
                      , Tuple3 (typedInt 1) (typedInt 2) (typed (Tuple3 (typedInt 3) (typedInt 4) (typedInt 5)))
                      , "[1,2,[3,4,5]]"
                      )
                    ]
                )
            ]
        , let
            runTest : ( String, TopLevelDeclaration Typed.LocatedExpr, String ) -> Test
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
                    , body = typedInt 1
                    }
                  , "const Foo$bar = 1;"
                  )
                ]
            )
        ]
