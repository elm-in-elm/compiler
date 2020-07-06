module EmitTest exposing (javascript)

import Dict
import Elm.AST.Typed as Typed exposing (Expr_(..))
import Elm.Data.Declaration exposing (Declaration, DeclarationBody(..))
import Elm.Data.Qualifiedness exposing (Qualified)
import Expect
import Stage.Emit.JavaScript as JS
import Test exposing (Test, describe, test)
import TestHelpers
    exposing
        ( typed
        , typedBool
        , typedInt
        , typedIntList
        , typedString
        )


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
          describe "emitExpr"
            [ describe "Int"
                (List.map runTest
                    [ ( "positive int", Int 42, "42" )
                    , ( "negative int", Int -998, "-998" )
                    , -- Elm wat
                      ( "negative zero int", Int (negate 0), "0" )
                    ]
                )

            -- See https://ellie-app.com/62Ydd5JYgxca1
            , describe "Float"
                (List.map runTest
                    [ ( "positive float", Float 12.3, "12.3" )
                    , ( "negative float", Float -12.3, "-12.3" )
                    , ( "positive zero float", Float 0.0, "0" )
                    , -- Elm wat
                      ( "negative zero float", Float -0.0, "0" )
                    , ( "positive infitiny", Float (1 / 0.0), "Infinity" )
                    , ( "negative infitiny", Float (1 / -0.0), "-Infinity" )
                    ]
                )
            , describe "Char"
                (List.map runTest
                    [ ( "simple ASCII", Char 'x', "\"x\"" )
                    , ( "Unicode", Char '😊', "\"😊\"" )
                    ]
                )
            , describe "String"
                (List.map runTest
                    [ ( "simple string", String "hello world", "\"hello world\"" )
                    , ( "string with newlines", String "abc\ndef", "\"abc\ndef\"" )
                    ]
                )
            , describe "Bool"
                (List.map runTest
                    [ ( "true", Bool True, "true" )
                    , ( "false", Bool False, "false" )
                    ]
                )
            , describe "Var"
                (List.map runTest
                    [ ( "simple", Var { module_ = "Foo", name = "bar" }, "Foo$bar" )
                    , ( "nested", Var { module_ = "Foo.Bar", name = "baz" }, "Foo$Bar$baz" )
                    ]
                )
            , describe "Argument"
                (List.map runTest
                    [ ( "simple", Argument "foo", "foo" )
                    ]
                )
            , describe "Plus"
                (List.map runTest
                    -- We need to give the child `Expr`s a type too
                    [ ( "simple", Plus (typedInt 1) (typedInt 2), "(1 + 2)" )
                    , ( "nested", Plus (typedInt 1) (typed (Plus (typedInt 2) (typedInt 3))), "(1 + (2 + 3))" )
                    ]
                )
            , describe "Cons"
                (List.map runTest
                    [ ( "simple"
                      , Cons (typedInt 1) (typedIntList [ 2, 3 ])
                      , "[1].concat([2, 3])"
                      )
                    , ( "nested"
                      , Cons
                            (typedInt 1)
                            (typed (Cons (typedInt 2) (typedIntList [ 3, 4 ])))
                      , "[1].concat([2].concat([3, 4]))"
                      )
                    ]
                )
            , describe "Lambda"
                (List.map runTest
                    [ ( "simple"
                      , Lambda
                            { argument = "x"
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
                            { fn = typed (Var { module_ = "Basics", name = "negate" })
                            , argument = typedInt 1
                            }
                      , "(Basics$negate(1))"
                      )
                    , ( "with lambda"
                      , Call
                            { fn =
                                typed
                                    (Lambda
                                        { argument = "x"
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
                            { test = typed (Bool True)
                            , then_ = typedInt 1
                            , else_ = typedInt 2
                            }
                      , "(true ? 1 : 2)"
                      )
                    , ( "simple - false"
                      , If
                            { test = typed (Bool False)
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
                                        { fn = typed (Var { module_ = "String", name = "isEmpty" })
                                        , argument = typed (String "foo")
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
                                Dict.singleton
                                    "x"
                                    { name = "x"
                                    , body = typedInt 2
                                    }
                            , body = typedInt 1
                            }
                      , "((() => {const x = 2; return 1;})())"
                      )
                    , ( "two bindings"
                      , Let
                            { bindings =
                                Dict.fromList
                                    [ ( "x"
                                      , { name = "x"
                                        , body = typedInt 2
                                        }
                                      )
                                    , ( "y"
                                      , { name = "y"
                                        , body = typedInt 3
                                        }
                                      )
                                    ]
                            , body = typedInt 1
                            }
                      , "((() => {const x = 2; const y = 3; return 1;})())"
                      )
                    , ( "one binding used in the body"
                      , Let
                            { bindings =
                                Dict.singleton
                                    "x"
                                    { name = "x"
                                    , body = typedInt 2
                                    }
                            , body =
                                typed
                                    (Plus
                                        (typedInt 1)
                                        (typed (Argument "x"))
                                    )
                            }
                      , "((() => {const x = 2; return (1 + x);})())"
                      )
                    , ( "two bindings dependent on each other"
                      , Let
                            { bindings =
                                Dict.fromList
                                    [ ( "x"
                                      , { name = "x"
                                        , body = typedInt 2
                                        }
                                      )
                                    , ( "y"
                                      , { name = "y"
                                        , body =
                                            typed
                                                (Plus
                                                    (typedInt 1)
                                                    (typed (Argument "x"))
                                                )
                                        }
                                      )
                                    ]
                            , body =
                                typedInt 42
                            }
                      , "((() => {const x = 2; const y = (1 + x); return 42;})())"
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
            , describe "Record"
                (List.map runTest
                    [ ( "record single field"
                      , Record
                            (Dict.fromList
                                [ ( "a", { name = "a", body = typedInt 42 } )
                                ]
                            )
                      , "{a: 42}"
                      )
                    , ( "void record"
                      , Record Dict.empty
                      , "{}"
                      )
                    , ( "record two fields"
                      , Record
                            (Dict.fromList
                                [ ( "a", { name = "a", body = typedInt 42 } )
                                , ( "b", { name = "b", body = typedString "Hello" } )
                                ]
                            )
                      , """{a: 42, b: "Hello"}"""
                      )
                    , ( "nested"
                      , Record
                            (Dict.fromList
                                [ ( "a"
                                  , { name = "a"
                                    , body =
                                        typed
                                            (Record
                                                (Dict.fromList [ ( "a", { name = "a", body = typedInt 42 } ) ])
                                            )
                                    }
                                  )
                                ]
                            )
                      , "{a: {a: 42}}"
                      )
                    ]
                )
            , describe "Mixed expressions"
                (List.map runTest
                    [ ( "plus in tuple"
                      , Tuple (typed (Plus (typedInt 1) (typedInt 41))) (typedString "Hello")
                      , """[(1 + 41),"Hello"]"""
                      )
                    , ( "tuple and cons in record"
                      , Record
                            (Dict.fromList
                                [ ( "a", { name = "a", body = typed (Tuple (typedInt 2) (typedInt 3)) } )
                                , ( "b", { name = "b", body = typed (Cons (typedInt 2) (typedIntList [ 3, 4 ])) } )
                                ]
                            )
                      , """{a: [2,3], b: [2].concat([3, 4])}"""
                      )
                    ]
                )
            ]
        , let
            runTest : ( String, Declaration Typed.LocatedExpr Never Qualified, String ) -> Test
            runTest ( description, input, output ) =
                test description <|
                    \() ->
                        input
                            |> JS.emitDeclaration
                            |> Expect.equal output
          in
          describe "emitDeclaration"
            (List.map runTest
                [ ( "simple"
                  , { module_ = "Foo"
                    , name = "bar"
                    , body =
                        Value
                            { typeAnnotation = Nothing
                            , expression = typedInt 1
                            }
                    }
                  , "const Foo$bar = 1;"
                  )
                ]
            )
        ]
