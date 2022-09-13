module EmitJsonTest exposing (json)

import Dict
import Elm.AST.Typed as Typed exposing (Expr_(..))
import Elm.Data.Declaration exposing (Declaration, DeclarationBody(..))
import Elm.Data.Qualifiedness exposing (Qualified)
import Expect
import Fuzz exposing (bool, char, int, niceFloat, string)
import Json.Encode as E
import Stage.Emit.JsonAST as JSON
import Test exposing (Test, describe, fuzz, test)
import TestHelpers
    exposing
        ( typed
        , typedInt
        , typedIntList
        , typedString
        )


fromBool : Bool -> String
fromBool b =
    if b then
        "true"

    else
        "false"


json : Test
json =
    describe "Stage.Emit.JsonAST"
        [ describe "literals"
            [ fuzz int "encode integer" <|
                \x ->
                    typedInt x
                        |> JSON.emitExpr
                        |> E.encode 0
                        |> Expect.equal
                            ("{\"type\":\"int\",\"value\":" ++ String.fromInt x ++ "}")
            , fuzz niceFloat "encode float" <|
                \x ->
                    typed (Float x)
                        |> JSON.emitExpr
                        |> E.encode 0
                        |> Expect.equal
                            ("{\"type\":\"float\",\"value\":" ++ String.fromFloat x ++ "}")
            , fuzz char "encode char" <|
                \x ->
                    typed (Char x)
                        |> JSON.emitExpr
                        |> E.encode 0
                        |> Expect.equal
                            ("{\"type\":\"char\",\"value\":" ++ E.encode 0 (E.string (String.fromChar x)) ++ "}")
            , fuzz string "encode string" <|
                \x ->
                    typed (String x)
                        |> JSON.emitExpr
                        |> E.encode 0
                        |> Expect.equal
                            ("{\"type\":\"string\",\"value\":" ++ E.encode 0 (E.string x) ++ "}")
            , fuzz bool "encode bool" <|
                \x ->
                    typed (Bool x)
                        |> JSON.emitExpr
                        |> E.encode 0
                        |> Expect.equal
                            ("{\"type\":\"bool\",\"value\":" ++ fromBool x ++ "}")
            ]
        , let
            runTest : ( String, Typed.Expr_, String ) -> Test
            runTest ( description, input, output ) =
                test description <|
                    \() ->
                        typed input
                            |> JSON.emitExpr
                            |> E.encode 0
                            |> Expect.equal output
          in
          describe "emitExpr"
            [ describe "Var"
                (List.map runTest
                    [ ( "simple", Var { module_ = "Foo", name = "bar" }, "{\"type\":\"var\",\"name\":\"Foo$bar\"}" )
                    , ( "nested", Var { module_ = "Foo.Bar", name = "baz" }, "{\"type\":\"var\",\"name\":\"Foo$Bar$baz\"}" )
                    ]
                )
            , describe "Argument"
                (List.map runTest
                    [ ( "simple", Argument "foo", "{\"type\":\"arg\",\"name\":\"foo\"}" )
                    ]
                )
            , describe "Plus"
                (List.map runTest
                    [ ( "simple", Plus (typedInt 1) (typedInt 2), "{\"type\":\"plus\",\"e1\":{\"type\":\"int\",\"value\":1},\"e2\":{\"type\":\"int\",\"value\":2}}" )
                    ]
                )
            , describe "Cons"
                (List.map runTest
                    [ ( "simple"
                      , Cons (typedInt 1) (typedIntList [ 2, 3 ])
                      , "{\"type\":\"cons\",\"e1\":{\"type\":\"int\",\"value\":1},\"e2\":{\"type\":\"list\",\"items\":[{\"type\":\"int\",\"value\":2},{\"type\":\"int\",\"value\":3}]}}"
                      )
                    ]
                )
            , describe "Lambda"
                (List.map runTest
                    [ ( "simple"
                      , Lambda { argument = "x", body = typedInt 1 }
                      , "{\"type\":\"lambda\",\"arg\":\"x\",\"body\":{\"type\":\"int\",\"value\":1}}"
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
                      , "{\"type\":\"call\",\"fn\":{\"type\":\"var\",\"name\":\"Basics$negate\"},\"arg\":{\"type\":\"int\",\"value\":1}}"
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
                      , "{\"type\":\"if\",\"test\":{\"type\":\"bool\",\"value\":true},\"then\":{\"type\":\"int\",\"value\":1},\"else\":{\"type\":\"int\",\"value\":2}}"
                      )
                    ]
                )
            , describe "List"
                (List.map runTest
                    [ ( "empty list"
                      , List []
                      , "{\"type\":\"list\",\"items\":[]}"
                      )
                    , ( "simple list"
                      , List [ typedInt 1, typedInt 2, typedInt 3 ]
                      , "{\"type\":\"list\",\"items\":[{\"type\":\"int\",\"value\":1},{\"type\":\"int\",\"value\":2},{\"type\":\"int\",\"value\":3}]}"
                      )
                    ]
                )
            , runTest
                ( "Unit"
                , Unit
                , "{\"type\":\"unit\"}"
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
                      , "{\"type\":\"let\",\"bind\":{\"x\":{\"type\":\"int\",\"value\":2}},\"body\":{\"type\":\"int\",\"value\":1}}"
                      )
                    ]
                )
            , describe "Tuple"
                (List.map runTest
                    [ ( "simple tuple"
                      , Tuple (typedInt 1) (typedInt 2)
                      , "{\"type\":\"tuple\",\"e1\":{\"type\":\"int\",\"value\":1},\"e2\":{\"type\":\"int\",\"value\":2}}"
                      )
                    ]
                )
            , describe "Tuple3"
                (List.map runTest
                    [ ( "simple tuple3"
                      , Tuple3 (typedInt 1) (typedInt 2) (typedInt 3)
                      , "{\"type\":\"tuple\",\"e1\":{\"type\":\"int\",\"value\":1},\"e2\":{\"type\":\"int\",\"value\":2},\"e3\":{\"type\":\"int\",\"value\":3}}"
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
                      , "{\"type\":\"record\",\"bind\":{\"a\":{\"type\":\"int\",\"value\":42}}}"
                      )
                    , ( "void record"
                      , Record Dict.empty
                      , "{\"type\":\"record\",\"bind\":{}}"
                      )
                    , ( "record two fields"
                      , Record
                            (Dict.fromList
                                [ ( "a", { name = "a", body = typedInt 42 } )
                                , ( "b", { name = "b", body = typedString "Hello" } )
                                ]
                            )
                      , "{\"type\":\"record\",\"bind\":{\"a\":{\"type\":\"int\",\"value\":42},\"b\":{\"type\":\"string\",\"value\":\"Hello\"}}}"
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
                            |> JSON.emitDeclaration
                            |> E.encode 0
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
                  , """{"type":"decl","name":"Foo$bar","expr":{"type":"int","value":1}}"""
                  )
                ]
            )
        ]
