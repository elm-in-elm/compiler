module InferTypesTest exposing (typeInference, typeToString)

import AST.Canonical as Canonical
import AST.Common.Literal as Literal
import AST.Common.Located as Located exposing (located)
import AST.Common.Type as Type exposing (Type(..))
import AST.Typed as Typed
import Common
import Common.Types as Types
import Dict.Any
import Error exposing (TypeError)
import Expect exposing (Expectation)
import Stage.InferTypes
import Test exposing (Test, describe, test)


typeInference : Test
typeInference =
    let
        runSection ( description, tests ) =
            describe description
                (List.map runTest tests)

        runTest ( description, input, output ) =
            test description <|
                \() ->
                    Stage.InferTypes.inferExpr input
                        |> Expect.equal output
    in
    describe "Stage.InferType"
        (List.map runSection
            [ ( "list"
              , [ ( "empty list"
                  , located { end = { col = 3, row = 1 }, start = { col = 1, row = 1 } } (Canonical.List [])
                  , Ok (located { end = { col = 3, row = 1 }, start = { col = 1, row = 1 } } ( Typed.List [], List (Var 1) ))
                  )
                , ( "one item"
                  , located { end = { col = 4, row = 1 }, start = { col = 1, row = 1 } } (Canonical.List [ located { end = { col = 3, row = 1 }, start = { col = 2, row = 1 } } (Canonical.Literal (Literal.Int 1)) ])
                  , Ok (located { end = { col = 4, row = 1 }, start = { col = 1, row = 1 } } ( Typed.List [ located { end = { col = 3, row = 1 }, start = { col = 2, row = 1 } } ( Typed.Literal (Literal.Int 1), Int ) ], List Int ))
                  )
                , ( "more items"
                  , located { end = { col = 8, row = 1 }, start = { col = 1, row = 1 } } (Canonical.List [ located { end = { col = 3, row = 1 }, start = { col = 2, row = 1 } } (Canonical.Literal (Literal.Int 1)), located { end = { col = 5, row = 1 }, start = { col = 4, row = 1 } } (Canonical.Literal (Literal.Int 2)), located { end = { col = 7, row = 1 }, start = { col = 6, row = 1 } } (Canonical.Literal (Literal.Int 3)) ])
                  , Ok (located { end = { col = 8, row = 1 }, start = { col = 1, row = 1 } } ( Typed.List [ located { end = { col = 3, row = 1 }, start = { col = 2, row = 1 } } ( Typed.Literal (Literal.Int 1), Int ), located { end = { col = 5, row = 1 }, start = { col = 4, row = 1 } } ( Typed.Literal (Literal.Int 2), Int ), located { end = { col = 7, row = 1 }, start = { col = 6, row = 1 } } ( Typed.Literal (Literal.Int 3), Int ) ], List Int ))
                  )
                , ( "different types"
                  , located { end = { col = 10, row = 1 }, start = { col = 1, row = 1 } } (Canonical.List [ located { end = { col = 3, row = 1 }, start = { col = 2, row = 1 } } (Canonical.Literal (Literal.Int 1)), located { end = { col = 7, row = 1 }, start = { col = 4, row = 1 } } (Canonical.Literal (Literal.String "2")) ])
                  , Err (Error.TypeMismatch Type.Int Type.String)
                  )
                , ( "more items with different types"
                  , located { end = { col = 10, row = 1 }, start = { col = 1, row = 1 } } (Canonical.List [ located { end = { col = 3, row = 1 }, start = { col = 2, row = 1 } } (Canonical.Literal (Literal.Bool True)), located { end = { col = 7, row = 1 }, start = { col = 4, row = 1 } } (Canonical.Literal (Literal.String "two")), located { end = { col = 7, row = 1 }, start = { col = 4, row = 1 } } (Canonical.Literal (Literal.Int 3)) ])
                  , Err (Error.TypeMismatch Type.Bool Type.String)
                  )
                ]
              )
            , ( "tuple"
              , [ ( "items with the same types"
                  , located { end = { col = 3, row = 1 }, start = { col = 1, row = 1 } } (Canonical.Tuple (located { end = { col = 3, row = 1 }, start = { col = 1, row = 1 } } (Canonical.Literal (Literal.String "Hello"))) (located { end = { col = 3, row = 1 }, start = { col = 1, row = 1 } } (Canonical.Literal (Literal.String "Elm"))))
                  , Ok (located { end = { col = 3, row = 1 }, start = { col = 1, row = 1 } } ( Typed.Tuple (located { end = { col = 3, row = 1 }, start = { col = 1, row = 1 } } ( Typed.Literal (Literal.String "Hello"), String )) (located { end = { col = 3, row = 1 }, start = { col = 1, row = 1 } } ( Typed.Literal (Literal.String "Elm"), String )), Tuple String String ))
                  )
                , ( "items of different types"
                  , located { end = { col = 3, row = 1 }, start = { col = 1, row = 1 } } (Canonical.Tuple (located { end = { col = 3, row = 1 }, start = { col = 1, row = 1 } } (Canonical.Literal (Literal.Bool True))) (located { end = { col = 3, row = 1 }, start = { col = 1, row = 1 } } (Canonical.Literal (Literal.Int 1))))
                  , Ok (located { end = { col = 3, row = 1 }, start = { col = 1, row = 1 } } ( Typed.Tuple (located { end = { col = 3, row = 1 }, start = { col = 1, row = 1 } } ( Typed.Literal (Literal.Bool True), Bool )) (located { end = { col = 3, row = 1 }, start = { col = 1, row = 1 } } ( Typed.Literal (Literal.Int 1), Int )), Tuple Bool Int ))
                  )
                ]
              )
            , ( "tuple3"
              , [ ( "same types"
                  , located { end = { col = 3, row = 1 }, start = { col = 1, row = 1 } } (Canonical.Tuple3 (located { end = { col = 3, row = 1 }, start = { col = 1, row = 1 } } (Canonical.Literal (Literal.String "FP"))) (located { end = { col = 3, row = 1 }, start = { col = 1, row = 1 } } (Canonical.Literal (Literal.String "is"))) (located { end = { col = 3, row = 1 }, start = { col = 1, row = 1 } } (Canonical.Literal (Literal.String "good"))))
                  , Ok (located { end = { col = 3, row = 1 }, start = { col = 1, row = 1 } } ( Typed.Tuple3 (located { end = { col = 3, row = 1 }, start = { col = 1, row = 1 } } ( Typed.Literal (Literal.String "FP"), String )) (located { end = { col = 3, row = 1 }, start = { col = 1, row = 1 } } ( Typed.Literal (Literal.String "is"), String )) (located { end = { col = 3, row = 1 }, start = { col = 1, row = 1 } } ( Typed.Literal (Literal.String "good"), String )), Tuple3 String String String ))
                  )
                , ( "different types"
                  , located { end = { col = 3, row = 1 }, start = { col = 1, row = 1 } } (Canonical.Tuple3 (located { end = { col = 3, row = 1 }, start = { col = 1, row = 1 } } (Canonical.Literal (Literal.Bool True))) (located { end = { col = 3, row = 1 }, start = { col = 1, row = 1 } } (Canonical.Literal (Literal.Int 1))) (located { end = { col = 3, row = 1 }, start = { col = 1, row = 1 } } (Canonical.Literal (Literal.Char 'h'))))
                  , Ok (located { end = { col = 3, row = 1 }, start = { col = 1, row = 1 } } ( Typed.Tuple3 (located { end = { col = 3, row = 1 }, start = { col = 1, row = 1 } } ( Typed.Literal (Literal.Bool True), Bool )) (located { end = { col = 3, row = 1 }, start = { col = 1, row = 1 } } ( Typed.Literal (Literal.Int 1), Int )) (located { end = { col = 3, row = 1 }, start = { col = 1, row = 1 } } ( Typed.Literal (Literal.Char 'h'), Char )), Tuple3 Bool Int Char ))
                  )
                ]
              )
            ]
        )


typeToString : Test
typeToString =
    let
        toStringOnce : Type -> String
        toStringOnce type_ =
            type_
                |> Type.toString Type.emptyState
                |> Tuple.first

        runTest ( description, input, output ) =
            test description <|
                \() ->
                    toStringOnce input
                        |> Expect.equal output

        runEqual ( description, input, output ) =
            test description <|
                \() ->
                    Expect.equal input output
    in
    describe "Type.toString"
        [ describe "list"
            [ runTest
                ( "empty list"
                , Type.List (Type.Var 0)
                , "List a"
                )
            , runTest
                ( "one item in list"
                , Type.List Type.Bool
                , "List Bool"
                )
            , runTest
                ( "list of list of String"
                , Type.List (Type.List Type.String)
                , "List (List String)"
                )
            ]
        , describe "lambda"
            [ runTest
                ( "function with one param"
                , Type.Function (Type.Var 99) Type.Int
                , "a -> Int"
                )
            , runTest
                ( "function with two params"
                , Type.Function (Type.Var 0) (Type.Function (Type.Var 1) (Type.Var 1))
                , "a -> b -> b"
                )
            , runTest
                ( "function as param"
                , Type.Function (Type.Function (Type.Var 9) (Type.Var 9)) (Type.Var 0)
                , "(a -> a) -> b"
                )
            , runTest
                ( "list of functions"
                , Type.List (Type.Function (Type.Var 0) (Type.Var 0))
                , "List (a -> a)"
                )
            ]
        , describe "edges"
            [ runEqual
                ( "Var number doesn't count"
                , toStringOnce <| Type.List (Type.Var 0)
                , toStringOnce <| Type.List (Type.Var 1)
                )
            , runEqual
                ( "TypeMismatch types share vars index"
                , Error.toString
                    (Error.TypeError
                        (Error.TypeMismatch
                            (Type.Function (Type.List (Type.Var 0)) (Type.Var 1))
                            (Type.Function (Type.List (Type.Var 1)) (Type.Var 0))
                        )
                    )
                , "The types `(List a) -> b` and `(List b) -> a` don't match."
                )
            ]
        , describe "tuples"
            [ runTest
                ( "tuple with two literals"
                , Type.Tuple Type.Int Type.String
                , "( Int, String )"
                )
            , runTest
                ( "tuple with two params"
                , Type.Tuple (Type.Var 0) (Type.Var 1)
                , "( a, b )"
                )
            , runTest
                ( "tuple with tree params"
                , Type.Tuple3 (Type.Var 0) (Type.Var 1) (Type.Var 2)
                , "( a, b, c )"
                )
            ]
        ]


niceVarName : Test
niceVarName =
    let
        runTest ( input, output ) =
            test output <|
                \() ->
                    Type.niceVarName input
                        |> Expect.equal output
    in
    describe "Type.niceVarName" <|
        List.map runTest
            [ ( 0, "a" )
            , ( 1, "b" )
            , ( 2, "c" )
            , ( 25, "z" )

            --
            , ( 26, "a1" )
            , ( 49, "x1" )
            , ( 50, "y1" )
            , ( 51, "z1" )

            --
            , ( 52, "a2" )
            , ( 77, "z2" )

            --
            , ( 259, "z9" )
            , ( 260, "a10" )
            ]
