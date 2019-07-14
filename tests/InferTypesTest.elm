module InferTypesTest exposing (typeInference, typeToString)

import AST.Canonical as Canonical
import AST.Common.Literal exposing (Literal(..))
import AST.Common.Type as Type exposing (Type)
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
                  , Canonical.List []
                  , Ok ( Typed.List [], Type.List (Type.Var 1) )
                  )
                , ( "one item"
                  , Canonical.List [ Canonical.Literal (Bool True) ]
                  , Ok ( Typed.List [ ( Typed.Literal (Bool True), Type.Bool ) ], Type.List Type.Bool )
                  )
                , ( "more items"
                  , Canonical.List [ Canonical.Literal (Int 1), Canonical.Literal (Int 2), Canonical.Literal (Int 3) ]
                  , Ok ( Typed.List [ ( Typed.Literal (Int 1), Type.Int ), ( Typed.Literal (Int 2), Type.Int ), ( Typed.Literal (Int 3), Type.Int ) ], Type.List Type.Int )
                  )
                , ( "different types"
                  , Canonical.List [ Canonical.Literal (Int 1), Canonical.Literal (String "two") ]
                  , Err (Error.TypeMismatch Type.Int Type.String)
                  )
                , ( "more items with different types"
                  , Canonical.List [ Canonical.Literal (Bool True), Canonical.Literal (String "two"), Canonical.Literal (Int 3) ]
                  , Err (Error.TypeMismatch Type.Bool Type.String)
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
                , "(Int,String)"
                )
            , runTest
                ( "tuple with two params"
                , Type.Tuple (Type.Var 0) (Type.Var 1)
                , "(a,b)"
                )
            , runTest
                ( "tuple with tree params"
                , Type.Tuple3 (Type.Var 0) (Type.Var 1) (Type.Var 2)
                , "(a,b,c)"
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
