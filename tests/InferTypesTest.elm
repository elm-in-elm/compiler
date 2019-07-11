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
        runSection ( description, tests ) =
            describe description
                (List.map runTest tests)

        runTest ( description, input, output ) =
            test description <|
                \() ->
                    Type.toString input
                        |> Expect.equal output
    in
    describe "Type.toString"
        (List.map runSection
            [ ( "list"
              , [ ( "empty list"
                  , Type.List (Type.Var 0)
                  , "List a"
                  )
                , ( "one item in list"
                  , Type.List Type.Bool
                  , "List Bool"
                  )
                ]
              )
            , ( "lambda"
              , [ ( "function with one param"
                  , Type.Function (Type.Var 0) Type.Int
                  , "a -> Int"
                  )
                , ( "function with two params"
                  , Type.Function (Type.Var 0) (Type.Function (Type.Var 1) (Type.Var 1))
                  , "a -> b -> b"
                  )
                , ( "function as params"
                  , Type.Function (Type.Function (Type.Var 1) (Type.Var 1)) (Type.Var 0)
                  , "(b -> b) -> a"
                  )
                , ( "list of functions"
                  , Type.List (Type.Function (Type.Var 0) (Type.Var 0))
                  , "List (a -> a)"
                  )
                ]
              )
            ]
        )
