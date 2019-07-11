module InferTypesTest exposing (typeInference)

import AST.Canonical as Canonical
import AST.Common.Literal exposing (Literal(..))
import AST.Common.Type as Type exposing (Type)
import AST.Typed as Typed
import Common
import Dict.Any
import Error exposing (TypeError)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Stage.InferTypes
import Test exposing (Test, describe, fuzz, test)


typeInference : Test
typeInference =
    describe "Stage.InferType"
        [ runSection "list"
            [ ( "empty list"
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
        , describe "fuzz exprInfer"
            [-- TODO: Fill this with calls to fuzzExpr
            ]
        ]


runSection : String -> List ( String, Canonical.Expr, Result Error.TypeError Typed.Expr ) -> Test
runSection description tests =
    describe description
        (List.map runTest tests)


runTest : ( String, Canonical.Expr, Result Error.TypeError Typed.Expr ) -> Test
runTest ( description, input, output ) =
    test description <|
        \() ->
            Stage.InferTypes.inferExpr input
                |> Expect.equal output


fuzzExpr : ( String, Type ) -> Test
fuzzExpr ( description, typeWanted ) =
    fuzz (randomExprFromType typeWanted) description <|
        \input ->
            Stage.InferTypes.inferExpr input
                |> Result.map Tuple.second
                |> Expect.equal (Ok typeWanted)


randomExprFromType : Type -> Fuzzer Canonical.Expr
randomExprFromType _ =
    Debug.todo "randomExprFromType"
