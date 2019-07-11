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
            [ fuzzExpr "Int" Type.Int
            , fuzzExpr "Float" Type.Float
            , fuzzExpr "Bool" Type.Bool
            , fuzzExpr "Char" Type.Char
            , fuzzExpr "String" Type.String
            , fuzzExpr "()" Type.Unit
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


fuzzExpr : String -> Type -> Test
fuzzExpr description typeWanted =
    fuzz (randomExprFromType typeWanted) description <|
        \input ->
            Stage.InferTypes.inferExpr input
                |> Result.map Tuple.second
                |> Expect.equal (Ok typeWanted)


randomExprFromType : Type -> Fuzzer Canonical.Expr
randomExprFromType targetType =
    let
        cannotFuzz () =
            Debug.todo <| "Cannot fuzz " ++ Type.toString targetType ++ " expressions."
    in
    case targetType of
        Type.Int ->
            Fuzz.map Canonical.Literal <| Fuzz.map Int <| Fuzz.int

        Type.Float ->
            Fuzz.map Canonical.Literal <| Fuzz.map Float <| Fuzz.float

        Type.Bool ->
            Fuzz.map Canonical.Literal <| Fuzz.map Bool <| Fuzz.bool

        Type.Char ->
            Fuzz.map Canonical.Literal <| Fuzz.map Char <| Fuzz.map Char.fromCode <| Fuzz.intRange 0 0x0010FFFF

        Type.String ->
            Fuzz.map Canonical.Literal <| Fuzz.map String <| Fuzz.map String.fromList <| Fuzz.map (List.map Char.fromCode) <| Fuzz.list <| Fuzz.intRange 0 0x0010FFFF

        Type.Unit ->
            Fuzz.constant Canonical.Unit

        _ ->
            cannotFuzz ()
