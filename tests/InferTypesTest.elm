module InferTypesTest exposing (typeInference)

import AST.Canonical as Canonical
import AST.Common.Literal exposing (Literal(..))
import AST.Common.Type as Type exposing (Type)
import AST.Typed as Typed
import Common
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
                        |> expectEqualInferType output
    in
    describe "Stage.InferType"
        (List.map runSection
            [ ( "list"
              , [ ( "empty list"
                  , Canonical.List []
                  , Ok ( Typed.List [], Type.Var 1 )
                  )
                , ( "one item"
                  , Canonical.List [ Canonical.Literal (Bool True) ]
                  , Ok ( Typed.List [ ( Typed.Literal (Bool True), Type.Bool ) ], Type.Var 2 )
                  )
                , ( "more items"
                  , Canonical.List [ Canonical.Literal (Int 1), Canonical.Literal (Int 2) ]
                  , Ok ( Typed.List [ ( Typed.Literal (Int 2), Type.Int ), ( Typed.Literal (Int 1), Type.Int ) ], Type.Var 3 )
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


expectEqualInferType :
    Result TypeError Typed.Expr
    -> Result TypeError Typed.Expr
    -> Expectation
expectEqualInferType expected actual =
    if actual == expected then
        Expect.pass

    else
        case actual of
            Err typeError ->
                Expect.fail (Error.toString (Error.TypeError typeError))

            _ ->
                Expect.equal expected actual
