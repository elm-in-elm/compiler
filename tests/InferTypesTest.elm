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
              , [ ( "different types"
                  , Canonical.List [ Canonical.Literal (Int 1), Canonical.Literal (String "two") ]
                  , Err (Error.TypeMismatch Type.Int Type.String)
                  )

                --, ( "more items types"
                --  , Ok (Canonical.List [(Canonical.Literal (Int 2),Var 0),(Canonical.Literal (Int 1),Var 1)],Var 2)
                --  , Err (Error.TypeMismatch (Type.Int) (Type.Int))
                --  )
                --, ( "same types"
                --  , Ok (Canonical.List [(Canonical.Literal (Int 2),Var 0),(Canonical.Literal (Int 1),Var 1)],Var 2)
                --  , Ok (Canonical.List [(Canonical.Literal (Int 2),Var 0),(Canonical.Literal (Int 1),Var 1)],Var 2)
                --  )
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
