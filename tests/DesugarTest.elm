module DesugarTest exposing (desugarTest)

--import Elm.Data.Type as Type exposing (Type(..))

import Dict
import Elm.AST.Canonical as Canonical
import Elm.AST.Canonical.Unwrapped as CanonicalU
import Elm.AST.Frontend as Frontend
import Elm.Data.Exposing as Exposing
import Elm.Data.Module as Module
import Expect
import Stage.Desugar as Desugar
import Test exposing (Test, describe, test)
import TestHelpers exposing (located)


desugarTest : Test
desugarTest =
    describe "Stage.Desugar"
        [ test "desugar \\a b -> a + b into \\a -> \\b -> a + b " <|
            \_ ->
                frontendLambda "a" "b"
                    |> Desugar.desugarExpr Dict.empty dummyModule
                    |> Result.map Canonical.unwrap
                    |> Expect.equal
                        (Ok <| canonicalLambda "a" "b")
        ]


{-| `frontendLambda "a" "b"` builds `\a b -> a + b`.
-}
frontendLambda : String -> String -> Frontend.LocatedExpr
frontendLambda arg1 arg2 =
    located <|
        Frontend.Lambda
            { arguments = [ arg1, arg2 ]
            , body =
                located <|
                    Frontend.Plus
                        (located <| Frontend.Argument arg1)
                        (located <| Frontend.Argument arg2)
            }


{-| `canonicalLambda "a" "b"` builds `\a -> \b -> a + b`.
-}
canonicalLambda : String -> String -> CanonicalU.Expr
canonicalLambda arg1 arg2 =
    CanonicalU.Lambda
        { argument = arg1
        , body =
            CanonicalU.Lambda
                { argument = arg2
                , body =
                    CanonicalU.Plus
                        (CanonicalU.Argument arg1)
                        (CanonicalU.Argument arg2)
                }
        }


dummyModule : Module.Module a
dummyModule =
    { imports = Dict.empty
    , name = "DummyModule"
    , filePath = "/"
    , declarations = Dict.empty
    , type_ = Module.PlainModule
    , exposing_ = Exposing.ExposingSome []
    }
