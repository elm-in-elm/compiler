module OptimizeTest exposing (optimize)

import AST.Common.Literal exposing (Literal(..))
import AST.Common.Type as Type
import AST.Typed as Typed exposing (Expr_(..))
import Common
import Common.Types
    exposing
        ( ModuleName(..)
        , TopLevelDeclaration
        , VarName(..)
        )
import Dict.Any
import Expect exposing (Expectation)
import Stage.Optimize
import Test exposing (Test, describe, test, todo)


optimize : Test
optimize =
    describe "Stage.Optimize"
        [ let
            runTest : ( String, Typed.Expr, Typed.Expr ) -> Test
            runTest ( description, input, output ) =
                test description <|
                    \() ->
                        input
                            |> Stage.Optimize.optimizeExpr
                            |> Expect.equal output
          in
          describe "optimizeExpr"
            [ describe "optimizePlus"
                (List.map runTest
                    [ ( "works with two literal ints"
                      , ( Plus
                            ( Literal (Int 2), Type.Int )
                            ( Literal (Int 5), Type.Int )
                        , Type.Int
                        )
                      , ( Literal (Int 7), Type.Int )
                      )
                    , ( "doesn't work if left is not int"
                      , ( Plus
                            ( Argument (VarName "x"), Type.Int )
                            ( Literal (Int 5), Type.Int )
                        , Type.Int
                        )
                      , ( Plus
                            ( Argument (VarName "x"), Type.Int )
                            ( Literal (Int 5), Type.Int )
                        , Type.Int
                        )
                      )
                    , ( "doesn't work if right is not int"
                      , ( Plus
                            ( Literal (Int 5), Type.Int )
                            ( Argument (VarName "x"), Type.Int )
                        , Type.Int
                        )
                      , ( Plus
                            ( Literal (Int 5), Type.Int )
                            ( Argument (VarName "x"), Type.Int )
                        , Type.Int
                        )
                      )
                    ]
                )
            , describe "optimizeIfLiteralBool"
                (List.map runTest
                    [ ( "folds to then if true"
                      , ( If
                            { test = ( Literal (Bool True), Type.Bool )
                            , then_ = ( Literal (Int 42), Type.Int )
                            , else_ = ( Literal (Int 0), Type.Int )
                            }
                        , Type.Int
                        )
                      , ( Literal (Int 42), Type.Int )
                      )
                    , ( "folds to else if false"
                      , ( If
                            { test = ( Literal (Bool False), Type.Bool )
                            , then_ = ( Literal (Int 0), Type.Int )
                            , else_ = ( Literal (Int 42), Type.Int )
                            }
                        , Type.Int
                        )
                      , ( Literal (Int 42), Type.Int )
                      )
                    , ( "doesn't work if the bool is not literal"
                      , ( If
                            { test = ( Argument (VarName "x"), Type.Bool )
                            , then_ = ( Literal (Int 0), Type.Int )
                            , else_ = ( Literal (Int 42), Type.Int )
                            }
                        , Type.Int
                        )
                      , ( If
                            { test = ( Argument (VarName "x"), Type.Bool )
                            , then_ = ( Literal (Int 0), Type.Int )
                            , else_ = ( Literal (Int 42), Type.Int )
                            }
                        , Type.Int
                        )
                      )
                    ]
                )
            ]
        ]
