module OptimizeTest exposing (optimize)

import AST.Common.Literal exposing (Literal(..))
import AST.Common.Located as Located exposing (Located)
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
import TestHelpers
    exposing
        ( located
        , typedBool
        , typedInt
        )


optimize : Test
optimize =
    describe "Stage.Optimize"
        [ let
            runTest : ( String, Typed.LocatedExpr, Typed.LocatedExpr ) -> Test
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
                      , located
                            ( Plus
                                (typedInt 2)
                                (typedInt 5)
                            , Type.Int
                            )
                      , typedInt 7
                      )
                    , ( "doesn't work if left is not int"
                      , located
                            ( Plus
                                (located ( Argument (VarName "x"), Type.Int ))
                                (typedInt 5)
                            , Type.Int
                            )
                      , located
                            ( Plus
                                (located ( Argument (VarName "x"), Type.Int ))
                                (typedInt 5)
                            , Type.Int
                            )
                      )
                    , ( "doesn't work if right is not int"
                      , located
                            ( Plus
                                (typedInt 5)
                                (located ( Argument (VarName "x"), Type.Int ))
                            , Type.Int
                            )
                      , located
                            ( Plus
                                (typedInt 5)
                                (located ( Argument (VarName "x"), Type.Int ))
                            , Type.Int
                            )
                      )
                    ]
                )
            , describe "optimizeIfLiteralBool"
                (List.map runTest
                    [ ( "folds to then if true"
                      , located
                            ( If
                                { test = typedBool True
                                , then_ = typedInt 42
                                , else_ = typedInt 0
                                }
                            , Type.Int
                            )
                      , typedInt 42
                      )
                    , ( "folds to else if false"
                      , located
                            ( If
                                { test = typedBool False
                                , then_ = typedInt 0
                                , else_ = typedInt 42
                                }
                            , Type.Int
                            )
                      , typedInt 42
                      )
                    , ( "doesn't work if the bool is not literal"
                      , located
                            ( If
                                { test = located ( Argument (VarName "x"), Type.Bool )
                                , then_ = typedInt 0
                                , else_ = typedInt 42
                                }
                            , Type.Int
                            )
                      , located
                            ( If
                                { test = located ( Argument (VarName "x"), Type.Bool )
                                , then_ = typedInt 0
                                , else_ = typedInt 42
                                }
                            , Type.Int
                            )
                      )
                    ]
                )
            ]
        ]
