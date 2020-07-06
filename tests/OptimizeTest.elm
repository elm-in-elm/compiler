module OptimizeTest exposing (optimize)

import Elm.AST.Typed as Typed exposing (Expr_(..))
import Elm.Data.Type as Type exposing (TypeOrId(..))
import Expect
import Stage.Optimize
import Test exposing (Test, describe, test)
import TestHelpers
    exposing
        ( located
        , typedBool
        , typedInt
        , typedIntList
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
                            , Type Type.Int
                            )
                      , typedInt 7
                      )
                    , ( "doesn't work if left is not int"
                      , located
                            ( Plus
                                (located ( Argument "x", Type Type.Int ))
                                (typedInt 5)
                            , Type Type.Int
                            )
                      , located
                            ( Plus
                                (located ( Argument "x", Type Type.Int ))
                                (typedInt 5)
                            , Type Type.Int
                            )
                      )
                    , ( "doesn't work if right is not int"
                      , located
                            ( Plus
                                (typedInt 5)
                                (located ( Argument "x", Type Type.Int ))
                            , Type Type.Int
                            )
                      , located
                            ( Plus
                                (typedInt 5)
                                (located ( Argument "x", Type Type.Int ))
                            , Type Type.Int
                            )
                      )
                    ]
                )
            , describe "optimizeCons"
                (List.map runTest
                    [ ( "works with one value"
                      , located
                            ( Cons
                                (typedInt 1)
                                (typedIntList [ 2, 3 ])
                            , Type Type.Int
                            )
                      , typedIntList [ 1, 2, 3 ]
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
                            , Type Type.Int
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
                            , Type Type.Int
                            )
                      , typedInt 42
                      )
                    , ( "doesn't work if the bool is not literal"
                      , located
                            ( If
                                { test = located ( Argument "x", Type Type.Bool )
                                , then_ = typedInt 0
                                , else_ = typedInt 42
                                }
                            , Type Type.Int
                            )
                      , located
                            ( If
                                { test = located ( Argument "x", Type Type.Bool )
                                , then_ = typedInt 0
                                , else_ = typedInt 42
                                }
                            , Type Type.Int
                            )
                      )
                    ]
                )
            ]
        ]
