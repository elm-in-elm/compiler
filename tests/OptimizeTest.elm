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


typed : Typed.Expr -> Typed.LocatedExpr
typed expr =
    Located.located
        -- position do not matters in optimize
        { start = { row = 0, col = 0 }, end = { row = 0, col = 0 } }
        expr


typedInt : Int -> Typed.LocatedExpr
typedInt int =
    typed ( Literal (Int int), Type.Int )


typedBool : Bool -> Typed.LocatedExpr
typedBool bool =
    typed ( Literal (Bool bool), Type.Bool )


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
                      , typed
                            ( Plus
                                (typedInt 2)
                                (typedInt 5)
                            , Type.Int
                            )
                      , typedInt 7
                      )
                    , ( "doesn't work if left is not int"
                      , typed
                            ( Plus
                                (typed ( Argument (VarName "x"), Type.Int ))
                                (typedInt 5)
                            , Type.Int
                            )
                      , typed
                            ( Plus
                                (typed ( Argument (VarName "x"), Type.Int ))
                                (typedInt 5)
                            , Type.Int
                            )
                      )
                    , ( "doesn't work if right is not int"
                      , typed
                            ( Plus
                                (typedInt 5)
                                (typed ( Argument (VarName "x"), Type.Int ))
                            , Type.Int
                            )
                      , typed
                            ( Plus
                                (typedInt 5)
                                (typed ( Argument (VarName "x"), Type.Int ))
                            , Type.Int
                            )
                      )
                    ]
                )
            , describe "optimizeIfLiteralBool"
                (List.map runTest
                    [ ( "folds to then if true"
                      , typed
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
                      , typed
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
                      , typed
                            ( If
                                { test = typed ( Argument (VarName "x"), Type.Bool )
                                , then_ = typedInt 0
                                , else_ = typedInt 42
                                }
                            , Type.Int
                            )
                      , typed
                            ( If
                                { test = typed ( Argument (VarName "x"), Type.Bool )
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
