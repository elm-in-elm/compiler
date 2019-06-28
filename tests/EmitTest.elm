module EmitTest exposing (javascript)

import Expect exposing (Expectation)
import Stage.Emit.JavaScript as JS
import Test exposing (Test, describe, test, todo)


javascript : Test
javascript =
    describe "Stage.Emit.JavaScript"
        [ describe "emitExpr"
            [ todo "add tests"
            ]
        , describe "emitTopLevelDeclaration"
            [ todo "add tests"
            ]
        , describe "mangleQualifiedVar"
            [ todo "add tests"
            ]
        ]
