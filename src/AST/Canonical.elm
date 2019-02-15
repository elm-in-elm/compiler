module AST.Canonical exposing
    ( Expr
    , ProjectFields
    )

import AST.Frontend as Frontend
import Common.Types exposing (Modules)


type alias ProjectFields =
    { program : Modules Expr }


{-| TODO make it different (let only has one binding etc.)
-}
type alias Expr =
    Frontend.Expr
