module AST.Canonical exposing (Expr)

import AST.Frontend as Frontend


{-| TODO make it different (let only has one binding etc.)
-}
type alias Expr =
    Frontend.Expr
