module AST.Backend exposing
    ( Expr
    , Graph
    , ProjectFields
    )

import AST.Typed as Typed
import Common.Types exposing (TopLevelDeclaration)
import Graph


type alias ProjectFields =
    { programGraph : Graph }


type alias Expr =
    Typed.Expr


type alias Graph =
    Graph.Graph (TopLevelDeclaration Expr) ()
