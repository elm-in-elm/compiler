module AST.Backend exposing
    ( Expr
    , Graph
    , ProjectFields
    )

import AST.Canonical as Canonical
import Common.Types exposing (TopLevelDeclaration)
import Graph


type alias ProjectFields =
    { programGraph : Graph }


type alias Expr =
    Canonical.Expr


type alias Graph =
    Graph.Graph (TopLevelDeclaration Expr) ()
