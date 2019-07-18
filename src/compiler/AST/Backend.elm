module AST.Backend exposing
    ( Graph
    , LocatedExpr
    , ProjectFields
    )

import AST.Typed as Typed
import Common.Types exposing (TopLevelDeclaration)
import Graph


type alias ProjectFields =
    { programGraph : Graph }


type alias LocatedExpr =
    Typed.LocatedExpr


type alias Graph =
    Graph.Graph (TopLevelDeclaration LocatedExpr) ()
