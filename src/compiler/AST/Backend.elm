module AST.Backend exposing
    ( Graph
    , LocatedExpr
    , ProjectFields
    )

import AST.Typed as Typed
import Data.Declaration exposing (Declaration)
import Graph


type alias ProjectFields =
    { programGraph : Graph }


type alias LocatedExpr =
    Typed.LocatedExpr


type alias Graph =
    Graph.Graph (Declaration LocatedExpr) ()
