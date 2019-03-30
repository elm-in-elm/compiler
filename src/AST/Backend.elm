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
    {- TODO is it worth it to concatenate the single-arg lambdas back to a multi-arg lambda,
       so that we emit eg. `(a,b) => a+b` instead of `(a) => (b) => a + b`?
    -}
    Canonical.Expr


type alias Graph =
    Graph.Graph (TopLevelDeclaration Expr) ()
