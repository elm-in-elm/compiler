module Stage.Emit exposing (emit)

-- TODO figure out module headers, compiling to one vs many files

import AST.Backend as Backend
import AST.Common.Literal exposing (Literal(..))
import AST.Typed exposing (Expr_(..))
import Common.Types
    exposing
        ( FileContents(..)
        , ModuleName(..)
        , Project
        , ProjectToEmit
        , TopLevelDeclaration
        , VarName(..)
        )
import Graph
import Stage.Emit.JavaScript as JS


emit : Project Backend.ProjectFields -> ProjectToEmit
emit project =
    project
        |> findPathToMain
        |> List.map JS.emitTopLevelDeclaration
        |> String.join "\n"
        |> FileContents
        |> ProjectToEmit


{-| We want to be able to emit `main`. We only emit what's needed for that.
Taken from the example in elm-community/graph README :sweat\_smile:
-}
findPathToMain : Project Backend.ProjectFields -> List (TopLevelDeclaration Backend.Expr)
findPathToMain { programGraph, mainModuleName } =
    Graph.guidedDfs
        Graph.alongIncomingEdges
        -- which edges to follow
        (Graph.onDiscovery
            (\ctx list ->
                -- append node labels on discovery
                ctx.node.label :: list
            )
        )
        -- start with "main" function
        (findMain programGraph mainModuleName)
        -- we could make sure some declaration gets always emmited by adding it here
        []
        programGraph
        -- ignore the untraversed path (dead code elimination!)
        |> Tuple.first


findMain : Backend.Graph -> ModuleName -> List Int
findMain graph mainModuleName =
    Graph.nodes graph
        |> List.filterMap
            (\{ id, label } ->
                if
                    (label.name == VarName "main")
                        && (label.module_ == mainModuleName)
                then
                    Just id

                else
                    Nothing
            )
