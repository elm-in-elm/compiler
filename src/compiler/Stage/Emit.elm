module Stage.Emit exposing (emit)

-- TODO figure out module headers, compiling to one vs many files

import AST.Backend as Backend
import AST.Common.Literal exposing (Literal(..))
import AST.Typed exposing (Expr_(..))
import Data.Declaration exposing (Declaration)
import Data.FileContents as FileContents
import Data.ModuleName exposing (ModuleName)
import Data.Project exposing (Project, ProjectToEmit)
import Data.VarName as VarName
import Graph
import Stage.Emit.JavaScript as JS


emit : Project Backend.ProjectFields -> ProjectToEmit
emit project =
    project
        |> findPathToMain
        |> List.map JS.emitDeclaration
        |> String.join "\n"
        |> FileContents.fromString
        |> ProjectToEmit


{-| We want to be able to emit `main`. We only emit what's needed for that.
Taken from the example in elm-community/graph README :sweat\_smile:
-}
findPathToMain : Project Backend.ProjectFields -> List (Declaration Backend.LocatedExpr)
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
                    (label.name == VarName.fromString "main")
                        && (label.module_ == mainModuleName)
                then
                    Just id

                else
                    Nothing
            )
