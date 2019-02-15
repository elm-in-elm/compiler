module Stage.Emit exposing (emit)

import AST.Backend as Backend
import AST.Frontend
    exposing
        ( Expr(..)
        , Literal(..)
        )
import Common.Types
    exposing
        ( FileContents(..)
        , ModuleName
        , Project
        , ProjectToEmit
        , TopLevelDeclaration
        , VarName(..)
        )
import Dict.Any
import Error exposing (EmitError(..), Error(..))
import Graph


emit : Project Backend.ProjectFields -> Result Error ProjectToEmit
emit project =
    project
        |> findPathToMain
        |> List.map emitTopLevelDeclaration
        |> String.join "\n"
        |> FileContents
        |> ProjectToEmit
        |> Ok


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
        -- start with "main" function(s)
        (findMains programGraph mainModuleName)
        -- we could make sure some declaration gets always emmited by adding it here
        []
        programGraph
        |> Tuple.first



-- ignore the untraversed path (dead code elimination!)


findMains : Backend.Graph -> ModuleName -> List Int
findMains graph mainModuleName =
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


emitTopLevelDeclaration : TopLevelDeclaration Backend.Expr -> String
emitTopLevelDeclaration { name, body } =
    let
        (VarName varName) =
            name
    in
    "const " ++ varName ++ " = " ++ emitExpr body ++ ";"


emitExpr : Backend.Expr -> String
emitExpr expr =
    case expr of
        Literal (LInt int) ->
            String.fromInt int
