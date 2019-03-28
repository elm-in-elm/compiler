module Stage.Emit exposing (emit)

import AST.Backend as Backend
import AST.Canonical exposing (Expr(..))
import AST.Common exposing (Literal(..))
import Common
import Common.Types
    exposing
        ( FileContents(..)
        , ModuleName(..)
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
        -- ignore the untraversed path (dead code elimination!)
        |> Tuple.first


findMains : Backend.Graph -> ModuleName -> List Int
findMains graph mainModuleName =
    {- TODO This will currently only find one main (we don't support more of those,
       see TODOs in Types.Project), so the name of the fn is misleading.
       The arguments to this fn will probably have to change (multiple main module names).
    -}
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
emitTopLevelDeclaration { module_, name, body } =
    "const " ++ mangleName module_ name ++ " = " ++ emitExpr body ++ ";"


emitExpr : Backend.Expr -> String
emitExpr expr =
    case expr of
        Literal (LInt int) ->
            String.fromInt int

        Var ( moduleName, varName ) ->
            mangleName moduleName varName

        Plus e1 e2 ->
            "(" ++ emitExpr e1 ++ " + " ++ emitExpr e2 ++ ")"

        Lambda { argName, body } ->
            "((" ++ Common.varNameToString argName ++ ") => " ++ emitExpr body ++ ")"


mangleName : ModuleName -> VarName -> String
mangleName moduleName (VarName varName) =
    -- TODO probably mangle var name too... what are the rules?
    mangleModuleName moduleName ++ "$" ++ varName


mangleModuleName : ModuleName -> String
mangleModuleName (ModuleName moduleName) =
    -- TODO what does the original Elm compiler do?
    String.replace "." "$" moduleName
