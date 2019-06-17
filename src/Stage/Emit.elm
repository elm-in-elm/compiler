module Stage.Emit exposing (emit)

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
    "const "
        ++ mangleQualifiedVar module_ name
        ++ " = "
        ++ emitExpr body
        ++ ";"


emitExpr : Backend.Expr -> String
emitExpr ( expr, type_ ) =
    case expr of
        Literal (Int int) ->
            String.fromInt int

        Literal (Char char) ->
            "\"" ++ String.fromChar char ++ "\""

        Literal (String string) ->
            "\"" ++ string ++ "\""

        Literal (Bool bool) ->
            if bool then
                "true"

            else
                "false"

        Var { qualifier, name } ->
            mangleQualifiedVar qualifier name

        Argument argument ->
            mangleVarName argument

        Plus e1 e2 ->
            "(" ++ emitExpr e1 ++ " + " ++ emitExpr e2 ++ ")"

        Lambda { argument, body } ->
            "((" ++ mangleVarName argument ++ ") => " ++ emitExpr body ++ ")"

        Call { fn, argument } ->
            "((" ++ emitExpr fn ++ ")(" ++ emitExpr argument ++ "))"

        If { test, then_, else_ } ->
            "((" ++ emitExpr test ++ ") ? (" ++ emitExpr then_ ++ ") : (" ++ emitExpr else_ ++ "))"

        Let { bindings, body } ->
            -- TODO emit in the right order? (dependencies, cycles...)
            let
                bindingsJS =
                    bindings
                        |> Dict.Any.values
                        |> List.map (\binding -> "const " ++ mangleVarName binding.name ++ " = " ++ emitExpr binding.body)
                        |> String.join ";"
            in
            "(() => {" ++ bindingsJS ++ "; return " ++ emitExpr body ++ ";})()"


mangleQualifiedVar : ModuleName -> VarName -> String
mangleQualifiedVar moduleName varName =
    mangleModuleName moduleName ++ "$" ++ mangleVarName varName


mangleModuleName : ModuleName -> String
mangleModuleName (ModuleName moduleName) =
    -- TODO what does the original Elm compiler do?
    String.replace "." "$" moduleName


mangleVarName : VarName -> String
mangleVarName (VarName varName) =
    -- TODO what does the original Elm compiler do?
    varName
