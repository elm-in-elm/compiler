module Stage.Emit.JavaScript exposing (emitExpr, emitTopLevelDeclaration, mangleQualifiedVar)

-- TODO after we figure out module headers, compiling to one vs many files, revise the exposed values here

import AST.Backend as Backend
import AST.Common.Literal exposing (Literal(..))
import AST.Typed exposing (Expr_(..))
import Common.Types
    exposing
        ( ModuleName(..)
        , TopLevelDeclaration
        , VarName(..)
        )
import Dict.Any


{-| TODO
-}
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
            let
                bindingsJS =
                    bindings
                        |> Dict.Any.values
                        |> List.map (\binding -> "const " ++ mangleVarName binding.name ++ " = " ++ emitExpr binding.body)
                        |> String.join ";"
            in
            "(() => {" ++ bindingsJS ++ "; return " ++ emitExpr body ++ ";})()"


{-| TODO test
-}
emitTopLevelDeclaration : TopLevelDeclaration Backend.Expr -> String
emitTopLevelDeclaration { module_, name, body } =
    "const "
        ++ mangleQualifiedVar module_ name
        ++ " = "
        ++ emitExpr body
        ++ ";"


{-| TODO test
-}
mangleQualifiedVar : ModuleName -> VarName -> String
mangleQualifiedVar moduleName varName =
    mangleModuleName moduleName ++ "$" ++ mangleVarName varName


mangleModuleName : ModuleName -> String
mangleModuleName (ModuleName moduleName) =
    String.replace "." "$" moduleName


mangleVarName : VarName -> String
mangleVarName (VarName varName) =
    varName
