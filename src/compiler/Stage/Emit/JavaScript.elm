module Stage.Emit.JavaScript exposing (emitDeclaration, emitExpr)

-- TODO after we figure out module headers, compiling to one vs many files, revise the exposed values here

import AST.Backend as Backend
import AST.Common.Literal exposing (Literal(..))
import AST.Typed as Typed exposing (Expr_(..))
import AssocList as Dict
import Data.Declaration exposing (Declaration)
import Data.ModuleName as ModuleName exposing (ModuleName)
import Data.VarName as VarName exposing (VarName)


emitExpr : Backend.LocatedExpr -> String
emitExpr located =
    case Typed.getExpr located of
        Literal (Int int) ->
            String.fromInt int

        Literal (Float float) ->
            String.fromFloat float

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

        Cons e1 e2 ->
            "[" ++ emitExpr e1 ++ "].concat(" ++ emitExpr e2 ++ ")"

        Lambda { argument, body } ->
            -- TODO are these parentheses needed?
            "((" ++ mangleVarName argument ++ ") => " ++ emitExpr body ++ ")"

        Call { fn, argument } ->
            -- TODO are these parentheses needed?
            "(" ++ emitExpr fn ++ "(" ++ emitExpr argument ++ "))"

        If { test, then_, else_ } ->
            "(" ++ emitExpr test ++ " ? " ++ emitExpr then_ ++ " : " ++ emitExpr else_ ++ ")"

        Let { bindings, body } ->
            let
                bindingsJS =
                    bindings
                        |> Dict.values
                        |> {- AssocList reverses the list on creation:
                              https://package.elm-lang.org/packages/pzp1997/assoc-list/latest/AssocList#fromList
                           -}
                           List.reverse
                        |> List.map (\binding -> "const " ++ mangleVarName binding.name ++ " = " ++ emitExpr binding.body)
                        |> String.join "; "
            in
            "((() => {" ++ bindingsJS ++ "; return " ++ emitExpr body ++ ";})())"

        List items ->
            "["
                ++ (List.map emitExpr items |> String.join ", ")
                ++ "]"

        Unit ->
            """{type: "unit"}"""

        Tuple e1 e2 ->
            "[" ++ emitExpr e1 ++ "," ++ emitExpr e2 ++ "]"

        Tuple3 e1 e2 e3 ->
            "[" ++ emitExpr e1 ++ "," ++ emitExpr e2 ++ "," ++ emitExpr e3 ++ "]"


emitDeclaration : Declaration Backend.LocatedExpr -> String
emitDeclaration { module_, name, body } =
    "const "
        ++ mangleQualifiedVar module_ name
        ++ " = "
        ++ emitExpr body
        ++ ";"


mangleQualifiedVar : ModuleName -> VarName -> String
mangleQualifiedVar moduleName varName =
    mangleModuleName moduleName ++ "$" ++ mangleVarName varName


mangleModuleName : ModuleName -> String
mangleModuleName moduleName =
    String.replace "." "$" (ModuleName.toString moduleName)


mangleVarName : VarName -> String
mangleVarName varName =
    -- TODO this does nothing currently... what does the official Elm compiler do?
    VarName.toString varName
