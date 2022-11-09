module Stage.Emit.JavaScript exposing
    ( emitProject
    , emitDeclaration, emitExpr
    )

{-| The `emitProject` function is the main entrypoint in this module, ie. every
`Stage.Emit.<INSERT LANGUAGE HERE>` module has to expose this function to fit
well with the APIs of the other stages. See src/cli/Main.elm and its `compile`
function for example usage.

@docs emitProject

All the other exposed functions are (as of time of writing) exposed only for
testing purposes.

@docs emitDeclaration, emitExpr

-}

import Dict exposing (Dict)
import Elm.AST.Typed as Typed exposing (Expr_(..))
import Elm.Compiler.Error exposing (Error)
import Elm.Data.Declaration exposing (Declaration, DeclarationBody(..))
import Elm.Data.FileContents exposing (FileContents)
import Elm.Data.FilePath exposing (FilePath)
import Elm.Data.Project exposing (Project)
import Elm.Data.Qualifiedness exposing (Qualified)
import Stage.Emit.Common exposing (mangleQualifiedVar, mangleVarName, prepareProjectFields)


type alias ProjectFields =
    { declarationList : List (Declaration Typed.LocatedExpr Never Qualified) }


emitProject : Project Typed.ProjectFields -> Result Error (Dict FilePath FileContents)
emitProject project =
    Ok project
        |> Result.andThen prepareProjectFields
        |> Result.map emitProject_


emitProject_ : Project ProjectFields -> Dict FilePath FileContents
emitProject_ { declarationList } =
    declarationList
        |> List.map emitDeclaration
        |> String.join "\n"
        |> Dict.singleton "out.js"


emitExpr : Typed.LocatedExpr -> String
emitExpr located =
    case Typed.getExpr located of
        Int int ->
            String.fromInt int

        Float float ->
            String.fromFloat float

        Char char ->
            "\"" ++ String.fromChar char ++ "\""

        String string ->
            "\"" ++ string ++ "\""

        Bool bool ->
            if bool then
                "true"

            else
                "false"

        Var var ->
            mangleQualifiedVar var

        Argument argument ->
            mangleVarName argument

        BinOp op e1 e2 ->
            "(" ++ emitExpr e1 ++ " " ++ op ++ " " ++ emitExpr e2 ++ ")"

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
                        {- TODO this doesn't take inter-let dependencies into
                           account, also Dict.values just returns stuff "randomly"
                        -}
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

        Record bindings ->
            let
                bindingsJS =
                    bindings
                        |> Dict.values
                        |> List.map (\binding -> mangleVarName binding.name ++ ": " ++ emitExpr binding.body)
                        |> String.join ", "
            in
            "{" ++ bindingsJS ++ "}"

        RecordAccess e field ->
            emitExpr e ++ "." ++ field

        Case _ _ ->
            "TODO"

        ConstructorValue _ ->
            "TODO"


emitDeclaration : Declaration Typed.LocatedExpr Never Qualified -> String
emitDeclaration { module_, name, body } =
    case body of
        Value { expression } ->
            "const "
                ++ mangleQualifiedVar { module_ = module_, name = name }
                ++ " = "
                ++ emitExpr expression
                ++ ";"

        TypeAlias _ ->
            ""

        CustomType _ ->
            ""

        Port _ ->
            -- TODO somehow emit ports!
            ""
