module Stage.Emit.JsonAST exposing
    ( emitProject
    , emitDeclaration, emitExpr
    )

{-| The `emitProject` function is the main entrypoint in this module, ie. every
`Stage.Emit.<INSERT LANGUAGE HERE>` module has to expose this function to fit
well with the APIs of the other stages. See cli/Main.elm and its `compile`
function for example usage.

@docs emitProject

All the other exposed functions are (as of time of writing) exposed only for
testing purposes.

@docs emitDeclaration, emitExpr

-}

import Dict exposing (Dict)
import Elm.AST.Typed as Typed exposing (Expr_(..))
import Elm.Compiler.Error exposing (Error(..))
import Elm.Data.Declaration exposing (Declaration, DeclarationBody(..))
import Elm.Data.FileContents exposing (FileContents)
import Elm.Data.FilePath exposing (FilePath)
import Elm.Data.ModuleName exposing (ModuleName)
import Elm.Data.Project exposing (Project)
import Elm.Data.VarName exposing (VarName)
import Stage.Emit as Emit


type alias ProjectFields =
    { declarationList : List (Declaration Typed.LocatedExpr) }


emitProject : Project Typed.ProjectFields -> Result Error (Dict FilePath FileContents)
emitProject project =
    Ok project
        |> Result.andThen prepareProjectFields
        |> Result.map emitProject_


prepareProjectFields : Project Typed.ProjectFields -> Result Error (Project ProjectFields)
prepareProjectFields project =
    Emit.projectToDeclarationList project
        |> Result.mapError EmitError
        |> Result.map
            (\declarationList ->
                { mainFilePath = project.mainFilePath
                , mainModuleName = project.mainModuleName
                , elmJson = project.elmJson
                , sourceDirectory = project.sourceDirectory
                , declarationList = declarationList
                }
            )


emitProject_ : Project ProjectFields -> Dict FilePath FileContents
emitProject_ { declarationList } =
    let
        declarations =
            declarationList
                |> List.map emitDeclaration
    in
    Dict.singleton "out.json" ("[" ++ String.join "," declarations ++ "]")


toJson : String -> List ( String, String ) -> String
toJson astType fields =
    (quote "type" ++ ":" ++ quote astType)
        :: List.map (\( k, v ) -> quote k ++ ":" ++ v) fields
        |> brace


quote : String -> String
quote s =
    "\"" ++ s ++ "\""


brace : List String -> String
brace s =
    "{" ++ String.join "," s ++ "}"


fromFloat : Float -> String
fromFloat f =
    if isNaN f || isInfinite f then
        quote (String.fromFloat f)

    else
        String.fromFloat f


emitExpr : Typed.LocatedExpr -> String
emitExpr located =
    case Typed.getExpr located of
        Int int ->
            toJson "int"
                [ ( "value", String.fromInt int ) ]

        Float float ->
            toJson "float" [ ( "value", fromFloat float ) ]

        Char char ->
            toJson "char" [ ( "value", quote (String.fromChar char) ) ]

        String string ->
            toJson "string" [ ( "value", quote string ) ]

        Bool bool ->
            if bool then
                toJson "bool" [ ( "value", "true" ) ]

            else
                toJson "bool" [ ( "value", "false" ) ]

        Var var ->
            toJson "var" [ ( "name", quote (mangleQualifiedVar var) ) ]

        Argument argument ->
            toJson "arg" [ ( "name", quote argument ) ]

        Plus e1 e2 ->
            toJson "plus"
                [ ( "e1", emitExpr e1 )
                , ( "e2", emitExpr e2 )
                ]

        Cons e1 e2 ->
            toJson "cons"
                [ ( "e1", emitExpr e1 )
                , ( "e2", emitExpr e2 )
                ]

        Lambda { argument, body } ->
            toJson "lambda"
                [ ( "arg", quote argument )
                , ( "body", emitExpr body )
                ]

        Call { fn, argument } ->
            toJson "call"
                [ ( "fn", emitExpr fn )
                , ( "arg", emitExpr argument )
                ]

        If { test, then_, else_ } ->
            toJson "if"
                [ ( "test", emitExpr test )
                , ( "then", emitExpr then_ )
                , ( "else", emitExpr else_ )
                ]

        Let { bindings, body } ->
            let
                {- TODO this doesn't take inter-let dependencies into account, also Dict.values just returns stuff "randomly" -}
                assignments =
                    bindings
                        |> Dict.values
                        |> List.map (\binding -> quote binding.name ++ ":" ++ emitExpr binding.body)
            in
            toJson "let"
                [ ( "bind", brace assignments )
                , ( "body", emitExpr body )
                ]

        List items ->
            toJson "list"
                [ ( "items", "[" ++ (List.map emitExpr items |> String.join ",") ++ "]" )
                ]

        Unit ->
            toJson "unit" []

        Tuple e1 e2 ->
            toJson "tuple"
                [ ( "e1", emitExpr e1 )
                , ( "e2", emitExpr e2 )
                ]

        Tuple3 e1 e2 e3 ->
            toJson "tuple"
                [ ( "e1", emitExpr e1 )
                , ( "e2", emitExpr e2 )
                , ( "e3", emitExpr e3 )
                ]

        Record bindings ->
            let
                entries =
                    bindings
                        |> Dict.values
                        |> List.map (\binding -> quote binding.name ++ ":" ++ emitExpr binding.body)
            in
            toJson "record" [ ( "bind", brace entries ) ]


emitDeclaration : Declaration Typed.LocatedExpr -> String
emitDeclaration { module_, name, body } =
    case body of
        Value expr ->
            toJson "decl"
                [ ( "name", quote (module_ ++ "$" ++ name) )
                , ( "expr", emitExpr expr )
                ]

        TypeAlias _ ->
            ""

        CustomType _ ->
            ""


mangleQualifiedVar : { module_ : ModuleName, name : VarName } -> String
mangleQualifiedVar { module_, name } =
    mangleModuleName module_ ++ "$" ++ name


mangleModuleName : ModuleName -> String
mangleModuleName moduleName =
    String.replace "." "$" moduleName
