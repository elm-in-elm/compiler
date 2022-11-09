module Stage.Emit.HVM exposing
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
import Elm.Data.Type as Type
import Elm.Data.Type.ToString as TypeToString
import FNV1a
import Hex
import Maybe.Extra as Maybe
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
        |> Dict.singleton "out.hvm"


preamble : String
preamble =
    """
(Bool.if (Bool.true)  t _) = t
(Bool.if (Bool.false) _ f) = f
"""


emitExpr : Typed.LocatedExpr -> String
emitExpr located =
    case Typed.getExpr located of
        Int int ->
            String.fromInt int

        Float float ->
            -- TODO update when HVM supports floats
            "(Float.fromString " ++ String.fromFloat float ++ ")"

        Char char ->
            "\"" ++ String.fromChar char ++ "\""

        String string ->
            -- TODO do we need to escape somehow?
            "\"" ++ string ++ "\""

        Bool bool ->
            if bool then
                "(Bool.true)"

            else
                "(Bool.false)"

        Var var ->
            mangleQualifiedVar var

        ConstructorValue var ->
            mangleQualifiedVar var

        Argument argument ->
            mangleVarName argument

        BinOp op e1 e2 ->
            "("
                ++ op
                ++ " "
                ++ emitExpr e1
                ++ " "
                ++ emitExpr e2
                ++ ")"

        Lambda { argument, body } ->
            -- we could have also used λ instead of @, but @ is more easily writable
            "@" ++ mangleVarName argument ++ " " ++ emitExpr body

        Call { fn, argument } ->
            -- TODO can we call an arbitrary expression? Or do these have to be rule names in the end?
            "("
                ++ emitExpr fn
                ++ " "
                ++ emitExpr argument
                ++ ")"

        If { test, then_, else_ } ->
            "(Bool.if "
                ++ emitExpr test
                ++ " "
                ++ emitExpr then_
                ++ " "
                ++ emitExpr else_
                ++ ")"

        Let { bindings, body } ->
            let
                letLines =
                    bindings
                        |> Dict.values
                        {- TODO this doesn't take inter-let dependencies into
                           account, also Dict.values just returns stuff "randomly"
                        -}
                        |> List.map (\binding -> "let " ++ mangleVarName binding.name ++ " = " ++ emitExpr binding.body)
                        |> String.join "\n"
            in
            letLines ++ "\n" ++ emitExpr body

        List items ->
            "["
                ++ (List.map emitExpr items |> String.join ", ")
                ++ "]"

        Unit ->
            "(Unit)"

        Tuple e1 e2 ->
            "(Tuple2 "
                ++ emitExpr e1
                ++ " "
                ++ emitExpr e2
                ++ ")"

        Tuple3 e1 e2 e3 ->
            "(Tuple3 "
                ++ emitExpr e1
                ++ " "
                ++ emitExpr e2
                ++ " "
                ++ emitExpr e3
                ++ ")"

        Record bindings ->
            -- TODO continue here
            {- We'll hash the record field names and types to generate its name.

               We need a predictable ordering of the fields.
               Dict.toList, Dict.values and Dict.keys all sort by key, ASC.
            -}
            let
                fieldNames : List String
                fieldNames =
                    Dict.keys bindings

                fieldTypes : List String
                fieldTypes =
                    -- TODO what if we can't find the record type and get the fields out?
                    let
                        types =
                            Typed.getType located
                                |> Maybe.andThen
                                    (\t ->
                                        case t of
                                            Type.Record typeBindings ->
                                                Dict.values typeBindings
                                                    |> Maybe.traverse Type.getType

                                            _ ->
                                                let
                                                    _ =
                                                        Debug.log "uh oh..." t
                                                in
                                                Nothing
                                    )
                                |> Maybe.withDefault []

                        initState =
                            TypeToString.fromTypes types
                    in
                    List.foldl
                        (\type_ ( accState, accList ) ->
                            let
                                ( string, newState ) =
                                    TypeToString.toStringType accState type_
                            in
                            ( newState, string :: accList )
                        )
                        ( initState, [] )
                        types
                        |> Tuple.second
                        |> List.reverse

                recordHashInt : Int
                recordHashInt =
                    List.foldl
                        FNV1a.hashWithSeed
                        FNV1a.initialSeed
                        (fieldNames ++ fieldTypes)

                recordHash : String
                recordHash =
                    Hex.toString recordHashInt

                recordName : String
                recordName =
                    "Record_" ++ recordHash

                fieldValues : String
                fieldValues =
                    bindings
                        |> Dict.values
                        |> List.map (.body >> emitExpr)
                        |> String.join " "
            in
            "("
                ++ recordName
                ++ " "
                ++ fieldValues
                ++ ")"

        RecordAccess e field ->
            "(Get_" ++ field ++ " " ++ emitExpr e ++ ")"

        Case _ _ ->
            "TODO"


emitDeclaration : Declaration Typed.LocatedExpr Never Qualified -> String
emitDeclaration { module_, name, body } =
    case body of
        Value { expression } ->
            "("
                ++ mangleQualifiedVar { module_ = module_, name = name }
                ++ ") = "
                ++ emitExpr expression

        TypeAlias _ ->
            ""

        CustomType _ ->
            ""

        Port _ ->
            -- TODO somehow emit ports!
            ""
