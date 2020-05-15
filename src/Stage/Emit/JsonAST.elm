module Stage.Emit.JsonAST exposing
    ( emitProject
    , emitDeclaration, emitExpr
    )

{-| This module encodes the AST representation into JSON. It is intended
that the JSON can then be processed by another tool.
See <https://github.com/sgdan/truffle-elm> for an example implementation that
uses the Truffle API and GraalVM to either interpret the code on the JVM,
or compile to a native linux executable.

`emitProject` is the main entrypoint for this module.

@docs emitProject

Other exposed functions are only for testing purposes.

@docs emitDeclaration, emitExpr

-}

import Dict exposing (Dict)
import Elm.AST.Typed as Typed exposing (Expr_(..))
import Elm.Compiler.Error exposing (Error(..))
import Elm.Data.Declaration exposing (Declaration, DeclarationBody(..))
import Elm.Data.FileContents exposing (FileContents)
import Elm.Data.FilePath exposing (FilePath)
import Elm.Data.Project exposing (Project)
import Json.Encode as E
import Stage.Emit.Common exposing (mangleQualifiedVar, prepareProjectFields)


type alias ProjectFields =
    { declarationList : List (Declaration Typed.LocatedExpr) }


emitProject : Project Typed.ProjectFields -> Result Error (Dict FilePath FileContents)
emitProject project =
    Ok project
        |> Result.andThen prepareProjectFields
        |> Result.map emitProject_


emitProject_ : Project ProjectFields -> Dict FilePath FileContents
emitProject_ { declarationList } =
    let
        emit =
            \d -> emitDeclaration d
    in
    Dict.singleton "out.json" (E.list emit declarationList |> E.encode 0)


encode : String -> List ( String, E.Value ) -> E.Value
encode tipe values =
    E.object (( "type", E.string tipe ) :: values)


emitExpr : Typed.LocatedExpr -> E.Value
emitExpr located =
    case Typed.getExpr located of
        Int int ->
            encode "int" [ ( "value", E.int int ) ]

        Float float ->
            encode "float" [ ( "value", E.float float ) ]

        Char char ->
            encode "char" [ ( "value", E.string (String.fromChar char) ) ]

        String string ->
            encode "string" [ ( "value", E.string string ) ]

        Bool bool ->
            encode "bool" [ ( "value", E.bool bool ) ]

        Var var ->
            encode "var" [ ( "name", E.string (mangleQualifiedVar var) ) ]

        Argument argument ->
            encode "arg" [ ( "name", E.string argument ) ]

        Plus e1 e2 ->
            encode "plus"
                [ ( "e1", emitExpr e1 )
                , ( "e2", emitExpr e2 )
                ]

        Cons e1 e2 ->
            encode "cons"
                [ ( "e1", emitExpr e1 )
                , ( "e2", emitExpr e2 )
                ]

        Lambda { argument, body } ->
            encode "lambda"
                [ ( "arg", E.string argument )
                , ( "body", emitExpr body )
                ]

        Call { fn, argument } ->
            encode "call"
                [ ( "fn", emitExpr fn )
                , ( "arg", emitExpr argument )
                ]

        If { test, then_, else_ } ->
            encode "if"
                [ ( "test", emitExpr test )
                , ( "then", emitExpr then_ )
                , ( "else", emitExpr else_ )
                ]

        Let { bindings, body } ->
            encode "let"
                [ ( "bind", E.dict (\k -> k) (\v -> emitExpr v.body) bindings )
                , ( "body", emitExpr body )
                ]

        List items ->
            encode "list"
                [ ( "items", E.list emitExpr items ) ]

        Unit ->
            encode "unit" []

        Tuple e1 e2 ->
            encode "tuple"
                [ ( "e1", emitExpr e1 )
                , ( "e2", emitExpr e2 )
                ]

        Tuple3 e1 e2 e3 ->
            encode "tuple"
                [ ( "e1", emitExpr e1 )
                , ( "e2", emitExpr e2 )
                , ( "e3", emitExpr e3 )
                ]

        Record bindings ->
            encode "record" [ ( "bind", E.dict (\k -> k) (\v -> emitExpr v.body) bindings ) ]

        Case test branches ->
            encode "case"
                [ ( "test", emitExpr test )
                , ( "branches"
                  , E.list
                        (\b ->
                            E.object
                                [ ( "pattern", E.string "TODO" )
                                , ( "body", emitExpr b.body )
                                ]
                        )
                        branches
                  )
                ]


emitDeclaration : Declaration Typed.LocatedExpr -> E.Value
emitDeclaration { module_, name, body } =
    case body of
        Value expr ->
            encode "decl"
                [ ( "name", E.string (module_ ++ "$" ++ name) )
                , ( "expr", emitExpr expr )
                ]

        TypeAlias _ ->
            E.string ""

        CustomType _ ->
            E.string ""
