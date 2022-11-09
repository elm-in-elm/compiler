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
import Elm.Compiler.Error exposing (Error)
import Elm.Data.Declaration exposing (Declaration, DeclarationBody(..))
import Elm.Data.FileContents exposing (FileContents)
import Elm.Data.FilePath exposing (FilePath)
import Elm.Data.Project exposing (Project)
import Elm.Data.Qualifiedness exposing (Qualified)
import Json.Encode as Encode exposing (Value)
import List.NonEmpty
import Stage.Emit.Common exposing (mangleQualifiedVar, prepareProjectFields)


type alias ProjectFields =
    { declarationList : List (Declaration Typed.LocatedExpr Never Qualified) }


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
    Dict.singleton "out.json" (Encode.list emit declarationList |> Encode.encode 0)


encode : String -> List ( String, Value ) -> Value
encode tipe values =
    Encode.object (( "type", Encode.string tipe ) :: values)


emitExpr : Typed.LocatedExpr -> Value
emitExpr located =
    case Typed.getExpr located of
        Int int ->
            encode "int" [ ( "value", Encode.int int ) ]

        Float float ->
            encode "float" [ ( "value", Encode.float float ) ]

        Char char ->
            encode "char" [ ( "value", Encode.string (String.fromChar char) ) ]

        String string ->
            encode "string" [ ( "value", Encode.string string ) ]

        Bool bool ->
            encode "bool" [ ( "value", Encode.bool bool ) ]

        Var var ->
            encode "var" [ ( "name", Encode.string (mangleQualifiedVar var) ) ]

        Argument argument ->
            encode "arg" [ ( "name", Encode.string argument ) ]

        BinOp op e1 e2 ->
            encode "binop"
                [ ( "op", Encode.string op )
                , ( "e1", emitExpr e1 )
                , ( "e2", emitExpr e2 )
                ]

        Lambda { argument, body } ->
            encode "lambda"
                [ ( "arg", Encode.string argument )
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
                [ ( "bind", Encode.dict identity (\v -> emitExpr v.body) bindings )
                , ( "body", emitExpr body )
                ]

        List items ->
            encode "list"
                [ ( "items", Encode.list emitExpr items ) ]

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
            encode "record" [ ( "bind", Encode.dict identity (\v -> emitExpr v.body) bindings ) ]

        RecordAccess e field ->
            encode "access"
                [ ( "expr", emitExpr e )
                , ( "field", Encode.string field )
                ]

        Case test branches ->
            encode "case"
                [ ( "test", emitExpr test )
                , ( "branches"
                  , Encode.list
                        (\b ->
                            Encode.object
                                [ ( "pattern", Encode.string "TODO" )
                                , ( "body", emitExpr b.body )
                                ]
                        )
                        (List.NonEmpty.toList branches)
                  )
                ]

        ConstructorValue rec ->
            encode "constructor" [ ( "name", Encode.string (mangleQualifiedVar rec) ) ]


emitDeclaration : Declaration Typed.LocatedExpr Never Qualified -> Value
emitDeclaration { module_, name, body } =
    case body of
        Value { expression } ->
            encode "decl"
                [ ( "name", Encode.string (module_ ++ "$" ++ name) )
                , ( "expr", emitExpr expression )
                ]

        TypeAlias _ ->
            -- We don't emit these structures. Typechecking has already been done!
            Encode.string ""

        CustomType _ ->
            -- We don't emit these structures. Typechecking has already been done!
            Encode.string ""

        Port _ ->
            -- TODO maybe there's a reasonable way to use ports with the Truffle?
            Encode.string ""
