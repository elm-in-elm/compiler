module Stage.InferTypes.Boilerplate exposing
    ( inferModule
    , inferProject
    )

import Dict exposing (Dict)
import Elm.AST.Canonical as Canonical
import Elm.AST.Typed as Typed
import Elm.Compiler.Error exposing (TypeError)
import Elm.Data.Declaration as Declaration exposing (Declaration, DeclarationBody(..))
import Elm.Data.Module exposing (Module)
import Elm.Data.ModuleName exposing (ModuleName)
import Elm.Data.Project exposing (Project)
import Elm.Data.Type exposing (Type, TypeQ)
import Elm.Data.VarName exposing (VarName)
import OurExtras.Dict as Dict
import Stage.InferTypes.SubstitutionMap exposing (SubstitutionMap)


type alias InferExprFn =
    SubstitutionMap
    -> Canonical.LocatedExpr
    -> Result ( TypeError, SubstitutionMap ) ( Typed.LocatedExpr, SubstitutionMap )


type alias UnifyWithTypeAnnotationFn =
    SubstitutionMap
    -> Declaration Typed.LocatedExpr TypeQ ModuleName
    -> Result ( TypeError, SubstitutionMap ) ( Declaration Typed.LocatedExpr Never ModuleName, SubstitutionMap )


type alias SubstResult a =
    Result ( TypeError, SubstitutionMap ) ( a, SubstitutionMap )


inferProject :
    InferExprFn
    -> UnifyWithTypeAnnotationFn
    -> SubstitutionMap
    -> Project Canonical.ProjectFields
    -> Result TypeError (Project Typed.ProjectFields)
inferProject inferExpr unifyWithTypeAnnotation substitutionMap project =
    -- TODO would it be benefitial to return the SubstitutionMap in the Err case too?
    let
        result : SubstResult (Dict ModuleName (Module Typed.LocatedExpr Never ModuleName))
        result =
            project.modules
                |> Dict.foldl
                    (\moduleName module_ result_ ->
                        result_
                            |> Result.andThen
                                (\( newModules, substMap ) ->
                                    inferModule inferExpr unifyWithTypeAnnotation substMap module_
                                        |> Result.map
                                            (\( newModule, newSubstMap ) ->
                                                ( Dict.insert moduleName newModule newModules
                                                , newSubstMap
                                                )
                                            )
                                )
                    )
                    (Ok ( Dict.empty, substitutionMap ))
    in
    result
        |> Result.map (Tuple.first >> projectOfNewType project)
        |> Result.mapError Tuple.first


projectOfNewType :
    Project Canonical.ProjectFields
    -> Dict ModuleName (Module Typed.LocatedExpr Never ModuleName)
    -> Project Typed.ProjectFields
projectOfNewType old modules =
    { elmJson = old.elmJson
    , mainFilePath = old.mainFilePath
    , mainModuleName = old.mainModuleName
    , sourceDirectory = old.sourceDirectory

    -- all that code because of this:
    , modules = modules
    }


inferModule :
    InferExprFn
    -> UnifyWithTypeAnnotationFn
    -> SubstitutionMap
    -> Module Canonical.LocatedExpr TypeQ ModuleName
    -> SubstResult (Module Typed.LocatedExpr Never ModuleName)
inferModule inferExpr unifyWithTypeAnnotation substitutionMap module_ =
    module_.declarations
        |> Dict.foldl
            (\varName decl result_ ->
                result_
                    |> Result.andThen
                        (\( newDecls, substMap ) ->
                            inferDeclaration inferExpr substMap decl
                                |> Result.andThen
                                    (\( newDecl, newSubstMap ) ->
                                        unifyWithTypeAnnotation
                                            newSubstMap
                                            newDecl
                                    )
                                |> Result.map
                                    (\( newDecl, newSubstMap ) ->
                                        ( Dict.insert varName newDecl newDecls
                                        , newSubstMap
                                        )
                                    )
                        )
            )
            (Ok ( Dict.empty, substitutionMap ))
        |> Result.map (Tuple.mapFirst (moduleOfNewType module_))


moduleOfNewType :
    Module Canonical.LocatedExpr TypeQ ModuleName
    -> Dict VarName (Declaration Typed.LocatedExpr Never ModuleName)
    -> Module Typed.LocatedExpr Never ModuleName
moduleOfNewType old newDecls =
    { imports = old.imports
    , name = old.name
    , filePath = old.filePath
    , type_ = old.type_
    , exposing_ = old.exposing_

    -- all that code because of this:
    , declarations = newDecls
    }


inferDeclaration :
    InferExprFn
    -> SubstitutionMap
    -> Declaration Canonical.LocatedExpr TypeQ ModuleName
    -> SubstResult (Declaration Typed.LocatedExpr TypeQ ModuleName)
inferDeclaration inferExpr substitutionMap decl =
    let
        result : SubstResult (DeclarationBody Typed.LocatedExpr ModuleName)
        result =
            decl.body
                |> Declaration.mapBody (inferExpr substitutionMap)
                |> Declaration.combine
                |> Result.map Declaration.combineSubstitutionMap
    in
    result
        |> Result.map (Tuple.mapFirst (declarationOfNewType decl))


declarationOfNewType :
    Declaration Canonical.LocatedExpr annotation ModuleName
    -> DeclarationBody Typed.LocatedExpr ModuleName
    -> Declaration Typed.LocatedExpr annotation ModuleName
declarationOfNewType old newBody =
    { name = old.name
    , module_ = old.module_
    , typeAnnotation = old.typeAnnotation

    -- all that code because of this:
    , body = newBody
    }
