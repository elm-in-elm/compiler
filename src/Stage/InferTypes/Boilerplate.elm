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
import Elm.Data.Type exposing (Type)
import Elm.Data.VarName exposing (VarName)
import OurExtras.Dict as Dict
import Stage.InferTypes.SubstitutionMap exposing (SubstitutionMap)


type alias InferExprFn =
    SubstitutionMap
    -> Canonical.LocatedExpr
    -> Result ( TypeError, SubstitutionMap ) ( Typed.LocatedExpr, SubstitutionMap )


type alias UnifyWithTypeAnnotationFn =
    SubstitutionMap
    -> Declaration Typed.LocatedExpr Type
    -> Result ( TypeError, SubstitutionMap ) ( Declaration Typed.LocatedExpr Never, SubstitutionMap )


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
        result : SubstResult (Dict ModuleName (Module Typed.LocatedExpr Never))
        result =
            project.modules
                |> Dict.foldl
                    (\moduleName module_ result_ ->
                        result_
                            |> Result.andThen
                                (\( newModules, substMap ) ->
                                    inferModule inferExpr substMap module_
                                        -- TODO unifyWithTypeAnnotation substitutionMap stuff
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
    -> Dict ModuleName (Module Typed.LocatedExpr Never)
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
    -> SubstitutionMap
    -> Module Canonical.LocatedExpr Type
    -> SubstResult (Module Typed.LocatedExpr Never)
inferModule inferExpr substitutionMap module_ =
    let
        result : SubstResult (Dict VarName (Declaration Typed.LocatedExpr Never))
        result =
            {- We'd like to Dict.map here, but we need to thread
               the SubstitutionMap through all the calls...
            -}
            module_.declarations
                |> Dict.foldl
                    (\varName decl result_ ->
                        result_
                            |> Result.andThen
                                (\( newDecls, substMap ) ->
                                    inferDeclaration inferExpr substMap decl
                                        |> Result.map
                                            (\( newDecl, newSubstMap ) ->
                                                ( Dict.insert varName newDecl newDecls
                                                , newSubstMap
                                                )
                                            )
                                )
                    )
                    (Ok ( Dict.empty, substitutionMap ))
    in
    result
        |> Result.map (Tuple.mapFirst (moduleOfNewType module_))


moduleOfNewType :
    Module Canonical.LocatedExpr Type
    -> Dict VarName (Declaration Typed.LocatedExpr Never)
    -> Module Typed.LocatedExpr Never
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
    -> Declaration Canonical.LocatedExpr Type
    -> SubstResult (Declaration Typed.LocatedExpr Never)
inferDeclaration inferExpr substitutionMap decl =
    let
        result : SubstResult (DeclarationBody Typed.LocatedExpr)
        result =
            decl.body
                |> Declaration.mapBody (inferExpr substitutionMap)
                |> Declaration.combine
                |> Result.map Declaration.combineSubstitutionMap
    in
    result
        |> Result.map (Tuple.mapFirst (declarationOfNewType decl))


declarationOfNewType :
    Declaration Canonical.LocatedExpr Type
    -> DeclarationBody Typed.LocatedExpr
    -> Declaration Typed.LocatedExpr Never
declarationOfNewType old newBody =
    { name = old.name
    , module_ = old.module_
    , typeAnnotation = Nothing

    -- all that code because of this:
    , body = newBody
    }
