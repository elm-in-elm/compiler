module Stage.InferTypes.Boilerplate exposing
    ( inferModule
    , inferProject
    )

import Dict exposing (Dict)
import Dict.Extra
import Elm.AST.Canonical as Canonical
import Elm.AST.Typed as Typed
import Elm.Compiler.Error exposing (TypeError)
import Elm.Data.Declaration as Declaration exposing (Declaration, DeclarationBody(..))
import Elm.Data.Module exposing (Module)
import Elm.Data.ModuleName exposing (ModuleName)
import Elm.Data.Project exposing (Project)
import Elm.Data.Qualifiedness exposing (Qualified)
import Elm.Data.Type.Concrete exposing (ConcreteType)
import Elm.Data.VarName exposing (VarName)
import OurExtras.Dict as Dict
import OurExtras.List as List
import Stage.InferTypes.SubstitutionMap exposing (SubstitutionMap)


type alias InferExprFn =
    Dict ( ModuleName, VarName ) (ConcreteType Qualified)
    -> SubstitutionMap
    -> Canonical.LocatedExpr
    -> SubstResult Typed.LocatedExpr


type alias UnifyWithTypeAnnotationFn =
    Dict ( ModuleName, VarName ) (ConcreteType Qualified)
    -> SubstitutionMap
    -> Declaration Typed.LocatedExpr (ConcreteType Qualified) Qualified
    -> SubstResult (Declaration Typed.LocatedExpr Never Qualified)


type alias SubstResult a =
    Result ( TypeError, SubstitutionMap ) ( a, SubstitutionMap )


getAliases :
    Project Canonical.ProjectFields
    -> Dict ( ModuleName, VarName ) (ConcreteType Qualified)
getAliases project =
    project.modules
        |> Dict.toList
        |> List.fastConcatMap
            (\( moduleName, module_ ) ->
                module_.declarations
                    |> Dict.Extra.filterMap (always Declaration.getTypeAlias)
                    |> Dict.toList
                    |> List.map
                        (\( varName, alias_ ) ->
                            ( ( moduleName, varName ), alias_.definition )
                        )
            )
        |> Dict.fromList


inferProject :
    InferExprFn
    -> UnifyWithTypeAnnotationFn
    -> SubstitutionMap
    -> Project Canonical.ProjectFields
    -> Result TypeError (Project Typed.ProjectFields)
inferProject inferExpr unifyWithTypeAnnotation substitutionMap project =
    -- TODO would it be benefitial to return the SubstitutionMap in the Err case too?
    let
        aliases : Dict ( ModuleName, VarName ) (ConcreteType Qualified)
        aliases =
            getAliases project

        result : SubstResult (Dict ModuleName (Module Typed.LocatedExpr Never Qualified))
        result =
            project.modules
                |> Dict.foldl
                    (\moduleName module_ result_ ->
                        result_
                            |> Result.andThen
                                (\( newModules, substMap ) ->
                                    inferModule inferExpr unifyWithTypeAnnotation aliases substMap module_
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
    -> Dict ModuleName (Module Typed.LocatedExpr Never Qualified)
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
    -> Dict ( ModuleName, VarName ) (ConcreteType Qualified)
    -> SubstitutionMap
    -> Module Canonical.LocatedExpr (ConcreteType Qualified) Qualified
    -> SubstResult (Module Typed.LocatedExpr Never Qualified)
inferModule inferExpr unifyWithTypeAnnotation aliases substitutionMap module_ =
    module_.declarations
        |> Dict.foldl
            (\varName decl result_ ->
                result_
                    |> Result.andThen
                        (\( newDecls, substMap ) ->
                            inferDeclaration inferExpr aliases substMap decl
                                |> Result.andThen
                                    (\( newDecl, newSubstMap ) ->
                                        unifyWithTypeAnnotation
                                            aliases
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
    Module Canonical.LocatedExpr (ConcreteType Qualified) Qualified
    -> Dict VarName (Declaration Typed.LocatedExpr Never Qualified)
    -> Module Typed.LocatedExpr Never Qualified
moduleOfNewType old newDecls =
    { imports = old.imports
    , name = old.name
    , filePath = old.filePath
    , type_ = old.type_
    , exposing_ = old.exposing_
    , comments = old.comments

    -- all that code because of this:
    , declarations = newDecls
    }


inferDeclaration :
    InferExprFn
    -> Dict ( ModuleName, VarName ) (ConcreteType Qualified)
    -> SubstitutionMap
    -> Declaration Canonical.LocatedExpr (ConcreteType Qualified) Qualified
    -> SubstResult (Declaration Typed.LocatedExpr (ConcreteType Qualified) Qualified)
inferDeclaration inferExpr aliases substitutionMap decl =
    let
        result : SubstResult (DeclarationBody Typed.LocatedExpr (ConcreteType Qualified) Qualified)
        result =
            decl.body
                |> Declaration.mapBody
                    (inferExpr aliases substitutionMap)
                    identity
                    identity
                |> Declaration.combineValue
                |> Result.map Declaration.combineSubstitutionMap
    in
    result
        |> Result.map (Tuple.mapFirst (declarationOfNewType decl))


declarationOfNewType :
    Declaration Canonical.LocatedExpr annotation Qualified
    -> DeclarationBody Typed.LocatedExpr annotation Qualified
    -> Declaration Typed.LocatedExpr annotation Qualified
declarationOfNewType old newBody =
    { name = old.name
    , module_ = old.module_

    -- all that code because of this:
    , body = newBody
    }
