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
import OurExtras.Tuple3 as Tuple3
import Stage.InferTypes.SubstitutionMap as SubstitutionMap exposing (SubstitutionMap)


type alias InferExprFn =
    Dict ( ModuleName, VarName ) (ConcreteType Qualified)
    -> Int
    -> SubstitutionMap
    -> Canonical.LocatedExpr
    -> SubstResult Typed.LocatedExpr


type alias UnifyWithTypeAnnotationFn =
    Dict ( ModuleName, VarName ) (ConcreteType Qualified)
    -> Int
    -> SubstitutionMap
    -> Declaration Typed.LocatedExpr (ConcreteType Qualified) Qualified
    -> SubstResult (Declaration Typed.LocatedExpr Never Qualified)


type alias SubstResult a =
    Result ( TypeError, SubstitutionMap ) ( a, SubstitutionMap, Int )


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
                                (\( newModules, substMap, unusedId ) ->
                                    inferModule
                                        inferExpr
                                        unifyWithTypeAnnotation
                                        aliases
                                        unusedId
                                        substMap
                                        module_
                                        |> Result.map
                                            (\( newModule, newSubstMap, newUnusedId ) ->
                                                ( Dict.insert moduleName newModule newModules
                                                , newSubstMap
                                                , newUnusedId
                                                )
                                            )
                                )
                    )
                    (Ok ( Dict.empty, substitutionMap, 0 ))
    in
    result
        |> Result.map (Tuple3.first >> projectOfNewType project)
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
    -> Int
    -> SubstitutionMap
    -> Module Canonical.LocatedExpr (ConcreteType Qualified) Qualified
    -> SubstResult (Module Typed.LocatedExpr Never Qualified)
inferModule inferExpr unifyWithTypeAnnotation aliases unusedId substitutionMap module_ =
    module_.declarations
        |> Dict.foldl
            (\varName decl result_ ->
                result_
                    |> Result.andThen
                        (\( newDecls, substitutionMap1, unusedId1 ) ->
                            inferDeclaration inferExpr aliases unusedId1 substitutionMap1 decl
                                |> Result.andThen
                                    (\( newDecl, substitutionMap2, unusedId2 ) ->
                                        unifyWithTypeAnnotation
                                            aliases
                                            unusedId2
                                            substitutionMap2
                                            newDecl
                                    )
                                |> Result.map (Tuple3.mapFirst (\newDecl -> Dict.insert varName newDecl newDecls))
                        )
            )
            (Ok ( Dict.empty, substitutionMap, unusedId ))
        |> Result.map (Tuple3.mapFirst (moduleOfNewType module_))


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

    -- all that code because of this:
    , declarations = newDecls
    }


inferDeclaration :
    InferExprFn
    -> Dict ( ModuleName, VarName ) (ConcreteType Qualified)
    -> Int
    -> SubstitutionMap
    -> Declaration Canonical.LocatedExpr (ConcreteType Qualified) Qualified
    -> SubstResult (Declaration Typed.LocatedExpr (ConcreteType Qualified) Qualified)
inferDeclaration inferExpr aliases unusedId substitutionMap decl =
    let
        result : SubstResult (DeclarationBody Typed.LocatedExpr (ConcreteType Qualified) Qualified)
        result =
            decl.body
                |> Declaration.mapBody
                    (inferExpr aliases unusedId substitutionMap)
                    identity
                    identity
                |> Declaration.combineValue
                {- TODO very unsure about this. Are we ever merging those empty
                   SubstitutionMaps with the non-empty ones?
                   Are the 0s meaningful?
                -}
                |> Result.map (Declaration.combineTuple3 ( SubstitutionMap.empty, 0 ))
    in
    result
        |> Result.map (Tuple3.mapFirst (declarationOfNewType decl))


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
