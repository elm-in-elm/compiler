module Stage.InferTypes.Boilerplate exposing
    ( inferModule
    , inferProject
    )

import Dict exposing (Dict)
import Dict.Extra
import Elm.AST.Canonical as Canonical
import Elm.AST.Typed as Typed
import Elm.Compiler.Error exposing (TypeError)
import Elm.Data.Declaration as Declaration exposing (Declaration, DeclarationBody)
import Elm.Data.Module exposing (Module)
import Elm.Data.ModuleName exposing (ModuleName)
import Elm.Data.Project exposing (Project)
import Elm.Data.Qualifiedness exposing (Qualified)
import Elm.Data.Type exposing (Id, TypeOrId)
import Elm.Data.Type.Concrete exposing (ConcreteType)
import Elm.Data.VarName exposing (VarName)
import OurExtras.List as List
import Stage.InferTypes.Environment as Env exposing (Environment)
import Stage.InferTypes.SubstitutionMap as SubstitutionMap exposing (SubstitutionMap)


type alias InferExprFn =
    Dict ( ModuleName, VarName ) (ConcreteType Qualified)
    -> Id
    -> Environment
    -> SubstitutionMap
    -> Canonical.LocatedExpr
    -> SubstResult Typed.LocatedExpr


type alias UnifyWithTypeAnnotationFn =
    Dict ( ModuleName, VarName ) (ConcreteType Qualified)
    -> Id
    -> Environment
    -> SubstitutionMap
    -> Declaration Typed.LocatedExpr (ConcreteType Qualified) Qualified
    -> SubstResult (Declaration Typed.LocatedExpr Never Qualified)


type alias SubstResult a =
    Result ( TypeError, SubstitutionMap ) ( a, ( SubstitutionMap, Id, Environment ) )


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
    -> Project Canonical.ProjectFields
    -> Result TypeError (Project Typed.ProjectFields)
inferProject inferExpr unifyWithTypeAnnotation project =
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
                                (\( newModules, ( substMap, unusedId, env ) ) ->
                                    inferModule
                                        inferExpr
                                        unifyWithTypeAnnotation
                                        aliases
                                        unusedId
                                        env
                                        substMap
                                        module_
                                        |> Result.map
                                            (\( newModule, ( newSubstMap, newUnusedId, newEnv ) ) ->
                                                ( Dict.insert moduleName newModule newModules
                                                , ( newSubstMap
                                                  , newUnusedId
                                                  , newEnv
                                                  )
                                                )
                                            )
                                )
                    )
                    (Ok ( Dict.empty, ( SubstitutionMap.empty, 0, Env.empty ) ))
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
    , sourceDirectories = old.sourceDirectories

    -- all that code because of this:
    , modules = modules
    }


inferModule :
    InferExprFn
    -> UnifyWithTypeAnnotationFn
    -> Dict ( ModuleName, VarName ) (ConcreteType Qualified)
    -> Id
    -> Environment
    -> SubstitutionMap
    -> Module Canonical.LocatedExpr (ConcreteType Qualified) Qualified
    -> SubstResult (Module Typed.LocatedExpr Never Qualified)
inferModule inferExpr unifyWithTypeAnnotation aliases unusedId env substitutionMap module_ =
    module_.declarations
        |> Dict.foldl
            (\varName decl result_ ->
                result_
                    |> Result.andThen
                        (\( newDecls, ( substitutionMap1, unusedId1, env1 ) ) ->
                            inferDeclaration inferExpr aliases unusedId1 env1 substitutionMap1 decl
                                |> Result.andThen
                                    (\( newDecl, ( substitutionMap2, unusedId2, env2 ) ) ->
                                        unifyWithTypeAnnotation
                                            aliases
                                            unusedId2
                                            env2
                                            substitutionMap2
                                            newDecl
                                    )
                                |> Result.map
                                    (Tuple.mapFirst
                                        (\newDecl -> Dict.insert varName newDecl newDecls)
                                    )
                        )
            )
            (Ok ( Dict.empty, ( substitutionMap, unusedId, env ) ))
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

    -- all that code because of this:
    , declarations = newDecls
    }


inferDeclaration :
    InferExprFn
    -> Dict ( ModuleName, VarName ) (ConcreteType Qualified)
    -> Id
    -> Environment
    -> SubstitutionMap
    -> Declaration Canonical.LocatedExpr (ConcreteType Qualified) Qualified
    -> SubstResult (Declaration Typed.LocatedExpr (ConcreteType Qualified) Qualified)
inferDeclaration inferExpr aliases unusedId env substitutionMap decl =
    let
        name : { module_ : ModuleName, name : VarName }
        name =
            { module_ = decl.module_
            , name = decl.name
            }

        result : SubstResult (DeclarationBody Typed.LocatedExpr (ConcreteType Qualified) Qualified)
        result =
            decl.body
                |> Declaration.mapBody
                    (inferExpr aliases unusedId env substitutionMap)
                    identity
                    identity
                |> Declaration.combineValue
                {- TODO very unsure about this. Are we ever merging those empty
                   SubstitutionMaps with the non-empty ones?
                   Are the 0s meaningful?
                -}
                |> Result.map
                    (\declBody ->
                        let
                            ( declBody_, ( finalSubstMap, finalCurrentId, finalEnv ) ) =
                                Declaration.combineTuple3
                                    {- These are just defaults, only used if
                                       DeclarationBody is not a Value. And thus
                                       harmless (there's nothing to infer for type or
                                       port declarations).
                                    -}
                                    ( SubstitutionMap.empty, 0, Env.empty )
                                    declBody

                            maybeType : Maybe (TypeOrId Qualified)
                            maybeType =
                                declBody_
                                    |> Declaration.getExpr
                                    |> Maybe.map Typed.getTypeOrId
                        in
                        ( declBody_
                        , ( finalSubstMap
                          , finalCurrentId
                          , case maybeType of
                                Nothing ->
                                    finalEnv

                                Just type_ ->
                                    finalEnv |> Env.add name type_
                          )
                        )
                    )
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
