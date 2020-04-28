module Stage.Desugar.Boilerplate exposing
    ( desugarModule
    , desugarProject
    )

import Dict exposing (Dict)
import Elm.AST.Canonical as Canonical
import Elm.AST.Frontend as Frontend
import Elm.Compiler.Error exposing (DesugarError)
import Elm.Data.Declaration as Declaration exposing (Declaration, DeclarationBody)
import Elm.Data.Module exposing (Module)
import Elm.Data.ModuleName exposing (ModuleName)
import Elm.Data.Project exposing (Project)
import Elm.Data.Type exposing (Type, TypeOrIdQ, TypeOrIdUnq, TypeQ, TypeUnq)
import Elm.Data.TypeAnnotation exposing (TypeAnnotation)
import Elm.Data.VarName exposing (VarName)
import OurExtras.Dict as Dict


type alias DesugarExprFn =
    Module Frontend.LocatedExpr TypeAnnotation (Maybe String)
    -> Frontend.LocatedExpr
    -> Result DesugarError Canonical.LocatedExpr


type alias DesugarTypeFn =
    Module Frontend.LocatedExpr TypeAnnotation (Maybe String)
    -> TypeOrIdUnq
    -> Result DesugarError TypeOrIdQ


type alias DesugarAnnotationFn =
    Declaration Canonical.LocatedExpr TypeAnnotation (Maybe String)
    -> Result DesugarError (Declaration Canonical.LocatedExpr TypeUnq (Maybe String))


desugarProject :
    DesugarExprFn
    -> DesugarTypeFn
    -> DesugarAnnotationFn
    -> Project Frontend.ProjectFields
    -> Result DesugarError (Project Canonical.ProjectFields)
desugarProject desugarExpr desugarType desugarAnnotation project =
    project.modules
        |> Dict.map
            (always
                (desugarModule
                    desugarExpr
                    desugarType
                    desugarAnnotation
                )
            )
        |> Dict.combine
        |> Result.map (projectOfNewType project)


projectOfNewType :
    Project Frontend.ProjectFields
    -> Dict ModuleName (Module Canonical.LocatedExpr TypeQ String)
    -> Project Canonical.ProjectFields
projectOfNewType old modules =
    { elmJson = old.elmJson
    , mainFilePath = old.mainFilePath
    , mainModuleName = old.mainModuleName
    , sourceDirectory = old.sourceDirectory

    -- all that code because of this:
    , modules = modules
    }


desugarModule :
    DesugarExprFn
    -> DesugarTypeFn
    -> DesugarAnnotationFn
    -> Module Frontend.LocatedExpr TypeAnnotation (Maybe String)
    -> Result DesugarError (Module Canonical.LocatedExpr TypeQ String)
desugarModule desugarExpr desugarType desugarAnnotation module_ =
    module_.declarations
        |> Dict.map
            (always
                (desugarDeclaration
                    (desugarExpr module_)
                    (desugarType module_)
                    desugarAnnotation
                )
            )
        |> Dict.combine
        |> Result.map (moduleOfNewType module_)


moduleOfNewType :
    Module Frontend.LocatedExpr TypeAnnotation (Maybe String)
    -> Dict VarName (Declaration Canonical.LocatedExpr TypeQ String)
    -> Module Canonical.LocatedExpr TypeQ String
moduleOfNewType old newDecls =
    { imports = old.imports
    , name = old.name
    , filePath = old.filePath
    , type_ = old.type_
    , exposing_ = old.exposing_

    -- all that code because of this:
    , declarations = newDecls
    }


desugarDeclaration :
    (Frontend.LocatedExpr -> Result DesugarError Canonical.LocatedExpr)
    -> (TypeOrIdUnq -> Result DesugarError TypeOrIdQ)
    -> DesugarAnnotationFn
    -> Declaration Frontend.LocatedExpr TypeAnnotation (Maybe String)
    -> Result DesugarError (Declaration Canonical.LocatedExpr TypeQ String)
desugarDeclaration desugarExpr desugarType desugarAnnotation decl =
    {-
       decl.body
           |> Declaration.mapBody desugarExpr
           |> Declaration.combine
           |> Result.map (declarationOfNewType decl)
           |> Result.andThen desugarAnnotation
    -}
    Debug.todo "TODO use desugarType here somewhere"


declarationOfNewType :
    Declaration exprA ann utm1
    -> DeclarationBody exprB utm2
    -> Declaration exprB ann utm2
declarationOfNewType old newBody =
    { name = old.name
    , module_ = old.module_
    , typeAnnotation = old.typeAnnotation

    -- all that code because of this:
    , body = newBody
    }
