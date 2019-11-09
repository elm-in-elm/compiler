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
import Elm.Data.VarName exposing (VarName)
import OurExtras.Dict as Dict


desugarProject : (Module Frontend.LocatedExpr -> Frontend.LocatedExpr -> Result DesugarError Canonical.LocatedExpr) -> Project Frontend.ProjectFields -> Result DesugarError (Project Canonical.ProjectFields)
desugarProject desugarExpr project =
    project.modules
        |> Dict.map (always (desugarModule desugarExpr))
        |> Dict.combine
        |> Result.map (projectOfNewType project)


projectOfNewType : Project Frontend.ProjectFields -> Dict ModuleName (Module Canonical.LocatedExpr) -> Project Canonical.ProjectFields
projectOfNewType old modules =
    { elmJson = old.elmJson
    , mainFilePath = old.mainFilePath
    , mainModuleName = old.mainModuleName
    , sourceDirectory = old.sourceDirectory

    -- all that code because of this:
    , modules = modules
    }


desugarModule : (Module Frontend.LocatedExpr -> Frontend.LocatedExpr -> Result DesugarError Canonical.LocatedExpr) -> Module Frontend.LocatedExpr -> Result DesugarError (Module Canonical.LocatedExpr)
desugarModule desugarExpr module_ =
    module_.declarations
        |> Dict.map (always (desugarDeclaration (desugarExpr module_)))
        |> Dict.combine
        |> Result.map (moduleOfNewType module_)


moduleOfNewType : Module Frontend.LocatedExpr -> Dict VarName (Declaration Canonical.LocatedExpr) -> Module Canonical.LocatedExpr
moduleOfNewType old newDecls =
    { imports = old.imports
    , name = old.name
    , filePath = old.filePath
    , type_ = old.type_
    , exposing_ = old.exposing_

    -- all that code because of this:
    , declarations = newDecls
    }


desugarDeclaration : (Frontend.LocatedExpr -> Result DesugarError Canonical.LocatedExpr) -> Declaration Frontend.LocatedExpr -> Result DesugarError (Declaration Canonical.LocatedExpr)
desugarDeclaration desugarExpr decl =
    decl.body
        |> Declaration.mapBody desugarExpr
        |> Declaration.combine
        |> Result.map (declarationOfNewType decl)


declarationOfNewType : Declaration Frontend.LocatedExpr -> DeclarationBody Canonical.LocatedExpr -> Declaration Canonical.LocatedExpr
declarationOfNewType old newBody =
    { name = old.name
    , module_ = old.module_
    , typeAnnotation = old.typeAnnotation

    -- all that code because of this:
    , body = newBody
    }
