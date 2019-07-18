module Stage.Desugar.Boilerplate exposing (desugarProject)

import AST.Canonical as Canonical
import AST.Frontend as Frontend
import Common
import Common.Types
    exposing
        ( Dict_
        , Module
        , Modules
        , Project
        , TopLevelDeclaration
        , VarName
        )
import Dict.Any
import Error exposing (DesugarError)
import Extra.Dict.Any


desugarProject : (Module Frontend.LocatedExpr -> Frontend.LocatedExpr -> Result DesugarError Canonical.LocatedExpr) -> Project Frontend.ProjectFields -> Result DesugarError (Project Canonical.ProjectFields)
desugarProject desugarExpr project =
    project.modules
        |> Dict.Any.map (always (desugarModule desugarExpr))
        |> Extra.Dict.Any.combine Common.moduleNameToString
        |> Result.map (projectOfNewType project)


projectOfNewType : Project Frontend.ProjectFields -> Modules Canonical.LocatedExpr -> Project Canonical.ProjectFields
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
    module_.topLevelDeclarations
        |> Dict.Any.map (always (desugarTopLevelDeclaration (desugarExpr module_)))
        |> Extra.Dict.Any.combine Common.varNameToString
        |> Result.map (moduleOfNewType module_)


moduleOfNewType : Module Frontend.LocatedExpr -> Dict_ VarName (TopLevelDeclaration Canonical.LocatedExpr) -> Module Canonical.LocatedExpr
moduleOfNewType old newDecls =
    { dependencies = old.dependencies
    , name = old.name
    , filePath = old.filePath
    , type_ = old.type_
    , exposing_ = old.exposing_

    -- all that code because of this:
    , topLevelDeclarations = newDecls
    }


desugarTopLevelDeclaration : (Frontend.LocatedExpr -> Result DesugarError Canonical.LocatedExpr) -> TopLevelDeclaration Frontend.LocatedExpr -> Result DesugarError (TopLevelDeclaration Canonical.LocatedExpr)
desugarTopLevelDeclaration desugarExpr decl =
    desugarExpr decl.body
        |> Result.map (topLevelDeclarationOfNewType decl)


topLevelDeclarationOfNewType : TopLevelDeclaration Frontend.LocatedExpr -> Canonical.LocatedExpr -> TopLevelDeclaration Canonical.LocatedExpr
topLevelDeclarationOfNewType old newBody =
    { name = old.name
    , module_ = old.module_

    -- all that code because of this:
    , body = newBody
    }
