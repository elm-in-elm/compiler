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


desugarProject : (Module Frontend.Expr -> Frontend.Expr -> Result DesugarError Canonical.Expr) -> Project Frontend.ProjectFields -> Result DesugarError (Project Canonical.ProjectFields)
desugarProject desugarExpr project =
    project.modules
        |> Dict.Any.map (always (desugarModule desugarExpr))
        |> Extra.Dict.Any.combine Common.moduleNameToString
        |> Result.map (projectOfNewType project)


projectOfNewType : Project Frontend.ProjectFields -> Modules Canonical.Expr -> Project Canonical.ProjectFields
projectOfNewType old modules =
    { elmJson = old.elmJson
    , mainFilePath = old.mainFilePath
    , mainModuleName = old.mainModuleName
    , sourceDirectory = old.sourceDirectory

    -- all that code because of this:
    , modules = modules
    }


desugarModule : (Module Frontend.Expr -> Frontend.Expr -> Result DesugarError Canonical.Expr) -> Module Frontend.Expr -> Result DesugarError (Module Canonical.Expr)
desugarModule desugarExpr module_ =
    module_.topLevelDeclarations
        |> Dict.Any.map (always (desugarTopLevelDeclaration (desugarExpr module_)))
        |> Extra.Dict.Any.combine Common.varNameToString
        |> Result.map (moduleOfNewType module_)


moduleOfNewType : Module Frontend.Expr -> Dict_ VarName (TopLevelDeclaration Canonical.Expr) -> Module Canonical.Expr
moduleOfNewType old newDecls =
    { dependencies = old.dependencies
    , name = old.name
    , filePath = old.filePath
    , type_ = old.type_
    , exposing_ = old.exposing_

    -- all that code because of this:
    , topLevelDeclarations = newDecls
    }


desugarTopLevelDeclaration : (Frontend.Expr -> Result DesugarError Canonical.Expr) -> TopLevelDeclaration Frontend.Expr -> Result DesugarError (TopLevelDeclaration Canonical.Expr)
desugarTopLevelDeclaration desugarExpr decl =
    desugarExpr decl.body
        |> Result.map (topLevelDeclarationOfNewType decl)


topLevelDeclarationOfNewType : TopLevelDeclaration Frontend.Expr -> Canonical.Expr -> TopLevelDeclaration Canonical.Expr
topLevelDeclarationOfNewType old newBody =
    { name = old.name
    , module_ = old.module_

    -- all that code because of this:
    , body = newBody
    }
