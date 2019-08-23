module Stage.Desugar.Boilerplate exposing (desugarProject)

import AST.Canonical as Canonical
import AST.Frontend as Frontend
import AssocList as Dict exposing (Dict)
import Data.Declaration as Declaration exposing (Declaration, DeclarationBody)
import Data.Module exposing (Module, Modules)
import Data.Project exposing (Project)
import Data.VarName exposing (VarName)
import Error exposing (DesugarError)
import OurExtras.AssocList as Dict


desugarProject : (Module Frontend.LocatedExpr -> Frontend.LocatedExpr -> Result DesugarError Canonical.LocatedExpr) -> Project Frontend.ProjectFields -> Result DesugarError (Project Canonical.ProjectFields)
desugarProject desugarExpr project =
    project.modules
        |> Dict.map (always (desugarModule desugarExpr))
        |> Dict.combine
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

    -- all that code because of this:
    , body = newBody
    }
