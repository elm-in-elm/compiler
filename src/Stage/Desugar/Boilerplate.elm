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
import Elm.Data.Type exposing (Type)
import Elm.Data.TypeAnnotation exposing (TypeAnnotation)
import Elm.Data.VarName exposing (VarName)
import OurExtras.Dict as Dict


desugarProject : (Module Frontend.LocatedExpr TypeAnnotation -> Frontend.LocatedExpr -> Result DesugarError Canonical.LocatedExpr) -> Project Frontend.ProjectFields -> Result DesugarError (Project Canonical.ProjectFields)
desugarProject desugarExpr project =
    project.modules
        |> Dict.map (always (desugarModule desugarExpr))
        |> Dict.combine
        |> Result.map (projectOfNewType project)


projectOfNewType : Project Frontend.ProjectFields -> Dict ModuleName (Module Canonical.LocatedExpr Type) -> Project Canonical.ProjectFields
projectOfNewType old modules =
    { elmJson = old.elmJson
    , mainFilePath = old.mainFilePath
    , mainModuleName = old.mainModuleName
    , sourceDirectory = old.sourceDirectory

    -- all that code because of this:
    , modules = modules
    }


desugarModule : (Module Frontend.LocatedExpr TypeAnnotation -> Frontend.LocatedExpr -> Result DesugarError Canonical.LocatedExpr) -> Module Frontend.LocatedExpr TypeAnnotation -> Result DesugarError (Module Canonical.LocatedExpr Type)
desugarModule desugarExpr module_ =
    module_.declarations
        |> Dict.map (always (desugarDeclaration (desugarExpr module_)))
        |> Dict.combine
        |> Result.map (moduleOfNewType module_)


moduleOfNewType : Module Frontend.LocatedExpr TypeAnnotation -> Dict VarName (Declaration Canonical.LocatedExpr Type) -> Module Canonical.LocatedExpr Type
moduleOfNewType old newDecls =
    { imports = old.imports
    , name = old.name
    , filePath = old.filePath
    , type_ = old.type_
    , exposing_ = old.exposing_

    -- all that code because of this:
    , declarations = newDecls
    }


desugarDeclaration : (Frontend.LocatedExpr -> Result DesugarError Canonical.LocatedExpr) -> Declaration Frontend.LocatedExpr TypeAnnotation -> Result DesugarError (Declaration Canonical.LocatedExpr Type)
desugarDeclaration desugarExpr decl =
    decl.body
        |> Declaration.mapBody desugarExpr
        |> Declaration.combine
        |> Result.map (declarationOfNewType decl)


declarationOfNewType : Declaration Frontend.LocatedExpr TypeAnnotation -> DeclarationBody Canonical.LocatedExpr -> Declaration Canonical.LocatedExpr Type
declarationOfNewType old newBody =
    { name = old.name
    , module_ = old.module_
    , typeAnnotation = old.typeAnnotation

    -- all that code because of this:
    , body = newBody
    }
