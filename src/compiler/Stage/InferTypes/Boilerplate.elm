module Stage.InferTypes.Boilerplate exposing (inferProject)

import AST.Canonical as Canonical
import AST.Typed as Typed
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
import Error exposing (TypeError)
import Extra.Dict.Any


inferProject : (Canonical.LocatedExpr -> Result TypeError Typed.LocatedExpr) -> Project Canonical.ProjectFields -> Result TypeError (Project Typed.ProjectFields)
inferProject inferExpr project =
    project.modules
        |> Dict.Any.map (always (inferModule inferExpr))
        |> Extra.Dict.Any.combine Common.moduleNameToString
        |> Result.map (projectOfNewType project)


projectOfNewType : Project Canonical.ProjectFields -> Modules Typed.LocatedExpr -> Project Typed.ProjectFields
projectOfNewType old modules =
    { elmJson = old.elmJson
    , mainFilePath = old.mainFilePath
    , mainModuleName = old.mainModuleName
    , sourceDirectory = old.sourceDirectory

    -- all that code because of this:
    , modules = modules
    }


inferModule : (Canonical.LocatedExpr -> Result TypeError Typed.LocatedExpr) -> Module Canonical.LocatedExpr -> Result TypeError (Module Typed.LocatedExpr)
inferModule inferExpr module_ =
    module_.topLevelDeclarations
        |> Dict.Any.map (always (inferTopLevelDeclaration inferExpr))
        |> Extra.Dict.Any.combine Common.varNameToString
        |> Result.map (moduleOfNewType module_)


moduleOfNewType : Module Canonical.LocatedExpr -> Dict_ VarName (TopLevelDeclaration Typed.LocatedExpr) -> Module Typed.LocatedExpr
moduleOfNewType old newDecls =
    { dependencies = old.dependencies
    , name = old.name
    , filePath = old.filePath
    , type_ = old.type_
    , exposing_ = old.exposing_

    -- all that code because of this:
    , topLevelDeclarations = newDecls
    }


inferTopLevelDeclaration : (Canonical.LocatedExpr -> Result TypeError Typed.LocatedExpr) -> TopLevelDeclaration Canonical.LocatedExpr -> Result TypeError (TopLevelDeclaration Typed.LocatedExpr)
inferTopLevelDeclaration inferExpr decl =
    inferExpr decl.body
        |> Result.map (topLevelDeclarationOfNewType decl)


topLevelDeclarationOfNewType : TopLevelDeclaration Canonical.LocatedExpr -> Typed.LocatedExpr -> TopLevelDeclaration Typed.LocatedExpr
topLevelDeclarationOfNewType old newBody =
    { name = old.name
    , module_ = old.module_

    -- all that code because of this:
    , body = newBody
    }
