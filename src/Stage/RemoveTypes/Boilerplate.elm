module Stage.RemoveTypes.Boilerplate exposing (projectOfNewType, removeTypesInModule)

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
import Error exposing (Error(..), TypeError)
import Extra.Dict.Any


projectOfNewType : Project Typed.ProjectFields -> Modules Canonical.Expr -> Project Canonical.ProjectFields
projectOfNewType old modules =
    { elmJson = old.elmJson
    , mainFilePath = old.mainFilePath
    , mainModuleName = old.mainModuleName
    , sourceDirectory = old.sourceDirectory

    -- all that code because of this:
    , modules = modules
    }


removeTypesInModule : (Typed.Expr -> Canonical.Expr) -> Module Typed.Expr -> Module Canonical.Expr
removeTypesInModule removeTypesInExpr module_ =
    module_.topLevelDeclarations
        |> Dict.Any.map (always (removeTypesInTopLevelDeclaration removeTypesInExpr))
        |> moduleOfNewType module_


moduleOfNewType : Module Typed.Expr -> Dict_ VarName (TopLevelDeclaration Canonical.Expr) -> Module Canonical.Expr
moduleOfNewType old newDecls =
    { dependencies = old.dependencies
    , name = old.name
    , filePath = old.filePath
    , type_ = old.type_
    , exposing_ = old.exposing_

    -- all that code because of this:
    , topLevelDeclarations = newDecls
    }


removeTypesInTopLevelDeclaration : (Typed.Expr -> Canonical.Expr) -> TopLevelDeclaration Typed.Expr -> TopLevelDeclaration Canonical.Expr
removeTypesInTopLevelDeclaration removeTypesInExpr decl =
    removeTypesInExpr decl.body
        |> topLevelDeclarationOfNewType decl


topLevelDeclarationOfNewType : TopLevelDeclaration Typed.Expr -> Canonical.Expr -> TopLevelDeclaration Canonical.Expr
topLevelDeclarationOfNewType old newBody =
    { name = old.name
    , module_ = old.module_

    -- all that code because of this:
    , body = newBody
    }
