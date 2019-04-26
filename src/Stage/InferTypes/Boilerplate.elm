module Stage.InferTypes.Boilerplate exposing (inferModule, projectOfNewType)

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


projectOfNewType : Project Canonical.ProjectFields -> Modules Typed.Expr -> Project Typed.ProjectFields
projectOfNewType old modules =
    { elmJson = old.elmJson
    , mainFilePath = old.mainFilePath
    , mainModuleName = old.mainModuleName
    , sourceDirectory = old.sourceDirectory

    -- all that code because of this:
    , modules = modules
    }


inferModule : (Canonical.Expr -> Result TypeError Typed.Expr) -> Module Canonical.Expr -> Result TypeError (Module Typed.Expr)
inferModule inferExpr module_ =
    module_.topLevelDeclarations
        |> Dict.Any.map (always (inferTopLevelDeclaration inferExpr))
        |> Extra.Dict.Any.combine Common.varNameToString
        |> Result.map (moduleOfNewType module_)


moduleOfNewType : Module Canonical.Expr -> Dict_ VarName (TopLevelDeclaration Typed.Expr) -> Module Typed.Expr
moduleOfNewType old newDecls =
    { dependencies = old.dependencies
    , name = old.name
    , filePath = old.filePath
    , type_ = old.type_
    , exposing_ = old.exposing_

    -- all that code because of this:
    , topLevelDeclarations = newDecls
    }


inferTopLevelDeclaration : (Canonical.Expr -> Result TypeError Typed.Expr) -> TopLevelDeclaration Canonical.Expr -> Result TypeError (TopLevelDeclaration Typed.Expr)
inferTopLevelDeclaration inferExpr decl =
    inferExpr decl.body
        |> Result.map (topLevelDeclarationOfNewType decl)


topLevelDeclarationOfNewType : TopLevelDeclaration Canonical.Expr -> Typed.Expr -> TopLevelDeclaration Typed.Expr
topLevelDeclarationOfNewType old newBody =
    { name = old.name
    , module_ = old.module_

    -- all that code because of this:
    , body = newBody
    }
