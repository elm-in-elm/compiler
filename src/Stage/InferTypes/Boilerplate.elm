module Stage.InferTypes.Boilerplate exposing
    ( inferModule
    , inferProject
    )

import Dict exposing (Dict)
import Elm.AST.Canonical as Canonical
import Elm.AST.Typed as Typed
import Elm.Compiler.Error exposing (TypeError)
import Elm.Data.Declaration as Declaration exposing (Declaration, DeclarationBody(..))
import Elm.Data.Module exposing (Module)
import Elm.Data.ModuleName exposing (ModuleName)
import Elm.Data.Project exposing (Project)
import Elm.Data.VarName exposing (VarName)
import OurExtras.Dict as Dict


inferProject :
    (Canonical.LocatedExpr -> Result TypeError Typed.LocatedExpr)
    -> Project Canonical.ProjectFields
    -> Result TypeError (Project Typed.ProjectFields)
inferProject inferExpr project =
    project.modules
        |> Dict.map (always (inferModule inferExpr))
        |> Dict.combine
        |> Result.map (projectOfNewType project)


projectOfNewType :
    Project Canonical.ProjectFields
    -> Dict ModuleName (Module Typed.LocatedExpr)
    -> Project Typed.ProjectFields
projectOfNewType old modules =
    { elmJson = old.elmJson
    , mainFilePath = old.mainFilePath
    , mainModuleName = old.mainModuleName
    , sourceDirectory = old.sourceDirectory

    -- all that code because of this:
    , modules = modules
    }


inferModule :
    (Canonical.LocatedExpr -> Result TypeError Typed.LocatedExpr)
    -> Module Canonical.LocatedExpr
    -> Result TypeError (Module Typed.LocatedExpr)
inferModule inferExpr module_ =
    module_.declarations
        |> Dict.map (always (inferDeclaration inferExpr))
        |> Dict.combine
        |> Result.map (moduleOfNewType module_)


moduleOfNewType :
    Module Canonical.LocatedExpr
    -> Dict VarName (Declaration Typed.LocatedExpr)
    -> Module Typed.LocatedExpr
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
    (Canonical.LocatedExpr -> Result TypeError Typed.LocatedExpr)
    -> Declaration Canonical.LocatedExpr
    -> Result TypeError (Declaration Typed.LocatedExpr)
inferDeclaration inferExpr decl =
    decl.body
        |> Declaration.mapBody inferExpr
        |> Declaration.combine
        |> Result.map (declarationOfNewType decl)


declarationOfNewType :
    Declaration Canonical.LocatedExpr
    -> DeclarationBody Typed.LocatedExpr
    -> Declaration Typed.LocatedExpr
declarationOfNewType old newBody =
    { name = old.name
    , module_ = old.module_
    , typeAnnotation = old.typeAnnotation

    -- all that code because of this:
    , body = newBody
    }
