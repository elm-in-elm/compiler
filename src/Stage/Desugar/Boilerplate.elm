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
import Elm.Data.Qualifiedness exposing (PossiblyQualified, Qualified)
import Elm.Data.Type exposing (Type, TypeOrId)
import Elm.Data.TypeAnnotation exposing (TypeAnnotation)
import Elm.Data.VarName exposing (VarName)
import OurExtras.Dict as Dict


type alias DesugarExprFn =
    Module Frontend.LocatedExpr TypeAnnotation (Maybe String)
    -> Frontend.LocatedExpr
    -> Result DesugarError Canonical.LocatedExpr


type alias DesugarUtmFn =
    Maybe String -> String


{-| Annotation desugaring happens after the declaration desugaring
-}
type alias DesugarAnnotationFn =
    Declaration Canonical.LocatedExpr TypeAnnotation String
    -> Result DesugarError (Declaration Canonical.LocatedExpr (Type Qualified) String)


desugarProject :
    DesugarExprFn
    -> DesugarUtmFn
    -> DesugarAnnotationFn
    -> Project Frontend.ProjectFields
    -> Result DesugarError (Project Canonical.ProjectFields)
desugarProject desugarExpr desugarUtm desugarAnnotation project =
    project.modules
        |> Dict.map
            (always
                (desugarModule
                    desugarExpr
                    desugarUtm
                    desugarAnnotation
                )
            )
        |> Dict.combine
        |> Result.map (projectOfNewType project)


projectOfNewType :
    Project Frontend.ProjectFields
    -> Dict ModuleName (Module Canonical.LocatedExpr (Type Qualified) String)
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
    -> DesugarUtmFn
    -> DesugarAnnotationFn
    -> Module Frontend.LocatedExpr TypeAnnotation (Maybe String)
    -> Result DesugarError (Module Canonical.LocatedExpr (Type Qualified) String)
desugarModule desugarExpr desugarUtm desugarAnnotation module_ =
    module_.declarations
        |> Dict.map
            (always
                (desugarDeclaration
                    (desugarExpr module_)
                    desugarUtm
                    desugarAnnotation
                )
            )
        |> Dict.combine
        |> Result.map (moduleOfNewType module_)


moduleOfNewType :
    Module Frontend.LocatedExpr TypeAnnotation (Maybe String)
    -> Dict VarName (Declaration Canonical.LocatedExpr (Type Qualified) String)
    -> Module Canonical.LocatedExpr (Type Qualified) String
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
    -> DesugarUtmFn
    -> DesugarAnnotationFn
    -> Declaration Frontend.LocatedExpr TypeAnnotation (Maybe String)
    -> Result DesugarError (Declaration Canonical.LocatedExpr (Type Qualified) String)
desugarDeclaration desugarExpr desugarUtm desugarAnnotation decl =
    decl.body
        |> Declaration.mapBody desugarExpr desugarUtm
        |> Declaration.combine
        |> Result.map (declarationOfNewType decl)
        |> Result.andThen desugarAnnotation


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
