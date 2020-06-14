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
    Module Frontend.LocatedExpr TypeAnnotation PossiblyQualified
    -> Frontend.LocatedExpr
    -> Result DesugarError Canonical.LocatedExpr


type alias DesugarQualifiednessFn =
    PossiblyQualified -> Qualified


{-| Annotation desugaring happens after the declaration desugaring, so the
"body" contents are already qualified and only the TypeAnnotation type changes.
-}
type alias DesugarAnnotationFn =
    Declaration Canonical.LocatedExpr TypeAnnotation Qualified
    -> Result DesugarError (Declaration Canonical.LocatedExpr (Type Qualified) Qualified)


desugarProject :
    DesugarExprFn
    -> DesugarQualifiednessFn
    -> DesugarAnnotationFn
    -> Project Frontend.ProjectFields
    -> Result DesugarError (Project Canonical.ProjectFields)
desugarProject desugarExpr desugarQualifiedness desugarAnnotation project =
    project.modules
        |> Dict.map
            (always
                (desugarModule
                    desugarExpr
                    desugarQualifiedness
                    desugarAnnotation
                )
            )
        |> Dict.combine
        |> Result.map (projectOfNewType project)


projectOfNewType :
    Project Frontend.ProjectFields
    -> Dict ModuleName (Module Canonical.LocatedExpr (Type Qualified) Qualified)
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
    -> DesugarQualifiednessFn
    -> DesugarAnnotationFn
    -> Module Frontend.LocatedExpr TypeAnnotation PossiblyQualified
    -> Result DesugarError (Module Canonical.LocatedExpr (Type Qualified) Qualified)
desugarModule desugarExpr desugarQualifiedness desugarAnnotation module_ =
    module_.declarations
        |> Dict.map
            (always
                (desugarDeclaration
                    (desugarExpr module_)
                    desugarQualifiedness
                    desugarAnnotation
                )
            )
        |> Dict.combine
        |> Result.map (moduleOfNewType module_)


moduleOfNewType :
    Module Frontend.LocatedExpr TypeAnnotation PossiblyQualified
    -> Dict VarName (Declaration Canonical.LocatedExpr (Type Qualified) Qualified)
    -> Module Canonical.LocatedExpr (Type Qualified) Qualified
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
    -> DesugarQualifiednessFn
    -> DesugarAnnotationFn
    -> Declaration Frontend.LocatedExpr TypeAnnotation PossiblyQualified
    -> Result DesugarError (Declaration Canonical.LocatedExpr (Type Qualified) Qualified)
desugarDeclaration desugarExpr desugarQualifiedness desugarAnnotation decl =
    decl.body
        |> Declaration.mapBody desugarExpr desugarQualifiedness
        |> Declaration.combine
        |> Result.map (declarationOfNewType decl)
        |> Result.andThen desugarAnnotation


declarationOfNewType :
    Declaration exprA ann qualifiedness1
    -> DeclarationBody exprB qualifiedness2
    -> Declaration exprB ann qualifiedness2
declarationOfNewType old newBody =
    { name = old.name
    , module_ = old.module_
    , typeAnnotation = old.typeAnnotation

    -- all that code because of this:
    , body = newBody
    }
