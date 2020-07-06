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
import Elm.Data.Type.Concrete exposing (ConcreteType)
import Elm.Data.TypeAnnotation exposing (TypeAnnotation)
import Elm.Data.VarName exposing (VarName)
import OurExtras.Dict as Dict


type alias ThisModule =
    Module Frontend.LocatedExpr TypeAnnotation PossiblyQualified


type alias DesugarExprFn =
    Frontend.LocatedExpr -> Result DesugarError Canonical.LocatedExpr


type alias DesugarQualifiednessFn =
    VarName -> PossiblyQualified -> Result DesugarError Qualified


{-| Annotation desugaring happens after the declaration desugaring, so the
"body" contents are already qualified and only the TypeAnnotation type changes.
-}
type alias DesugarAnnotationFn =
    Declaration Canonical.LocatedExpr TypeAnnotation Qualified
    -> Result DesugarError (Declaration Canonical.LocatedExpr (ConcreteType Qualified) Qualified)


desugarProject :
    (ThisModule -> DesugarExprFn)
    -> (ThisModule -> DesugarQualifiednessFn)
    -> (ThisModule -> DesugarAnnotationFn)
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
    -> Dict ModuleName (Module Canonical.LocatedExpr (ConcreteType Qualified) Qualified)
    -> Project Canonical.ProjectFields
projectOfNewType old modules =
    { elmJson = old.elmJson
    , mainFilePath = old.mainFilePath
    , mainModuleName = old.mainModuleName
    , sourceDirectories = old.sourceDirectories

    -- all that code because of this:
    , modules = modules
    }


desugarModule :
    (ThisModule -> DesugarExprFn)
    -> (ThisModule -> DesugarQualifiednessFn)
    -> (ThisModule -> DesugarAnnotationFn)
    -> ThisModule
    -> Result DesugarError (Module Canonical.LocatedExpr (ConcreteType Qualified) Qualified)
desugarModule desugarExpr desugarQualifiedness desugarAnnotation module_ =
    module_.declarations
        |> Dict.map
            (always
                (desugarDeclaration
                    (desugarExpr module_)
                    (desugarQualifiedness module_)
                    (desugarAnnotation module_)
                )
            )
        |> Dict.combine
        |> Result.map (moduleOfNewType module_)


moduleOfNewType :
    Module Frontend.LocatedExpr TypeAnnotation PossiblyQualified
    -> Dict VarName (Declaration Canonical.LocatedExpr (ConcreteType Qualified) Qualified)
    -> Module Canonical.LocatedExpr (ConcreteType Qualified) Qualified
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
    DesugarExprFn
    -> DesugarQualifiednessFn
    -> DesugarAnnotationFn
    -> Declaration Frontend.LocatedExpr TypeAnnotation PossiblyQualified
    -> Result DesugarError (Declaration Canonical.LocatedExpr (ConcreteType Qualified) Qualified)
desugarDeclaration desugarExpr desugarQualifiedness desugarAnnotation decl =
    decl.body
        |> Declaration.mapBody
            desugarExpr
            identity
            (desugarQualifiedness decl.name)
        |> Declaration.combineValue
        |> Result.andThen Declaration.combineType
        |> Result.map (declarationOfNewType decl)
        |> Result.andThen desugarAnnotation


declarationOfNewType :
    Declaration expr1 ann1 qualifiedness1
    -> DeclarationBody expr2 ann2 qualifiedness2
    -> Declaration expr2 ann2 qualifiedness2
declarationOfNewType old newBody =
    { name = old.name
    , module_ = old.module_

    -- all that code because of this:
    , body = newBody
    }
