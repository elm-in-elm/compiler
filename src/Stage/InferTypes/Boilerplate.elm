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
import Elm.Data.Type exposing (Type)
import Elm.Data.VarName exposing (VarName)
import OurExtras.Dict as Dict
import Stage.InferTypes.SubstitutionMap exposing (SubstitutionMap)


type alias InferExprFn =
    SubstitutionMap
    -> Canonical.LocatedExpr
    -> Result ( TypeError, SubstitutionMap ) ( Typed.LocatedExpr, SubstitutionMap )


type alias UnifyWithTypeAnnotationFn =
    SubstitutionMap
    -> Declaration Typed.LocatedExpr Type
    -> Result ( TypeError, SubstitutionMap ) ( Declaration Typed.LocatedExpr Never, SubstitutionMap )


inferProject :
    InferExprFn
    -> UnifyWithTypeAnnotationFn
    -> Project Canonical.ProjectFields
    -> Result ( TypeError, SubstitutionMap ) ( Project Typed.ProjectFields, SubstitutionMap )
inferProject inferExpr unifyWithTypeAnnotation project =
    project.modules
        |> Dict.map (always (inferModule inferExpr))
        |> Dict.combine
        |> Result.andThen
            (\( stuff, substitutionMap ) ->
                -- TODO start here
                -- stuff needs to be Declaration Typed.LocatedExpr Type
                -- but is Dict String (Module Typed.LocatedExpr Never)
                unifyWithTypeAnnotation substitutionMap stuff
            )
        |> Result.map (projectOfNewType project)


projectOfNewType :
    Project Canonical.ProjectFields
    -> Dict ModuleName (Module Typed.LocatedExpr Never)
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
    InferExprFn
    -> Module Canonical.LocatedExpr Type
    -> Result ( TypeError, SubstitutionMap ) ( Module Typed.LocatedExpr Never, SubstitutionMap )
inferModule inferExpr module_ =
    module_.declarations
        |> Dict.map (always (inferDeclaration inferExpr))
        |> Dict.combine
        |> Result.map (moduleOfNewType module_)


moduleOfNewType :
    Module Canonical.LocatedExpr Type
    -> Dict VarName (Declaration Typed.LocatedExpr Never)
    -> Module Typed.LocatedExpr Never
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
    InferExprFn
    -> SubstitutionMap
    -> Declaration Canonical.LocatedExpr Type
    -> Result ( TypeError, SubstitutionMap ) ( Declaration Typed.LocatedExpr Never, SubstitutionMap )
inferDeclaration inferExpr substitutionMap decl =
    decl.body
        |> Declaration.mapBody (inferExpr substitutionMap)
        |> Declaration.combine
        -- TODO start here - how to get the SubstitutionMap out of the declaration body?
        -- Tried Declaration.combineTuple and similar things - got blocked by
        -- there not being this value for non-Value constructors...
        -- Do we have to provide empty SubstitutionMap?
        -- We should probably return a Maybe, signaling this isn't possible?
        |> Result.map (declarationOfNewType decl)


declarationOfNewType :
    Declaration Canonical.LocatedExpr Type
    -> DeclarationBody Typed.LocatedExpr
    -> Declaration Typed.LocatedExpr Never
declarationOfNewType old newBody =
    { name = old.name
    , module_ = old.module_
    , typeAnnotation = Nothing

    -- all that code because of this:
    , body = newBody
    }
