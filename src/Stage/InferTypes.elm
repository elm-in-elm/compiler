module Stage.InferTypes exposing (inferTypes)

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


{-| We're using Hindley-Milner algorithm (Algorithm W). It has essentially three parts:

1.  Annotate all the subexpressions with IDs
2.  Generate equations (constraints) between the IDs (apply type inference rules)
3.  Solve the equations (find something called "most general unifier" for a specific ID)

See the `inferExpr` function for more! (Stuff in between is just one huge boilerplate.)

-}
inferTypes : Project Canonical.ProjectFields -> Result Error (Project Typed.ProjectFields)
inferTypes project =
    project.modules
        {- TODO this can't work, we'll have to typecheck across modules, right?
           This only typechecks each module separately...
        -}
        |> Dict.Any.map (always inferModule)
        |> Extra.Dict.Any.combine Common.moduleNameToString
        |> Result.mapError TypeError
        |> Result.map (projectOfNewType project)


{-| Boilerplate.
-}
projectOfNewType : Project Canonical.ProjectFields -> Modules Typed.Expr -> Project Typed.ProjectFields
projectOfNewType old modules =
    { elmJson = old.elmJson
    , mainFilePath = old.mainFilePath
    , mainModuleName = old.mainModuleName
    , sourceDirectory = old.sourceDirectory

    -- all that code because of this:
    , modules = modules
    }


{-| Boilerplate.
-}
inferModule : Module Canonical.Expr -> Result TypeError (Module Typed.Expr)
inferModule module_ =
    module_.topLevelDeclarations
        |> Dict.Any.map (always inferTopLevelDeclaration)
        |> Extra.Dict.Any.combine Common.varNameToString
        |> Result.map (moduleOfNewType module_)


{-| Boilerplate.
-}
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


{-| Boilerplate.
-}
inferTopLevelDeclaration : TopLevelDeclaration Canonical.Expr -> Result TypeError (TopLevelDeclaration Typed.Expr)
inferTopLevelDeclaration decl =
    inferExpr decl.body
        |> Result.map (topLevelDeclarationOfNewType decl)


{-| Boilerplate.
-}
topLevelDeclarationOfNewType : TopLevelDeclaration Canonical.Expr -> Typed.Expr -> TopLevelDeclaration Typed.Expr
topLevelDeclarationOfNewType old newBody =
    { name = old.name
    , module_ = old.module_

    -- all that code because of this:
    , body = newBody
    }


{-| The real work starts here!
-}
inferExpr : Canonical.Expr -> Result TypeError Typed.Expr
inferExpr expr =
    Debug.todo "inferExpr"
