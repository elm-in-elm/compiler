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
import Stage.InferTypes.Boilerplate exposing (..)


{-| We're using Hindley-Milner algorithm (Algorithm W). It has essentially three parts:

1.  Annotate all the subexpressions with IDs
2.  Generate equations (constraints) between the IDs (apply type inference rules)
3.  Solve the equations (find something called "most general unifier" for a specific ID)

-}
inferTypes : Project Canonical.ProjectFields -> Result Error (Project Typed.ProjectFields)
inferTypes project =
    project.modules
        {- TODO this can't work, we'll have to typecheck across modules, right?
           This only typechecks each module separately...
        -}
        |> Dict.Any.map (always (inferModule inferExpr))
        |> Extra.Dict.Any.combine Common.moduleNameToString
        |> Result.mapError TypeError
        |> Result.map (projectOfNewType project)


inferExpr : Canonical.Expr -> Result TypeError Typed.Expr
inferExpr expr =
    Debug.todo "inferExpr"
