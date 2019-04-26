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
        )
import Dict.Any
import Error exposing (Error(..), TypeError)
import Extra.Dict.Any


{-| We're using Hindley-Milner algorithm. It has essentially three parts:

1.  Annotate all the subexpressions with IDs
2.  Generate equations (constraints) between the IDs (apply type inference rules)
3.  Solve the equations (find something called "most general unifier" for a specific ID)

-}
typecheck : Project Canonical.ProjectFields -> Result Error (Project Typechecked.ProjectFields)
typecheck project =
    project.modules
        |> typecheckModules
        |> projectOfNewType project


{-| Boilerplate, for nice pipelined `typecheck` function.
-}
typecheckModules : Modules Canonical.Expr -> Result TypeError (Modules Typechecked.Expr)
typecheckModules modules =
    modules
        {- TODO this can't work, we'll have to typecheck across modules, right?
           This only typechecks each module separately...
        -}
        |> Dict.Any.map typecheckModule
        |> Extra.Dict.Any.combine Common.moduleNameToString
        |> Result.mapError TypeError


{-| Boilerplate.

Elm's record update syntax doesn't allow changing the type, so we have to
create the record from scratch again.

-}
projectOfNewType : Project a -> Result TypeError (Modules Typechecked.Expr) -> Result Error (Project Typechecked.ProjectFields)
projectOfNewType p modules =
    modules
        |> Result.mapError TypeError
        |> Result.map
            (\newModules ->
                { elmJson = p.elmJson
                , mainFilePath = p.mainFilePath
                , mainModuleName = p.mainModuleName
                , sourceDirectory = p.sourceDirectory

                -- all that because of this:
                , modules = newModules
                }
            )


{-| Boilerplate.
-}
typecheckModule : Module Canonical.Expr -> Result TypeError (Module Typechecked.Expr)
typecheckModule module_ =
    module_.topLevelDeclarations
        |> Dict.Any.map (Debug.todo "after map")
        |> Extra.Dict.Any.combine Common.varNameToString
        |> Result.map moduleOfNewType module_


{-| Boilerplate.
-}
moduleOfNewType : Module Canonical.Expr -> Dict_ VarName (TopLevelDeclaration Typed.Expr) -> Module Typed.Expr
moduleOfNewType old newDecls =
    { dependencies = old.dependencies
    , name = old.name
    , filePath = old.filePath
    , topLevelDeclarations = newDecls
    , type_ = old.type_
    , exposing_ = old.exposing_
    }


{-| The real work starts here!
-}
typecheckExpr : Canonical.Expr -> Result TypeError Typechecked.Expr
typecheckExpr expr =
    Debug.todo "typecheckExpr"
