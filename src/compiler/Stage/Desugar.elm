module Stage.Desugar exposing (desugar)

import AST.Canonical as Canonical
import AST.Common.Literal exposing (Literal)
import AST.Frontend as Frontend
import Basics.Extra exposing (flip)
import Common
import Common.Types
    exposing
        ( Binding
        , Module
        , ModuleName
        , Modules
        , Project
        , VarName
        )
import Dict.Any
import Error exposing (DesugarError(..), Error(..))
import Extra.Dict.Any
import Maybe.Extra
import Result.Extra
import Stage.Desugar.Boilerplate as Boilerplate


desugar : Project Frontend.ProjectFields -> Result Error (Project Canonical.ProjectFields)
desugar project =
    Boilerplate.desugarProject (desugarExpr project.modules) project
        |> Result.mapError DesugarError


{-| Combines the various small desugaring passes into one pass that tries all
of them. Thus, we get some separation of concerns - each pass only cares about
a small subset of the whole process!
-}
desugarExpr : Modules Frontend.Expr -> Module Frontend.Expr -> Frontend.Expr -> Result DesugarError Canonical.Expr
desugarExpr modules thisModule expr =
    let
        recurse expr_ =
            desugarExpr modules thisModule expr_
    in
    case expr of
        Frontend.Literal literal ->
            desugarLiteral literal

        Frontend.Var { qualifier, name } ->
            desugarVar modules thisModule qualifier name

        Frontend.Argument varName ->
            desugarArgument varName

        Frontend.Plus e1 e2 ->
            desugarPlus recurse e1 e2

        Frontend.Lambda { arguments, body } ->
            desugarLambda recurse arguments body

        Frontend.Call { fn, argument } ->
            desugarCall recurse fn argument

        Frontend.If { test, then_, else_ } ->
            desugarIf recurse test then_ else_

        Frontend.Let { bindings, body } ->
            desugarLet recurse bindings body

        Frontend.List list ->
            desugarList recurse list

        Frontend.Unit ->
            desugarUnit



-- DESUGAR PASSES


desugarLiteral : Literal -> Result DesugarError Canonical.Expr
desugarLiteral literal =
    Ok (Canonical.Literal literal)


desugarVar : Modules Frontend.Expr -> Module Frontend.Expr -> Maybe ModuleName -> VarName -> Result DesugarError Canonical.Expr
desugarVar modules thisModule maybeModuleName varName =
    findModuleOfVar modules thisModule maybeModuleName varName
        |> Result.fromMaybe (VarNotInEnvOfModule { var = ( maybeModuleName, varName ), module_ = thisModule.name })
        |> Result.map (\moduleName -> Canonical.var moduleName varName)


desugarArgument : VarName -> Result DesugarError Canonical.Expr
desugarArgument varName =
    Ok (Canonical.Argument varName)


desugarPlus : (Frontend.Expr -> Result DesugarError Canonical.Expr) -> Frontend.Expr -> Frontend.Expr -> Result DesugarError Canonical.Expr
desugarPlus recurse e1 e2 =
    Result.map2 Canonical.Plus
        (recurse e1)
        (recurse e2)


desugarLambda : (Frontend.Expr -> Result DesugarError Canonical.Expr) -> List VarName -> Frontend.Expr -> Result DesugarError Canonical.Expr
desugarLambda recurse arguments body =
    recurse body
        |> Result.map (curryLambda arguments)


desugarCall : (Frontend.Expr -> Result DesugarError Canonical.Expr) -> Frontend.Expr -> Frontend.Expr -> Result DesugarError Canonical.Expr
desugarCall recurse fn argument =
    Result.map2
        (\fn_ argument_ ->
            Canonical.Call
                { fn = fn_
                , argument = argument_
                }
        )
        (recurse fn)
        (recurse argument)


desugarIf : (Frontend.Expr -> Result DesugarError Canonical.Expr) -> Frontend.Expr -> Frontend.Expr -> Frontend.Expr -> Result DesugarError Canonical.Expr
desugarIf recurse test then_ else_ =
    Result.map3
        (\test_ then__ else__ ->
            Canonical.If
                { test = test_
                , then_ = then__
                , else_ = else__
                }
        )
        (recurse test)
        (recurse then_)
        (recurse else_)


desugarLet : (Frontend.Expr -> Result DesugarError Canonical.Expr) -> List (Binding Frontend.Expr) -> Frontend.Expr -> Result DesugarError Canonical.Expr
desugarLet recurse bindings body =
    Result.map2
        (\bindings_ body_ ->
            Canonical.Let
                { bindings =
                    bindings_
                        |> List.map (\binding -> ( binding.name, binding ))
                        |> Dict.Any.fromList Common.varNameToString
                , body = body_
                }
        )
        -- TODO a bit mouthful:
        (Result.Extra.combine (List.map (Common.mapBinding recurse >> Common.combineBinding) bindings))
        (recurse body)


desugarList : (Frontend.Expr -> Result DesugarError Canonical.Expr) -> List Frontend.Expr -> Result DesugarError Canonical.Expr
desugarList recurse list =
    List.map recurse list
        |> List.foldl (Result.map2 (::)) (Ok [])
        |> Result.map Canonical.List


desugarUnit : Result DesugarError Canonical.Expr
desugarUnit =
    Ok Canonical.Unit



-- HELPERS


{-| Convert a multi-arg lambda into multiple single-arg lambdas.

    -- from
    Frontend.Lambda [ arg1, arg2 ] body

    -- to
    Canonical.Lambda arg1 (Canonical.Lambda arg2 body)

-}
curryLambda : List VarName -> Canonical.Expr -> Canonical.Expr
curryLambda arguments body =
    List.foldr Canonical.lambda body arguments


{-| We have roughly these options:

  - bar = >baz< (baz being defined elsewhere in this module)
  - import Foo exposing (baz); bar = >baz<
  - import Foo; bar = >Foo.baz<
  - import Foo as F; bar = >F.baz<

In all these cases we need to find the full unaliased module name of the var.

-}
findModuleOfVar : Modules Frontend.Expr -> Module Frontend.Expr -> Maybe ModuleName -> VarName -> Maybe ModuleName
findModuleOfVar modules thisModule maybeModuleName varName =
    {- TODO does this allow for some collisions by "returning early"?
       Should we check that exactly one is Just and the others are Nothing?
    -}
    unqualifiedVarInThisModule thisModule maybeModuleName varName
        |> Maybe.Extra.orElseLazy (\() -> unqualifiedVarInImportedModule modules thisModule maybeModuleName varName)
        |> Maybe.Extra.orElseLazy (\() -> qualifiedVarInImportedModule modules maybeModuleName varName)
        |> Maybe.Extra.orElseLazy (\() -> qualifiedVarInAliasedModule modules thisModule maybeModuleName varName)


unqualifiedVarInThisModule : Module Frontend.Expr -> Maybe ModuleName -> VarName -> Maybe ModuleName
unqualifiedVarInThisModule thisModule maybeModuleName varName =
    if maybeModuleName == Nothing && Dict.Any.member varName thisModule.topLevelDeclarations then
        Just thisModule.name

    else
        Nothing


unqualifiedVarInImportedModule : Modules Frontend.Expr -> Module Frontend.Expr -> Maybe ModuleName -> VarName -> Maybe ModuleName
unqualifiedVarInImportedModule modules thisModule maybeModuleName varName =
    if maybeModuleName == Nothing then
        -- find a module which exposes that var
        thisModule.dependencies
            |> Extra.Dict.Any.find
                (\_ dependency ->
                    Dict.Any.get dependency.moduleName modules
                        |> Maybe.map (Common.exposes varName)
                        |> Maybe.withDefault False
                )
            |> Maybe.map (\( _, dependency ) -> dependency.moduleName)

    else
        Nothing


qualifiedVarInImportedModule : Modules Frontend.Expr -> Maybe ModuleName -> VarName -> Maybe ModuleName
qualifiedVarInImportedModule modules maybeModuleName varName =
    maybeModuleName
        |> Maybe.andThen (flip Dict.Any.get modules)
        |> Maybe.andThen
            (\module_ ->
                if Dict.Any.member varName module_.topLevelDeclarations then
                    Just maybeModuleName

                else
                    Nothing
            )
        |> Maybe.withDefault Nothing


qualifiedVarInAliasedModule : Modules Frontend.Expr -> Module Frontend.Expr -> Maybe ModuleName -> VarName -> Maybe ModuleName
qualifiedVarInAliasedModule modules thisModule maybeModuleName varName =
    let
        unaliasedModuleName =
            Maybe.andThen (Common.unalias thisModule) maybeModuleName
    in
    -- Reusing the existing functionality. TODO is this a good idea?
    qualifiedVarInImportedModule modules unaliasedModuleName varName
