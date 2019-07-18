module Stage.Desugar exposing (desugar)

import AST.Canonical as Canonical
import AST.Common.Literal exposing (Literal)
import AST.Common.Located as Located
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
desugarExpr :
    Modules Frontend.LocatedExpr
    -> Module Frontend.LocatedExpr
    -> Frontend.LocatedExpr
    -> Result DesugarError Canonical.LocatedExpr
desugarExpr modules thisModule located =
    let
        recurse =
            desugarExpr modules thisModule

        return expr =
            Ok (Located.replaceWith expr located)

        map fn =
            Result.map
                (\expr ->
                    Located.replaceWith
                        (fn expr)
                        located
                )

        map2 fn =
            Result.map2
                (\expr1 expr2 ->
                    Located.replaceWith
                        (fn expr1 expr2)
                        located
                )

        map3 fn =
            Result.map3
                (\expr1 expr2 expr3 ->
                    Located.replaceWith
                        (fn expr1 expr2 expr3)
                        located
                )
    in
    case Located.unwrap located of
        Frontend.Literal literal ->
            return <| Canonical.Literal literal

        Frontend.Var { qualifier, name } ->
            findModuleOfVar modules thisModule qualifier name
                |> Result.fromMaybe
                    (VarNotInEnvOfModule
                        { var = ( qualifier, name )
                        , module_ = thisModule.name
                        }
                    )
                |> map (\moduleName -> Canonical.var moduleName name)

        Frontend.Argument varName ->
            return <| Canonical.Argument varName

        Frontend.Plus e1 e2 ->
            map2 Canonical.Plus
                (recurse e1)
                (recurse e2)

        Frontend.Lambda { arguments, body } ->
            recurse body
                |> Result.map (curryLambda located arguments)

        Frontend.Call { fn, argument } ->
            map2
                (\fn_ argument_ ->
                    Canonical.Call
                        { fn = fn_
                        , argument = argument_
                        }
                )
                (recurse fn)
                (recurse argument)

        Frontend.If { test, then_, else_ } ->
            map3
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

        Frontend.Let { bindings, body } ->
            map2
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

        Frontend.List items ->
            List.map recurse items
                |> List.foldr (Result.map2 (::)) (Ok [])
                |> map Canonical.List

        Frontend.Tuple e1 e2 ->
            map2 Canonical.Tuple
                (recurse e1)
                (recurse e2)

        Frontend.Tuple3 e1 e2 e3 ->
            map3 Canonical.Tuple3
                (recurse e1)
                (recurse e2)
                (recurse e3)

        Frontend.Unit ->
            return Canonical.Unit



-- HELPERS


{-| Convert a multi-arg lambda into multiple single-arg lambdas.

    -- from
    Frontend.Lambda [ arg1, arg2 ] body

    -- to
    Canonical.Lambda arg1 (Canonical.Lambda arg2 body)

-}
curryLambda : Frontend.LocatedExpr -> List VarName -> Canonical.LocatedExpr -> Canonical.LocatedExpr
curryLambda located arguments body =
    List.foldr
        (\argument body_ ->
            Located.replaceWith
                (Canonical.lambda argument body_)
                located
        )
        body
        arguments


{-| We have roughly these options:

  - bar = >baz< (baz being defined elsewhere in this module)
  - import Foo exposing (baz); bar = >baz<
  - import Foo; bar = >Foo.baz<
  - import Foo as F; bar = >F.baz<

In all these cases we need to find the full unaliased module name of the var.

-}
findModuleOfVar : Modules Frontend.LocatedExpr -> Module Frontend.LocatedExpr -> Maybe ModuleName -> VarName -> Maybe ModuleName
findModuleOfVar modules thisModule maybeModuleName varName =
    {- TODO does this allow for some collisions by "returning early"?
       Should we check that exactly one is Just and the others are Nothing?
    -}
    unqualifiedVarInThisModule thisModule maybeModuleName varName
        |> Maybe.Extra.orElseLazy (\() -> unqualifiedVarInImportedModule modules thisModule maybeModuleName varName)
        |> Maybe.Extra.orElseLazy (\() -> qualifiedVarInImportedModule modules maybeModuleName varName)
        |> Maybe.Extra.orElseLazy (\() -> qualifiedVarInAliasedModule modules thisModule maybeModuleName varName)


unqualifiedVarInThisModule : Module Frontend.LocatedExpr -> Maybe ModuleName -> VarName -> Maybe ModuleName
unqualifiedVarInThisModule thisModule maybeModuleName varName =
    if maybeModuleName == Nothing && Dict.Any.member varName thisModule.topLevelDeclarations then
        Just thisModule.name

    else
        Nothing


unqualifiedVarInImportedModule : Modules Frontend.LocatedExpr -> Module Frontend.LocatedExpr -> Maybe ModuleName -> VarName -> Maybe ModuleName
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


qualifiedVarInImportedModule : Modules Frontend.LocatedExpr -> Maybe ModuleName -> VarName -> Maybe ModuleName
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


qualifiedVarInAliasedModule : Modules Frontend.LocatedExpr -> Module Frontend.LocatedExpr -> Maybe ModuleName -> VarName -> Maybe ModuleName
qualifiedVarInAliasedModule modules thisModule maybeModuleName varName =
    let
        unaliasedModuleName =
            Maybe.andThen (Common.unalias thisModule) maybeModuleName
    in
    -- Reusing the existing functionality. TODO is this a good idea?
    qualifiedVarInImportedModule modules unaliasedModuleName varName
