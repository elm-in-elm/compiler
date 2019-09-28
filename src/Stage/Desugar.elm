module Stage.Desugar exposing (desugar, desugarExpr)

import Basics.Extra exposing (flip)
import Dict
import Dict.Extra as Dict
import Elm.AST.Canonical as Canonical
import Elm.AST.Common.Literal exposing (Literal)
import Elm.AST.Common.Located as Located
import Elm.AST.Frontend as Frontend
import Elm.Compiler.Error exposing (DesugarError(..), Error(..))
import Elm.Data.Binding as Binding
import Elm.Data.Module as Module exposing (Module, Modules)
import Elm.Data.ModuleName exposing (ModuleName)
import Elm.Data.Project exposing (Project)
import Elm.Data.VarName exposing (VarName)
import Maybe.Extra
import Result.Extra as Result
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

        Frontend.Var var ->
            findModuleOfVar modules thisModule var
                |> Result.fromMaybe
                    (VarNotInEnvOfModule
                        { var = var
                        , module_ = thisModule.name
                        }
                    )
                |> map (\moduleName -> Canonical.Var { module_ = moduleName, name = var.name })

        Frontend.Argument varName ->
            return <| Canonical.Argument varName

        Frontend.Plus e1 e2 ->
            map2 Canonical.Plus
                (recurse e1)
                (recurse e2)

        Frontend.Cons e1 e2 ->
            map2 Canonical.Cons
                (recurse e1)
                (recurse e2)

        Frontend.ListConcat e1 e2 ->
            let
                region =
                    Located.getRegion located

                listConcatVar =
                    Frontend.Var
                        { module_ = Just "List"
                        , name = "append"
                        }
                        |> Located.located region

                firstCall =
                    Frontend.Call { fn = listConcatVar, argument = e1 } |> Located.located region

                expr =
                    Frontend.Call { fn = firstCall, argument = e2 } |> Located.located region
            in
            recurse expr

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
                                |> Dict.fromList
                        , body = body_
                        }
                )
                -- TODO a bit mouthful:
                (Result.combine (List.map (Binding.map recurse >> Binding.combine) bindings))
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
curryLambda :
    Frontend.LocatedExpr
    -> List VarName
    -> Canonical.LocatedExpr
    -> Canonical.LocatedExpr
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
findModuleOfVar :
    Modules Frontend.LocatedExpr
    -> Module Frontend.LocatedExpr
    -> { module_ : Maybe ModuleName, name : VarName }
    -> Maybe ModuleName
findModuleOfVar modules thisModule var =
    {- TODO does this allow for some collisions by "returning early"?
       Should we check that exactly one is Just and the others are Nothing?
    -}
    unqualifiedVarInThisModule thisModule var
        |> Maybe.Extra.orElseLazy (\() -> unqualifiedVarInImportedModule modules thisModule var)
        |> Maybe.Extra.orElseLazy (\() -> qualifiedVarInImportedModule modules var)
        |> Maybe.Extra.orElseLazy (\() -> qualifiedVarInAliasedModule modules thisModule var)


unqualifiedVarInThisModule :
    Module Frontend.LocatedExpr
    -> { module_ : Maybe ModuleName, name : VarName }
    -> Maybe ModuleName
unqualifiedVarInThisModule thisModule { module_, name } =
    if module_ == Nothing && Dict.member name thisModule.declarations then
        Just thisModule.name

    else
        Nothing


unqualifiedVarInImportedModule :
    Modules Frontend.LocatedExpr
    -> Module Frontend.LocatedExpr
    -> { module_ : Maybe ModuleName, name : VarName }
    -> Maybe ModuleName
unqualifiedVarInImportedModule modules thisModule { module_, name } =
    if module_ == Nothing then
        -- find a module which exposes that var
        thisModule.imports
            |> Dict.find
                (\_ import_ ->
                    Dict.get import_.moduleName modules
                        |> Maybe.map (Module.exposes name)
                        |> Maybe.withDefault False
                )
            |> Maybe.map (\( _, import_ ) -> import_.moduleName)

    else
        Nothing


qualifiedVarInImportedModule :
    Modules Frontend.LocatedExpr
    -> { module_ : Maybe ModuleName, name : VarName }
    -> Maybe ModuleName
qualifiedVarInImportedModule modules { module_, name } =
    module_
        |> Maybe.andThen (flip Dict.get modules)
        |> Maybe.andThen
            (\module__ ->
                if Dict.member name module__.declarations then
                    Just module_

                else
                    Nothing
            )
        |> Maybe.withDefault Nothing


qualifiedVarInAliasedModule :
    Modules Frontend.LocatedExpr
    -> Module Frontend.LocatedExpr
    -> { module_ : Maybe ModuleName, name : VarName }
    -> Maybe ModuleName
qualifiedVarInAliasedModule modules thisModule { module_, name } =
    let
        unaliasedModuleName =
            Maybe.andThen (Module.unalias thisModule) module_
    in
    -- Reusing the existing functionality. TODO is this a good idea?
    qualifiedVarInImportedModule modules { module_ = unaliasedModuleName, name = name }
