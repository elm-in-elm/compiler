module Stage.Desugar exposing (desugar, desugarExpr)

import Basics.Extra exposing (flip)
import Dict exposing (Dict)
import Dict.Extra as Dict
import Elm.AST.Canonical as Canonical
import Elm.AST.Frontend as Frontend
import Elm.Compiler.Error exposing (DesugarError(..), Error(..))
import Elm.Data.Binding as Binding exposing (Binding)
import Elm.Data.Located as Located
import Elm.Data.Module as Module exposing (Module)
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
    Dict ModuleName (Module Frontend.LocatedExpr)
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
        Frontend.Int int ->
            return <| Canonical.Int int

        Frontend.Float float ->
            return <| Canonical.Float float

        Frontend.Char char ->
            return <| Canonical.Char char

        Frontend.String string ->
            return <| Canonical.String string

        Frontend.Bool bool ->
            return <| Canonical.Bool bool

        Frontend.Var var ->
            findModuleOfVar modules thisModule var
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

        Frontend.Record bindings ->
            case maybeDuplicateBindingsError thisModule.name bindings of
                Just error ->
                    Err error

                Nothing ->
                    bindings
                        |> List.map (Binding.map recurse >> Binding.combine)
                        |> Result.combine
                        |> map
                            (\canonicalBindings ->
                                canonicalBindings
                                    |> List.map (\canonicalBinding -> ( canonicalBinding.name, canonicalBinding ))
                                    |> Dict.fromList
                                    |> Canonical.Record
                            )

        Frontend.Case test branches ->
            Result.map2
                (\expr branches_ ->
                    Located.replaceWith
                        (Canonical.Case expr branches_)
                        located
                )
                (recurse test)
                (List.map
                    (\{ pattern, body } ->
                        Result.map2
                            (\p b ->
                                { pattern = p
                                , body = b
                                }
                            )
                            (desugarPattern pattern)
                            (recurse body)
                    )
                    branches
                    |> List.foldr (Result.map2 (::)) (Ok [])
                )


desugarPattern :
    Frontend.LocatedPattern
    -> Result DesugarError Canonical.LocatedPattern
desugarPattern located =
    let
        recurse =
            desugarPattern

        return pattern =
            Ok (Located.replaceWith pattern located)

        map fn =
            Result.map
                (\pattern ->
                    Located.replaceWith
                        (fn pattern)
                        located
                )

        map2 fn =
            Result.map2
                (\pttrn1 pttrn2 ->
                    Located.replaceWith
                        (fn pttrn1 pttrn2)
                        located
                )

        map3 fn =
            Result.map3
                (\pttrn1 pttrn2 pttrn3 ->
                    Located.replaceWith
                        (fn pttrn1 pttrn2 pttrn3)
                        located
                )
    in
    case Located.unwrap located of
        Frontend.PAnything ->
            return <| Canonical.PAnything

        Frontend.PVar varName ->
            return <| Canonical.PVar varName

        Frontend.PRecord varNames ->
            return <| Canonical.PRecord varNames

        Frontend.PAlias pttrn varName ->
            recurse pttrn
                |> map (\p -> Canonical.PAlias p varName)

        Frontend.PUnit ->
            return <| Canonical.PUnit

        Frontend.PTuple pttrn1 pttrn2 ->
            map2 Canonical.PTuple
                (recurse pttrn1)
                (recurse pttrn2)

        Frontend.PTuple3 pttrn1 pttrn2 pttrn3 ->
            map3 Canonical.PTuple3
                (recurse pttrn1)
                (recurse pttrn2)
                (recurse pttrn3)

        Frontend.PList pttrns ->
            List.map recurse pttrns
                |> List.foldr (Result.map2 (::)) (Ok [])
                |> map Canonical.PList

        Frontend.PCons pttrn1 pttrn2 ->
            map2 Canonical.PCons
                (recurse pttrn1)
                (recurse pttrn2)

        Frontend.PBool bool ->
            return <| Canonical.PBool bool

        Frontend.PChar char ->
            return <| Canonical.PChar char

        Frontend.PString string ->
            return <| Canonical.PString string

        Frontend.PInt int ->
            return <| Canonical.PInt int

        Frontend.PFloat float ->
            return <| Canonical.PFloat float



-- HELPERS


{-| Ensure that there are no two bindings with the same name.

NOTE: The function will produce an error only for the _first_ duplicate pair.
Subsequent duplicate pairs are ignored.

-}
maybeDuplicateBindingsError : ModuleName -> List (Binding Frontend.LocatedExpr) -> Maybe DesugarError
maybeDuplicateBindingsError moduleName bindings =
    bindings
        |> findDuplicatesBy .name
        |> Maybe.map
            (\( first, second ) ->
                DuplicateRecordField
                    { name = first.name
                    , insideModule = moduleName
                    , firstOccurrence = Located.replaceWith () first.body
                    , secondOccurrence = Located.replaceWith () second.body
                    }
            )


{-| Find the first two elements in a list that duplicate a given property.

Benchmarks in benchmarks/findDuplicatesBy/

-}
findDuplicatesBy : (a -> comparable) -> List a -> Maybe ( a, a )
findDuplicatesBy property list =
    let
        recurse li =
            case li of
                a :: b :: tail ->
                    if property a == property b then
                        Just ( a, b )

                    else
                        recurse (b :: tail)

                _ ->
                    Nothing
    in
    list
        |> List.sortBy property
        |> recurse


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
                (Canonical.Lambda
                    { argument = argument
                    , body = body_
                    }
                )
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

There are two possible errors here:

  - VarNameNotFound: you used var name that can't be found
  - AmbiguousName: you used a name that is imported/defined more than once

TODO "module name not found" somewhere... maybe here, maybe parsing? dunno yet...

-}
findModuleOfVar :
    Dict ModuleName (Module Frontend.LocatedExpr)
    -> Module Frontend.LocatedExpr
    -> { module_ : Maybe ModuleName, name : VarName }
    -> Result DesugarError ModuleName
findModuleOfVar modules thisModule var =
    unqualifiedVarInThisModule thisModule var
        |> Maybe.Extra.orElseLazy (\() -> unqualifiedVarInImportedModule modules thisModule var)
        |> Maybe.Extra.orElseLazy (\() -> qualifiedVarInImportedModule modules var)
        |> Maybe.Extra.orElseLazy (\() -> qualifiedVarInAliasedModule modules thisModule var)
        |> Result.fromMaybe (VarNameNotFound { var = var, insideModule = thisModule.name })
        |> Result.andThen identity


unqualifiedVarInThisModule :
    Module Frontend.LocatedExpr
    -> { module_ : Maybe ModuleName, name : VarName }
    -> Maybe (Result DesugarError ModuleName)
unqualifiedVarInThisModule thisModule { module_, name } =
    if module_ == Nothing && Dict.member name thisModule.declarations then
        Just (Ok thisModule.name)

    else
        Nothing


unqualifiedVarInImportedModule :
    Dict ModuleName (Module Frontend.LocatedExpr)
    -> Module Frontend.LocatedExpr
    -> { module_ : Maybe ModuleName, name : VarName }
    -> Maybe (Result DesugarError ModuleName)
unqualifiedVarInImportedModule modules thisModule { module_, name } =
    if module_ == Nothing then
        -- find a module which exposes that var
        let
            acceptableImports =
                thisModule.imports
                    |> Dict.values
                    |> List.filter
                        (\import_ ->
                            Dict.get import_.moduleName modules
                                |> Maybe.map (Module.exposes name)
                                |> Maybe.withDefault False
                        )
        in
        case acceptableImports of
            [] ->
                Nothing

            [ acceptableImport ] ->
                Just (Ok acceptableImport.moduleName)

            _ ->
                Just
                    (Err
                        (AmbiguousName
                            { name = name
                            , insideModule = thisModule.name
                            , possibleModules = List.map .moduleName acceptableImports
                            }
                        )
                    )

    else
        Nothing


{-| We don't think about module `as` aliasing here.
-}
qualifiedVarInImportedModule :
    Dict ModuleName (Module Frontend.LocatedExpr)
    -> { module_ : Maybe ModuleName, name : VarName }
    -> Maybe (Result DesugarError ModuleName)
qualifiedVarInImportedModule modules { module_, name } =
    module_
        |> Maybe.andThen (flip Dict.get modules)
        |> Maybe.andThen
            (\module__ ->
                if Dict.member name module__.declarations then
                    Just (Ok module__.name)

                else
                    Nothing
            )


qualifiedVarInAliasedModule :
    Dict ModuleName (Module Frontend.LocatedExpr)
    -> Module Frontend.LocatedExpr
    -> { module_ : Maybe ModuleName, name : VarName }
    -> Maybe (Result DesugarError ModuleName)
qualifiedVarInAliasedModule modules thisModule { module_, name } =
    let
        unaliasedModuleName =
            Maybe.andThen (Module.unalias thisModule) module_
    in
    qualifiedVarInImportedModule
        modules
        { module_ = unaliasedModuleName, name = name }
