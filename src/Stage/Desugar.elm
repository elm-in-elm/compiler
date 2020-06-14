module Stage.Desugar exposing
    ( checkAndDesugarTypeAnnotation
    , desugar
    , desugarExpr
    )

import Basics.Extra exposing (flip)
import Dict exposing (Dict)
import Dict.Extra as Dict
import Elm.AST.Canonical as Canonical
import Elm.AST.Frontend as Frontend
import Elm.Compiler.Error exposing (DesugarError(..), Error(..))
import Elm.Data.Binding as Binding exposing (Binding)
import Elm.Data.Declaration exposing (Declaration)
import Elm.Data.Located as Located
import Elm.Data.Module as Module exposing (Module)
import Elm.Data.ModuleName exposing (ModuleName)
import Elm.Data.Project exposing (Project)
import Elm.Data.Qualifiedness exposing (PossiblyQualified, Qualified)
import Elm.Data.Type exposing (Type(..), TypeOrId(..))
import Elm.Data.TypeAnnotation exposing (TypeAnnotation)
import Elm.Data.VarName exposing (VarName)
import Maybe.Extra
import Result.Extra as Result
import Stage.Desugar.Boilerplate as Boilerplate


desugar : Project Frontend.ProjectFields -> Result Error (Project Canonical.ProjectFields)
desugar project =
    project
        |> Boilerplate.desugarProject
            (desugarExpr project.modules)
            desugarQualifiedness
            checkAndDesugarTypeAnnotation
        |> Result.mapError DesugarError


{-| Combines the various small desugaring passes into one pass that tries all
of them. Thus, we get some separation of concerns - each pass only cares about
a small subset of the whole process!
-}
desugarExpr :
    Dict ModuleName (Module Frontend.LocatedExpr TypeAnnotation (Maybe String))
    -> Module Frontend.LocatedExpr TypeAnnotation (Maybe String)
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
            Module.findModuleOfVar modules thisModule var
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


desugarQualifiedness : a
desugarQualifiedness =
    Debug.todo "desugarQualifiedness"


{-| TODO this should be used somewhere in the checkAndDesugarTypeAnnotation
function?

TODO or is this even needed? This was `desugarType` before we figured out the
declaration needs to change the qualifiedness.

We only do stuff in the UserDefinedType case. The rest is boilerplate.

-}
todoDesugarType :
    Dict ModuleName (Module Frontend.LocatedExpr TypeAnnotation (Maybe String))
    -> Module Frontend.LocatedExpr TypeAnnotation (Maybe String)
    -> TypeOrId PossiblyQualified
    -> Result DesugarError (TypeOrId Qualified)
todoDesugarType modules thisModule typeOrId =
    let
        f =
            todoDesugarType modules thisModule
    in
    case typeOrId of
        Id id ->
            Ok <| Id id

        Type type_ ->
            case type_ of
                Var s ->
                    Ok <| Type <| Var s

                Function { from, to } ->
                    Result.map2
                        (\from_ to_ -> Type <| Function { from = from_, to = to_ })
                        (f from)
                        (f to)

                Int ->
                    Ok <| Type Int

                Float ->
                    Ok <| Type Float

                Char ->
                    Ok <| Type Char

                String ->
                    Ok <| Type String

                Bool ->
                    Ok <| Type Bool

                List listType ->
                    Result.map (Type << List)
                        (f listType)

                Unit ->
                    Ok <| Type Unit

                Tuple a b ->
                    Result.map2 (\a_ b_ -> Type <| Tuple a_ b_)
                        (f a)
                        (f b)

                Tuple3 a b c ->
                    Result.map3 (\a_ b_ c_ -> Type <| Tuple3 a_ b_ c_)
                        (f a)
                        (f b)
                        (f c)

                Record bindings ->
                    bindings
                        |> Dict.toList
                        |> List.map {- pheeew... -} (Result.combineMapSecond f)
                        |> Result.combine
                        |> Result.map (Dict.fromList >> Record >> Type)

                UserDefinedType { module_, name, args } ->
                    Result.map2
                        (\foundModuleName desugaredArgs ->
                            Type <|
                                UserDefinedType
                                    { module_ = foundModuleName
                                    , name = name
                                    , args = desugaredArgs
                                    }
                        )
                        (Module.findModuleOfVar
                            modules
                            thisModule
                            { module_ = module_
                            , name = name
                            }
                        )
                        (Result.combine (List.map f args))


{-| Check the var name in the type annotation is the same as the one in the declaration:

    x : Int
    x =
        123

If they don't match, throw an error:

     x : Int
     y =
         123

TODO test

-}
checkAndDesugarTypeAnnotation :
    Declaration a TypeAnnotation (Maybe String)
    -> Result DesugarError (Declaration a (Type Qualified) (Maybe String))
checkAndDesugarTypeAnnotation decl =
    start here
        -- TODO find the modules of user types here too
        -- TODO this can be done using todoDesugarType
        decl.typeAnnotation
        |> Maybe.map
            (\{ varName, type_ } ->
                if varName == decl.name then
                    Ok <| throwAwayTypeAnnotationName decl

                else
                    Err <|
                        VarNameAndTypeAnnotationDontMatch
                            { typeAnnotation = varName
                            , varName = decl.name
                            }
            )
        |> Maybe.withDefault (Ok <| throwAwayTypeAnnotationName decl)


throwAwayTypeAnnotationName : Declaration a TypeAnnotation (Maybe String) -> Declaration a TypeUnq (Maybe String)
throwAwayTypeAnnotationName decl =
    { module_ = decl.module_
    , typeAnnotation = Maybe.map .type_ decl.typeAnnotation
    , name = decl.name
    , body = decl.body
    }



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
