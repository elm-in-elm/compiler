module Stage.Desugar exposing (desugar, desugarExpr)

import Basics.Extra exposing (flip)
import Dict exposing (Dict)
import Dict.Extra as Dict
import Elm.AST.Canonical as Canonical
import Elm.AST.Frontend as Frontend
import Elm.Compiler.Error exposing (DesugarError(..), Error(..))
import Elm.Data.Binding as Binding
import Elm.Data.Declaration exposing (Declaration)
import Elm.Data.Located as Located
import Elm.Data.Module as Module exposing (Module)
import Elm.Data.ModuleName exposing (ModuleName)
import Elm.Data.Project exposing (Project)
import Elm.Data.Type exposing (Type)
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
            desugarTypeAnnotation
        |> Result.mapError DesugarError


{-| Combines the various small desugaring passes into one pass that tries all
of them. Thus, we get some separation of concerns - each pass only cares about
a small subset of the whole process!
-}
desugarExpr :
    Dict ModuleName (Module Frontend.LocatedExpr TypeAnnotation)
    -> Module Frontend.LocatedExpr TypeAnnotation
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
desugarTypeAnnotation : Declaration a TypeAnnotation -> Result DesugarError (Declaration a Type)
desugarTypeAnnotation decl =
    decl.typeAnnotation
        |> Maybe.map
            (\{ varName, type_ } ->
                if varName == decl.name then
                    Ok
                        { module_ = decl.module_
                        , typeAnnotation = Just type_
                        , name = decl.name
                        , body = decl.body
                        }

                else
                    Err <|
                        VarNameAndTypeAnnotationDontMatch
                            { typeAnnotation = varName
                            , varName = decl.name
                            }
            )
        |> Maybe.withDefault
            (Ok
                { module_ = decl.module_
                , typeAnnotation = Maybe.map .type_ decl.typeAnnotation
                , name = decl.name
                , body = decl.body
                }
            )



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
    Dict ModuleName (Module Frontend.LocatedExpr TypeAnnotation)
    -> Module Frontend.LocatedExpr TypeAnnotation
    -> { module_ : Maybe ModuleName, name : VarName }
    -> Result DesugarError ModuleName
findModuleOfVar modules thisModule var =
    unqualifiedVarInThisModule thisModule var
        |> Maybe.Extra.orElseLazy (\() -> unqualifiedVarInImportedModule modules thisModule var)
        |> Maybe.Extra.orElseLazy (\() -> qualifiedVarInImportedModule modules thisModule var)
        |> Maybe.Extra.orElseLazy (\() -> qualifiedVarInAliasedModule modules thisModule var)
        |> Result.fromMaybe (VarNameNotFound { var = var, insideModule = thisModule.name })
        |> Result.andThen identity


unqualifiedVarInThisModule :
    Module Frontend.LocatedExpr TypeAnnotation
    -> { module_ : Maybe ModuleName, name : VarName }
    -> Maybe (Result DesugarError ModuleName)
unqualifiedVarInThisModule thisModule { module_, name } =
    if module_ == Nothing && Dict.member name thisModule.declarations then
        Just (Ok thisModule.name)

    else
        Nothing


unqualifiedVarInImportedModule :
    Dict ModuleName (Module Frontend.LocatedExpr TypeAnnotation)
    -> Module Frontend.LocatedExpr TypeAnnotation
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
    Dict ModuleName (Module Frontend.LocatedExpr TypeAnnotation)
    -> Module Frontend.LocatedExpr TypeAnnotation
    -> { module_ : Maybe ModuleName, name : VarName }
    -> Maybe (Result DesugarError ModuleName)
qualifiedVarInImportedModule modules thisModule { module_, name } =
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
    Dict ModuleName (Module Frontend.LocatedExpr TypeAnnotation)
    -> Module Frontend.LocatedExpr TypeAnnotation
    -> { module_ : Maybe ModuleName, name : VarName }
    -> Maybe (Result DesugarError ModuleName)
qualifiedVarInAliasedModule modules thisModule { module_, name } =
    let
        unaliasedModuleName =
            Maybe.andThen (Module.unalias thisModule) module_
    in
    qualifiedVarInImportedModule
        modules
        thisModule
        { module_ = unaliasedModuleName, name = name }
