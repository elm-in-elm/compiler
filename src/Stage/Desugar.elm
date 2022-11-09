module Stage.Desugar exposing
    ( desugar
    , desugarExpr
    , desugarQualifiedness
    , desugarTypeAnnotation
    )

import Dict exposing (Dict)
import Elm.AST.Canonical as Canonical
import Elm.AST.Frontend as Frontend
import Elm.Compiler.Error
    exposing
        ( DesugarError(..)
        , Error(..)
        )
import Elm.Data.Binding as Binding exposing (Binding)
import Elm.Data.Declaration as Declaration
    exposing
        ( Declaration
        , DeclarationBody(..)
        )
import Elm.Data.Located as Located
import Elm.Data.Module as Module exposing (Module)
import Elm.Data.ModuleName exposing (ModuleName)
import Elm.Data.Project exposing (Project)
import Elm.Data.Qualifiedness exposing (PossiblyQualified(..), Qualified(..))
import Elm.Data.Type.Concrete exposing (ConcreteType(..))
import Elm.Data.TypeAnnotation exposing (TypeAnnotation)
import Elm.Data.VarName exposing (VarName)
import List.NonEmpty exposing (NonEmpty)
import OurExtras.Result as Result
import Result.Extra as Result
import Stage.Desugar.Boilerplate as Boilerplate


desugar : Project Frontend.ProjectFields -> Result Error (Project Canonical.ProjectFields)
desugar project =
    project
        |> Boilerplate.desugarProject
            (desugarExpr project.modules)
            (desugarQualifiedness project.modules)
            (desugarTypeAnnotation project.modules)
        |> Result.mapError DesugarError


{-| Combines the various small desugaring passes into one pass that tries all
of them. Thus, we get some separation of concerns - each pass only cares about
a small subset of the whole process!
-}
desugarExpr :
    Dict ModuleName (Module Frontend.LocatedExpr TypeAnnotation PossiblyQualified)
    -> Module Frontend.LocatedExpr TypeAnnotation PossiblyQualified
    -> Frontend.LocatedExpr
    -> Result DesugarError Canonical.LocatedExpr
desugarExpr modules thisModule locatedExpr =
    let
        f =
            desugarExpr modules thisModule

        return expr =
            Ok (Located.replaceWith expr locatedExpr)

        map fn =
            Result.map
                (\expr ->
                    Located.replaceWith
                        (fn expr)
                        locatedExpr
                )

        map2 fn =
            Result.map2
                (\expr1 expr2 ->
                    Located.replaceWith
                        (fn expr1 expr2)
                        locatedExpr
                )

        map3 fn =
            Result.map3
                (\expr1 expr2 expr3 ->
                    Located.replaceWith
                        (fn expr1 expr2 expr3)
                        locatedExpr
                )
    in
    case Located.unwrap locatedExpr of
        Frontend.Int int ->
            return <| Canonical.Int int

        Frontend.HexInt int ->
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
                |> map
                    (\moduleName ->
                        Canonical.Var
                            { module_ = moduleName
                            , name = var.name
                            }
                    )

        Frontend.Argument varName ->
            return <| Canonical.Argument varName

        Frontend.BinOp op e1 e2 ->
            map2 (Canonical.BinOp op)
                (f e1)
                (f e2)

        Frontend.Lambda { arguments, body } ->
            f body
                |> Result.map (curryLambda locatedExpr arguments)

        Frontend.Call { fn, argument } ->
            map2
                (\fn_ argument_ ->
                    Canonical.Call
                        { fn = fn_
                        , argument = argument_
                        }
                )
                (f fn)
                (f argument)

        Frontend.If { test, then_, else_ } ->
            map3
                (\test_ then__ else__ ->
                    Canonical.If
                        { test = test_
                        , then_ = then__
                        , else_ = else__
                        }
                )
                (f test)
                (f then_)
                (f else_)

        Frontend.Let { bindings, body } ->
            map2
                (\bindings_ body_ ->
                    Canonical.Let
                        { bindings =
                            bindings_
                                |> List.NonEmpty.toList
                                |> List.map (\binding -> ( binding.name, binding ))
                                |> Dict.fromList
                        , body = body_
                        }
                )
                -- TODO a bit mouthful:
                (Result.combineNonEmpty (List.NonEmpty.map (Binding.map f >> Binding.combine) bindings))
                (f body)

        Frontend.List items ->
            List.map f items
                |> Result.combine
                |> map Canonical.List

        Frontend.Tuple e1 e2 ->
            map2 Canonical.Tuple
                (f e1)
                (f e2)

        Frontend.Tuple3 e1 e2 e3 ->
            map3 Canonical.Tuple3
                (f e1)
                (f e2)
                (f e3)

        Frontend.Unit ->
            return Canonical.Unit

        Frontend.Record bindings ->
            case maybeDuplicateBindingsError thisModule.name bindings of
                Just error ->
                    Err error

                Nothing ->
                    bindings
                        |> List.map (Binding.map f >> Binding.combine)
                        |> Result.combine
                        |> map
                            (\canonicalBindings ->
                                canonicalBindings
                                    |> List.map (\canonicalBinding -> ( canonicalBinding.name, canonicalBinding ))
                                    |> Dict.fromList
                                    |> Canonical.Record
                            )

        Frontend.RecordAccess e field ->
            f e
                |> Result.map
                    (\e_ ->
                        Located.replaceWith
                            (Canonical.RecordAccess e_ field)
                            locatedExpr
                    )

        Frontend.Case test branches ->
            Result.map2
                (\expr branches_ ->
                    Located.replaceWith
                        (Canonical.Case expr branches_)
                        locatedExpr
                )
                (f test)
                (branches
                    |> List.NonEmpty.map
                        (\{ pattern, body } ->
                            Result.map2
                                (\p b ->
                                    { pattern = p
                                    , body = b
                                    }
                                )
                                (desugarPattern pattern)
                                (f body)
                        )
                    |> Result.combineNonEmpty
                )

        Frontend.ConstructorValue rec ->
            Module.findModuleOfVar modules thisModule rec
                |> map
                    (\moduleName ->
                        Canonical.ConstructorValue
                            { module_ = moduleName
                            , name = rec.name
                            }
                    )


desugarPattern :
    Frontend.LocatedPattern
    -> Result DesugarError Canonical.LocatedPattern
desugarPattern located =
    let
        f =
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
                (\pattern1 pattern2 ->
                    Located.replaceWith
                        (fn pattern1 pattern2)
                        located
                )

        map3 fn =
            Result.map3
                (\pattern1 pattern2 pattern3 ->
                    Located.replaceWith
                        (fn pattern1 pattern2 pattern3)
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

        Frontend.PAlias pattern varName ->
            f pattern
                |> map (\p -> Canonical.PAlias p varName)

        Frontend.PUnit ->
            return <| Canonical.PUnit

        Frontend.PTuple pattern1 pattern2 ->
            map2 Canonical.PTuple
                (f pattern1)
                (f pattern2)

        Frontend.PTuple3 pattern1 pattern2 pattern3 ->
            map3 Canonical.PTuple3
                (f pattern1)
                (f pattern2)
                (f pattern3)

        Frontend.PList patterns ->
            List.map f patterns
                |> List.foldr (Result.map2 (::)) (Ok [])
                |> map Canonical.PList

        Frontend.PCons pattern1 pattern2 ->
            map2 Canonical.PCons
                (f pattern1)
                (f pattern2)

        Frontend.PChar char ->
            return <| Canonical.PChar char

        Frontend.PString string ->
            return <| Canonical.PString string

        Frontend.PInt int ->
            return <| Canonical.PInt int

        Frontend.PFloat float ->
            return <| Canonical.PFloat float


desugarQualifiedness :
    Dict ModuleName (Module Frontend.LocatedExpr TypeAnnotation PossiblyQualified)
    -> Module Frontend.LocatedExpr TypeAnnotation PossiblyQualified
    -> VarName
    -> PossiblyQualified
    -> Result DesugarError Qualified
desugarQualifiedness modules thisModule name qualifiedness =
    Module.findModuleOfVar
        modules
        thisModule
        { qualifiedness = qualifiedness
        , name = name
        }
        |> Result.map Qualified


{-| We only do stuff in the UserDefinedType case. The rest is boilerplate.
-}
desugarType :
    Dict ModuleName (Module Frontend.LocatedExpr TypeAnnotation PossiblyQualified)
    -> Module Frontend.LocatedExpr TypeAnnotation PossiblyQualified
    -> ConcreteType PossiblyQualified
    -> Result DesugarError (ConcreteType Qualified)
desugarType modules thisModule type_ =
    let
        f =
            desugarType modules thisModule
    in
    case type_ of
        TypeVar s ->
            Ok <| TypeVar s

        Function { from, to } ->
            Result.map2
                (\from_ to_ ->
                    Function
                        { from = from_
                        , to = to_
                        }
                )
                (f from)
                (f to)

        Int ->
            Ok Int

        Float ->
            Ok Float

        Char ->
            Ok Char

        String ->
            Ok String

        Bool ->
            Ok Bool

        List listType ->
            f listType
                |> Result.map List

        Unit ->
            Ok Unit

        Tuple a b ->
            Result.map2 Tuple
                (f a)
                (f b)

        Tuple3 a b c ->
            Result.map3 Tuple3
                (f a)
                (f b)
                (f c)

        Record bindings ->
            bindings
                |> Dict.toList
                |> List.map {- pheeew... -} (Result.combineMapSecond f)
                |> Result.combine
                |> Result.map (Dict.fromList >> Record)

        UserDefinedType { qualifiedness, name, args } ->
            Result.map2
                (\foundModuleName desugaredArgs ->
                    UserDefinedType
                        { qualifiedness = Qualified foundModuleName
                        , name = name
                        , args = desugaredArgs
                        }
                )
                (Module.findModuleOfVar
                    modules
                    thisModule
                    { qualifiedness = qualifiedness
                    , name = name
                    }
                )
                (Result.combine (List.map f args))


{-| TODO test (make sure the integration tests properly fail/succeed and have snapshots)
-}
desugarTypeAnnotation :
    Dict ModuleName (Module Frontend.LocatedExpr TypeAnnotation PossiblyQualified)
    -> Module Frontend.LocatedExpr TypeAnnotation PossiblyQualified
    -> Declaration a TypeAnnotation b
    -> Result DesugarError (Declaration a (ConcreteType Qualified) b)
desugarTypeAnnotation modules thisModule decl =
    decl
        |> checkAndDropTypeAnnotationName
        |> Result.andThen (desugarTypeAnnotationQualifiedness modules thisModule)


desugarTypeAnnotationQualifiedness :
    Dict ModuleName (Module Frontend.LocatedExpr TypeAnnotation PossiblyQualified)
    -> Module Frontend.LocatedExpr TypeAnnotation PossiblyQualified
    -> Declaration a (ConcreteType PossiblyQualified) b
    -> Result DesugarError (Declaration a (ConcreteType Qualified) b)
desugarTypeAnnotationQualifiedness modules thisModule decl =
    let
        default =
            decl
                |> Declaration.setAnnotation Nothing
                |> Ok
    in
    case decl.body of
        Value r ->
            r.typeAnnotation
                |> Maybe.map
                    (\type_ ->
                        type_
                            |> desugarType modules thisModule
                            |> Result.map
                                (\desugaredType ->
                                    Declaration.setAnnotation
                                        (Just desugaredType)
                                        decl
                                )
                    )
                |> Maybe.withDefault default

        TypeAlias _ ->
            default

        CustomType _ ->
            default

        Port _ ->
            default


{-| Check the var name in the type annotation is the same as the one in the declaration:

     x : Int
     x = 123 -- "x" == "X"

If they don't match, throw an error:

     x : Int
     y = 123 -- "x" /= "y"

-}
checkAndDropTypeAnnotationName :
    Declaration a TypeAnnotation b
    -> Result DesugarError (Declaration a (ConcreteType PossiblyQualified) b)
checkAndDropTypeAnnotationName decl =
    case decl.body of
        Value r ->
            r.typeAnnotation
                |> Maybe.map
                    (\{ varName } ->
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

        TypeAlias r ->
            Ok
                { module_ = decl.module_
                , name = decl.name
                , body = TypeAlias r
                }

        CustomType r ->
            Ok
                { module_ = decl.module_
                , name = decl.name
                , body = CustomType r
                }

        Port type_ ->
            Ok
                { module_ = decl.module_
                , name = decl.name
                , body = Port type_
                }


throwAwayTypeAnnotationName :
    Declaration a TypeAnnotation b
    -> Declaration a (ConcreteType PossiblyQualified) b
throwAwayTypeAnnotationName decl =
    { module_ = decl.module_
    , name = decl.name
    , body =
        case decl.body of
            Value r ->
                Value
                    { expression = r.expression
                    , typeAnnotation = Maybe.map .type_ r.typeAnnotation
                    }

            TypeAlias r ->
                TypeAlias r

            CustomType r ->
                CustomType r

            Port type_ ->
                Port type_
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
    -> NonEmpty VarName
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
        (List.NonEmpty.toList arguments)
