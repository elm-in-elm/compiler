module Elm.AST.Typed exposing
    ( ProjectFields
    , LocatedExpr, Expr, Expr_(..)
    , LocatedPattern, Pattern, Pattern_(..)
    , getExpr, unwrap, transformAll, transformOnce, recursiveChildren, setExpr
    , dropTypes, getTypeOrId, getType
    )

{-| Typed AST holds the inferred [types](Elm.Data.Type) for every expression.

@docs ProjectFields
@docs LocatedExpr, Expr, Expr_
@docs LocatedPattern, Pattern, Pattern_
@docs getExpr, unwrap, transformAll, transformOnce, recursiveChildren, setExpr
@docs dropTypes, getTypeOrId, getType

-}

import Dict exposing (Dict)
import Elm.AST.Canonical as Canonical
import Elm.AST.Typed.Unwrapped as Unwrapped
import Elm.Data.Binding as Binding exposing (Binding)
import Elm.Data.Located as Located exposing (Located)
import Elm.Data.Module exposing (Module)
import Elm.Data.ModuleName exposing (ModuleName)
import Elm.Data.Qualifiedness exposing (Qualified)
import Elm.Data.Type as Type exposing (Type, TypeOrId)
import Elm.Data.VarName exposing (VarName)
import List.NonEmpty exposing (NonEmpty)
import OurExtras.List as List
import Transform


{-| "What does this compiler stage need to store about the whole project?

(See [`Elm.Data.Project`](Elm.Data.Project).)

In this case, a dict of all the compiled Elm [modules](Elm.Data.Module#Module)
that hold [typed AST expressions](#LocatedExpr).

-}
type alias ProjectFields =
    { modules : Dict ModuleName (Module LocatedExpr Never Qualified) }


{-| The main type of this module. Expression with [location metadata](Elm.Data.Located).

Note the underlying [`Expr`](#Expr) custom type recurses on this [`LocatedExpr`](#LocatedExpr) type,
so that the children also each have their location metadata.

If you want expressions without location metadata, look at [`unwrap`](#unwrap).

-}
type alias LocatedExpr =
    Located Expr


{-| Differs from [Canonical.Expr](Elm.AST.Canonical#Expr) by:

  - being a tuple of the underlying Expr\_ and its inferred type

-}
type alias Expr =
    ( Expr_, TypeOrId Qualified )


{-| -}
type Expr_
    = Int Int
    | Float Float
    | Char Char
    | String String
    | Bool Bool
    | Var { module_ : ModuleName, name : VarName }
    | ConstructorValue { module_ : ModuleName, name : VarName }
    | Argument VarName
    | BinOp String LocatedExpr LocatedExpr
    | Lambda { argument : VarName, body : LocatedExpr }
    | Call { fn : LocatedExpr, argument : LocatedExpr }
    | If { test : LocatedExpr, then_ : LocatedExpr, else_ : LocatedExpr }
    | Let { bindings : Dict VarName (Binding LocatedExpr), body : LocatedExpr }
    | List (List LocatedExpr)
    | Unit
    | Tuple LocatedExpr LocatedExpr
    | Tuple3 LocatedExpr LocatedExpr LocatedExpr
    | Record (Dict VarName (Binding LocatedExpr))
    | RecordAccess LocatedExpr String
    | Case LocatedExpr (NonEmpty { pattern : LocatedPattern, body : LocatedExpr })


type alias LocatedPattern =
    Located Pattern


{-| Differs from [Canonical.Pattern](Elm.AST.Canonical#Pattern) by:

  - being a tuple of the underlying Pattern\_ and its inferred type

-}
type alias Pattern =
    ( Pattern_, TypeOrId Qualified )


type Pattern_
    = PAnything
    | PVar VarName
    | PRecord (List VarName)
    | PAlias LocatedPattern VarName
    | PUnit
    | PTuple LocatedPattern LocatedPattern
    | PTuple3 LocatedPattern LocatedPattern LocatedPattern
    | PList (List LocatedPattern)
    | PCons LocatedPattern LocatedPattern
    | PChar Char
    | PString String
    | PInt Int
    | PFloat Float


{-| A helper for the [Transform](/packages/Janiczek/transform/latest/) library.
Runs the given function recursively on all children.
-}
recurse : (LocatedExpr -> LocatedExpr) -> LocatedExpr -> LocatedExpr
recurse fn locatedExpr =
    locatedExpr
        |> mapExpr
            (\expr ->
                case expr of
                    Int _ ->
                        expr

                    Float _ ->
                        expr

                    Char _ ->
                        expr

                    String _ ->
                        expr

                    Bool _ ->
                        expr

                    Var _ ->
                        expr

                    Argument _ ->
                        expr

                    BinOp op e1 e2 ->
                        BinOp op
                            (fn e1)
                            (fn e2)

                    Lambda ({ body } as lambda_) ->
                        Lambda { lambda_ | body = fn body }

                    Call call ->
                        Call
                            { fn = fn call.fn
                            , argument = fn call.argument
                            }

                    If { test, then_, else_ } ->
                        If
                            { test = fn test
                            , then_ = fn then_
                            , else_ = fn else_
                            }

                    Let { bindings, body } ->
                        Let
                            { bindings =
                                Dict.map
                                    (always (Binding.map fn))
                                    bindings
                            , body = fn body
                            }

                    List items ->
                        List (List.map fn items)

                    Tuple e1 e2 ->
                        Tuple
                            (fn e1)
                            (fn e2)

                    Tuple3 e1 e2 e3 ->
                        Tuple3
                            (fn e1)
                            (fn e2)
                            (fn e3)

                    Unit ->
                        expr

                    Record bindings ->
                        Record
                            (Dict.map
                                (always (Binding.map fn))
                                bindings
                            )

                    RecordAccess e field ->
                        RecordAccess (fn e) field

                    Case test branches ->
                        Case (fn test) <|
                            List.NonEmpty.map
                                (\{ pattern, body } ->
                                    { pattern = pattern
                                    , body = fn body
                                    }
                                )
                                branches

                    ConstructorValue rec ->
                        ConstructorValue rec
            )


{-| Transform the expression once using the provided function.
Start at the children, apply once then go up.
-}
transformOnce : (LocatedExpr -> LocatedExpr) -> LocatedExpr -> LocatedExpr
transformOnce pass locatedExpr =
    Transform.transformOnce
        recurse
        pass
        locatedExpr


{-| Transform the expression using the provided function.
Start at the children, repeatedly apply on them until they stop changing,
then go up.
-}
transformAll : List (LocatedExpr -> Maybe LocatedExpr) -> LocatedExpr -> LocatedExpr
transformAll passes locatedExpr =
    Transform.transformAll
        recurse
        (Transform.orList passes)
        locatedExpr


{-| Find all the children of this expression (and their children, etc...)
-}
recursiveChildren : (LocatedExpr -> List LocatedExpr) -> LocatedExpr -> List LocatedExpr
recursiveChildren fn locatedExpr =
    case getExpr locatedExpr of
        Int _ ->
            []

        Float _ ->
            []

        Char _ ->
            []

        String _ ->
            []

        Bool _ ->
            []

        Var _ ->
            []

        Argument _ ->
            []

        BinOp _ left right ->
            fn left
                ++ fn right

        Lambda { body } ->
            fn body

        Call data ->
            fn data.fn
                ++ fn data.argument

        If { test, then_, else_ } ->
            fn test
                ++ fn then_
                ++ fn else_

        Let { bindings, body } ->
            fn body
                ++ List.fastConcatMap (.body >> fn) (Dict.values bindings)

        Unit ->
            []

        List items ->
            List.fastConcatMap fn items

        Tuple e1 e2 ->
            fn e1 ++ fn e2

        Tuple3 e1 e2 e3 ->
            fn e1 ++ fn e2 ++ fn e3

        Record bindings ->
            List.fastConcatMap (.body >> fn) (Dict.values bindings)

        RecordAccess e field ->
            fn e

        Case e branches ->
            fn e ++ List.fastConcatMap (.body >> fn) (List.NonEmpty.toList branches)

        ConstructorValue _ ->
            []


mapExpr : (Expr_ -> Expr_) -> LocatedExpr -> LocatedExpr
mapExpr fn locatedExpr =
    locatedExpr
        |> Located.map (Tuple.mapFirst fn)


{-| Replace the existing expression with the provided one.
-}
setExpr : Expr_ -> LocatedExpr -> LocatedExpr
setExpr expr locatedExpr =
    mapExpr (always expr) locatedExpr


{-| Extract the expression (remove the location and type information).
-}
getExpr : LocatedExpr -> Expr_
getExpr locatedExpr =
    locatedExpr
        |> Located.unwrap
        |> Tuple.first


{-| Extract the type (remove the location information and the expression).
-}
getTypeOrId : LocatedExpr -> TypeOrId Qualified
getTypeOrId locatedExpr =
    locatedExpr
        |> Located.unwrap
        |> Tuple.second


getType : LocatedExpr -> Maybe (Type Qualified)
getType locatedExpr =
    locatedExpr
        |> getTypeOrId
        |> Type.getType


{-| Discard the [location metadata](Elm.Data.Located#Located).
-}
unwrap : LocatedExpr -> Unwrapped.Expr
unwrap expr =
    let
        f =
            unwrap

        ( expr_, type_ ) =
            Located.unwrap expr
    in
    ( case expr_ of
        Int int ->
            Unwrapped.Int int

        Float float ->
            Unwrapped.Float float

        Char char ->
            Unwrapped.Char char

        String string ->
            Unwrapped.String string

        Bool bool ->
            Unwrapped.Bool bool

        Var var_ ->
            Unwrapped.Var var_

        Argument name ->
            Unwrapped.Argument name

        BinOp op e1 e2 ->
            Unwrapped.BinOp op
                (f e1)
                (f e2)

        Lambda { argument, body } ->
            Unwrapped.Lambda
                { argument = argument
                , body = f body
                }

        Call { fn, argument } ->
            Unwrapped.Call
                { fn = f fn
                , argument = f argument
                }

        If { test, then_, else_ } ->
            Unwrapped.If
                { test = f test
                , then_ = f then_
                , else_ = f else_
                }

        Let { bindings, body } ->
            Unwrapped.Let
                { bindings =
                    Dict.map
                        (always (Binding.map f))
                        bindings
                , body = f body
                }

        List list ->
            Unwrapped.List
                (List.map f list)

        Unit ->
            Unwrapped.Unit

        Tuple e1 e2 ->
            Unwrapped.Tuple
                (f e1)
                (f e2)

        Tuple3 e1 e2 e3 ->
            Unwrapped.Tuple3
                (f e1)
                (f e2)
                (f e3)

        Record bindings ->
            Unwrapped.Record <|
                Dict.map
                    (always (Binding.map unwrap))
                    bindings

        RecordAccess e field ->
            Unwrapped.RecordAccess (f e) field

        Case test branches ->
            Unwrapped.Case (f test) <|
                List.NonEmpty.map
                    (\{ pattern, body } ->
                        { pattern = unwrapPattern pattern
                        , body = f body
                        }
                    )
                    branches

        ConstructorValue rec ->
            Unwrapped.ConstructorValue rec
    , type_
    )


{-| Discard the [location metadata](Elm.Data.Located#Located).
-}
unwrapPattern : LocatedPattern -> Unwrapped.Pattern
unwrapPattern expr =
    let
        ( expr_, type_ ) =
            Located.unwrap expr
    in
    ( case expr_ of
        PAnything ->
            Unwrapped.PAnything

        PVar varName ->
            Unwrapped.PVar varName

        PRecord varNames ->
            Unwrapped.PRecord varNames

        PAlias p varName ->
            Unwrapped.PAlias (unwrapPattern p) varName

        PUnit ->
            Unwrapped.PUnit

        PTuple p1 p2 ->
            Unwrapped.PTuple (unwrapPattern p1) (unwrapPattern p2)

        PTuple3 p1 p2 p3 ->
            Unwrapped.PTuple3
                (unwrapPattern p1)
                (unwrapPattern p2)
                (unwrapPattern p3)

        PList ps ->
            Unwrapped.PList (List.map unwrapPattern ps)

        PCons p1 p2 ->
            Unwrapped.PCons (unwrapPattern p1) (unwrapPattern p2)

        PChar char ->
            Unwrapped.PChar char

        PString string ->
            Unwrapped.PString string

        PInt int ->
            Unwrapped.PInt int

        PFloat float ->
            Unwrapped.PFloat float
    , type_
    )


{-| Go from `AST.Typed` to [`AST.Canonical`](Elm.AST.Canonical).
-}
dropTypes : LocatedExpr -> Canonical.LocatedExpr
dropTypes locatedExpr =
    let
        f =
            dropTypes
    in
    locatedExpr
        |> Located.map
            (\( expr, _ ) ->
                case expr of
                    Int int ->
                        Canonical.Int int

                    Float float ->
                        Canonical.Float float

                    Char char ->
                        Canonical.Char char

                    String string ->
                        Canonical.String string

                    Bool bool ->
                        Canonical.Bool bool

                    Var var ->
                        Canonical.Var var

                    Argument var ->
                        Canonical.Argument var

                    BinOp op e1 e2 ->
                        Canonical.BinOp op
                            (f e1)
                            (f e2)

                    Lambda { argument, body } ->
                        Canonical.Lambda
                            { argument = argument
                            , body = f body
                            }

                    Call { fn, argument } ->
                        Canonical.Call
                            { fn = f fn
                            , argument = f argument
                            }

                    If { test, then_, else_ } ->
                        Canonical.If
                            { test = f test
                            , then_ = f then_
                            , else_ = f else_
                            }

                    Let { bindings, body } ->
                        Canonical.Let
                            { bindings = Dict.map (always (Binding.map f)) bindings
                            , body = f body
                            }

                    List exprs ->
                        Canonical.List (List.map f exprs)

                    Unit ->
                        Canonical.Unit

                    Tuple e1 e2 ->
                        Canonical.Tuple
                            (f e1)
                            (f e2)

                    Tuple3 e1 e2 e3 ->
                        Canonical.Tuple3
                            (f e1)
                            (f e2)
                            (f e3)

                    Record bindings ->
                        Canonical.Record <|
                            Dict.map (always (Binding.map f)) bindings

                    RecordAccess e field ->
                        Canonical.RecordAccess (f e) field

                    Case test branches ->
                        Canonical.Case (f test) <|
                            List.NonEmpty.map
                                (\{ pattern, body } ->
                                    { pattern = dropPatternTypes pattern
                                    , body = f body
                                    }
                                )
                                branches

                    ConstructorValue rec ->
                        Canonical.ConstructorValue rec
            )


{-| Go from `AST.Typed` to [`AST.Canonical`](Elm.AST.Canonical).
-}
dropPatternTypes : LocatedPattern -> Canonical.LocatedPattern
dropPatternTypes locatedPattern =
    let
        f =
            dropPatternTypes
    in
    locatedPattern
        |> Located.map
            (\( pattern, _ ) ->
                case pattern of
                    PAnything ->
                        Canonical.PAnything

                    PVar varName ->
                        Canonical.PVar varName

                    PRecord varNames ->
                        Canonical.PRecord varNames

                    PAlias p varName ->
                        Canonical.PAlias (f p) varName

                    PUnit ->
                        Canonical.PUnit

                    PTuple p1 p2 ->
                        Canonical.PTuple
                            (f p1)
                            (f p2)

                    PTuple3 p1 p2 p3 ->
                        Canonical.PTuple3
                            (f p1)
                            (f p2)
                            (f p3)

                    PList ps ->
                        Canonical.PList (List.map f ps)

                    PCons p1 p2 ->
                        Canonical.PCons (f p1) (f p2)

                    PChar char ->
                        Canonical.PChar char

                    PString string ->
                        Canonical.PString string

                    PInt int ->
                        Canonical.PInt int

                    PFloat float ->
                        Canonical.PFloat float
            )
