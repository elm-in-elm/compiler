module Elm.AST.Canonical exposing
    ( ProjectFields
    , LocatedExpr, Expr(..), unwrap, fromUnwrapped
    , LocatedPattern, Pattern(..), fromUnwrappedPattern, unwrapPattern
    )

{-| Canonical AST is stripped of useless information,
and generally tries to make life a bit more easier for compiler developers.

@docs ProjectFields
@docs LocatedExpr, Expr, unwrap, fromUnwrapped

-}

import Dict exposing (Dict)
import Elm.AST.Canonical.Unwrapped as Unwrapped
import Elm.Data.Binding as Binding exposing (Binding)
import Elm.Data.Located as Located exposing (Located)
import Elm.Data.Module exposing (Module)
import Elm.Data.ModuleName exposing (ModuleName)
import Elm.Data.VarName exposing (VarName)


{-| "What does this compiler stage need to store about the whole project?

(See [`Elm.Data.Project`](Elm.Data.Project).)

In this case, a dict of all the compiled Elm [modules](Elm.Data.Module#Module)
that hold [canonical AST expressions](#LocatedExpr).

-}
type alias ProjectFields =
    { modules : Dict ModuleName (Module LocatedExpr) }


{-| The main type of this module. Expression with [location metadata](Elm.Data.Located).

Note the underlying [`Expr`](#Expr) custom type recurses on this [`LocatedExpr`](#LocatedExpr) type,
so that the children also each have their location metadata.

If you want expressions without location metadata, look at [`unwrap`](#unwrap).

-}
type alias LocatedExpr =
    Located Expr


{-| Differs from [Frontend.Expr](Elm.AST.Frontend#Expr) by:

  - having fully qualified variables
  - having only single argument lambdas

-}
type Expr
    = Int Int
    | Float Float
    | Char Char
    | String String
    | Bool Bool
    | Var { module_ : ModuleName, name : VarName }
    | Argument VarName
    | Plus LocatedExpr LocatedExpr
    | Cons LocatedExpr LocatedExpr
    | Lambda { argument : VarName, body : LocatedExpr }
    | Call { fn : LocatedExpr, argument : LocatedExpr }
    | If { test : LocatedExpr, then_ : LocatedExpr, else_ : LocatedExpr }
    | Let { bindings : Dict VarName (Binding LocatedExpr), body : LocatedExpr }
    | List (List LocatedExpr)
    | Unit
    | Tuple LocatedExpr LocatedExpr
    | Tuple3 LocatedExpr LocatedExpr LocatedExpr
    | Record (Dict VarName (Binding LocatedExpr))
    | Case LocatedExpr (List { pattern : LocatedPattern, body : LocatedExpr })


type alias LocatedPattern =
    Located Pattern


{-| Exactly equal to [Frontend.Pattern](Elm.AST.Frontend#Pattern).
-}
type Pattern
    = PAnything
    | PVar VarName
    | PRecord (List VarName)
    | PAlias LocatedPattern VarName
    | PUnit
    | PTuple LocatedPattern LocatedPattern
    | PTuple3 LocatedPattern LocatedPattern LocatedPattern
    | PList (List LocatedPattern)
    | PCons LocatedPattern LocatedPattern
    | PBool Bool
    | PChar Char
    | PString String
    | PInt Int
    | PFloat Float


{-| Discard the [location metadata](Elm.Data.Located#Located).
-}
unwrap : LocatedExpr -> Unwrapped.Expr
unwrap expr =
    case Located.unwrap expr of
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

        Plus e1 e2 ->
            Unwrapped.Plus
                (unwrap e1)
                (unwrap e2)

        Cons e1 e2 ->
            Unwrapped.Cons
                (unwrap e1)
                (unwrap e2)

        Lambda { argument, body } ->
            Unwrapped.Lambda
                { argument = argument
                , body = unwrap body
                }

        Call { fn, argument } ->
            Unwrapped.Call
                { fn = unwrap fn
                , argument = unwrap argument
                }

        If { test, then_, else_ } ->
            Unwrapped.If
                { test = unwrap test
                , then_ = unwrap then_
                , else_ = unwrap else_
                }

        Let { bindings, body } ->
            Unwrapped.Let
                { bindings =
                    Dict.map
                        (always (Binding.map unwrap))
                        bindings
                , body = unwrap body
                }

        List list ->
            Unwrapped.List
                (List.map unwrap list)

        Unit ->
            Unwrapped.Unit

        Tuple e1 e2 ->
            Unwrapped.Tuple
                (unwrap e1)
                (unwrap e2)

        Tuple3 e1 e2 e3 ->
            Unwrapped.Tuple3
                (unwrap e1)
                (unwrap e2)
                (unwrap e3)

        Record bindings ->
            Unwrapped.Record <|
                Dict.map
                    (always (Binding.map unwrap))
                    bindings

        Case e branches ->
            Unwrapped.Case (unwrap e) <|
                List.map
                    (\{ pattern, body } ->
                        { pattern = unwrapPattern pattern
                        , body = unwrap body
                        }
                    )
                    branches


{-| Discard the [location metadata](Elm.Data.Located#Located).
-}
unwrapPattern : LocatedPattern -> Unwrapped.Pattern
unwrapPattern expr =
    case Located.unwrap expr of
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

        PBool bool ->
            Unwrapped.PBool bool

        PChar char ->
            Unwrapped.PChar char

        PString string ->
            Unwrapped.PString string

        PInt int ->
            Unwrapped.PInt int

        PFloat float ->
            Unwrapped.PFloat float


{-| Adds [**dummy** locations](Elm.Data.Located#dummyRegion) to the [Unwrapped.Expr](Elm.AST.Canonical.Unwrapped#Expr).
-}
fromUnwrapped : Unwrapped.Expr -> LocatedExpr
fromUnwrapped expr =
    Located.located Located.dummyRegion <|
        case expr of
            Unwrapped.Int int ->
                Int int

            Unwrapped.Float float ->
                Float float

            Unwrapped.Char char ->
                Char char

            Unwrapped.String string ->
                String string

            Unwrapped.Bool bool ->
                Bool bool

            Unwrapped.Var var_ ->
                Var var_

            Unwrapped.Argument name ->
                Argument name

            Unwrapped.Plus e1 e2 ->
                Plus
                    (fromUnwrapped e1)
                    (fromUnwrapped e2)

            Unwrapped.Cons e1 e2 ->
                Cons
                    (fromUnwrapped e1)
                    (fromUnwrapped e2)

            Unwrapped.Lambda { argument, body } ->
                Lambda
                    { argument = argument
                    , body = fromUnwrapped body
                    }

            Unwrapped.Call { fn, argument } ->
                Call
                    { fn = fromUnwrapped fn
                    , argument = fromUnwrapped argument
                    }

            Unwrapped.If { test, then_, else_ } ->
                If
                    { test = fromUnwrapped test
                    , then_ = fromUnwrapped then_
                    , else_ = fromUnwrapped else_
                    }

            Unwrapped.Let { bindings, body } ->
                Let
                    { bindings =
                        Dict.map
                            (always (Binding.map fromUnwrapped))
                            bindings
                    , body = fromUnwrapped body
                    }

            Unwrapped.List list ->
                List
                    (List.map fromUnwrapped list)

            Unwrapped.Unit ->
                Unit

            Unwrapped.Tuple e1 e2 ->
                Tuple
                    (fromUnwrapped e1)
                    (fromUnwrapped e2)

            Unwrapped.Tuple3 e1 e2 e3 ->
                Tuple3
                    (fromUnwrapped e1)
                    (fromUnwrapped e2)
                    (fromUnwrapped e3)

            Unwrapped.Record bindings ->
                Record <|
                    Dict.map
                        (always (Binding.map fromUnwrapped))
                        bindings

            Unwrapped.Case e branches ->
                Case (fromUnwrapped e) <|
                    List.map
                        (\{ pattern, body } ->
                            { pattern = fromUnwrappedPattern pattern
                            , body = fromUnwrapped body
                            }
                        )
                        branches


{-| Adds [**dummy** locations](Elm.Data.Located#dummyRegion) to the [Unwrapped.Pattern](Elm.AST.Canonical.Unwrapped#Pattern).
-}
fromUnwrappedPattern : Unwrapped.Pattern -> LocatedPattern
fromUnwrappedPattern pattern =
    Located.located Located.dummyRegion <|
        case pattern of
            Unwrapped.PAnything ->
                PAnything

            Unwrapped.PVar varName ->
                PVar varName

            Unwrapped.PRecord varNames ->
                PRecord varNames

            Unwrapped.PAlias p varName ->
                PAlias (fromUnwrappedPattern p) varName

            Unwrapped.PUnit ->
                PUnit

            Unwrapped.PTuple p1 p2 ->
                PTuple (fromUnwrappedPattern p1) (fromUnwrappedPattern p2)

            Unwrapped.PTuple3 p1 p2 p3 ->
                PTuple3
                    (fromUnwrappedPattern p1)
                    (fromUnwrappedPattern p2)
                    (fromUnwrappedPattern p3)

            Unwrapped.PList ps ->
                PList (List.map fromUnwrappedPattern ps)

            Unwrapped.PCons p1 p2 ->
                PCons (fromUnwrappedPattern p1) (fromUnwrappedPattern p2)

            Unwrapped.PBool bool ->
                PBool bool

            Unwrapped.PChar char ->
                PChar char

            Unwrapped.PString string ->
                PString string

            Unwrapped.PInt int ->
                PInt int

            Unwrapped.PFloat float ->
                PFloat float
