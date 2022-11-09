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
import Elm.Data.Qualifiedness exposing (Qualified)
import Elm.Data.Type.Concrete exposing (ConcreteType)
import Elm.Data.VarName exposing (VarName)
import List.NonEmpty exposing (NonEmpty)


{-| "What does this compiler stage need to store about the whole project?

(See [`Elm.Data.Project`](Elm.Data.Project).)

In this case, a dict of all the compiled Elm [modules](Elm.Data.Module#Module)
that hold [canonical AST expressions](#LocatedExpr).

-}
type alias ProjectFields =
    { modules : Dict ModuleName (Module LocatedExpr (ConcreteType Qualified) Qualified) }


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

We'd love to simplify the `let..in` expressions to only contain one binding
instead of a list or a dict, but to do that we would need to sort them in the
topological order. To do _that_ (see `Stage.Emit.findDependencies`), we need the
vars to be all qualified, to be able to follow them to their definitions.

And, well, the qualification process happens in the desugaring phase - right
when we'd like to "curry" the `let..in` expressions.

So, perhaps we could do this by splitting the Desugar phase into two (qualify
the vars and then curry the lets). For now, we're not doing that and instead we
bite the bullet and keep the bindings together.

-}
type Expr
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
    | PChar Char
    | PString String
    | PInt Int
    | PFloat Float


{-| Discard the [location metadata](Elm.Data.Located#Located).
-}
unwrap : LocatedExpr -> Unwrapped.Expr
unwrap expr =
    let
        f =
            unwrap
    in
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
            Unwrapped.RecordAccess (unwrap e) field

        Case e branches ->
            Unwrapped.Case (f e) <|
                List.NonEmpty.map
                    (\{ pattern, body } ->
                        { pattern = unwrapPattern pattern
                        , body = f body
                        }
                    )
                    branches

        ConstructorValue rec ->
            Unwrapped.ConstructorValue
                { module_ = rec.module_
                , name = rec.name
                }


{-| Discard the [location metadata](Elm.Data.Located#Located).
-}
unwrapPattern : LocatedPattern -> Unwrapped.Pattern
unwrapPattern expr =
    let
        f =
            unwrapPattern
    in
    case Located.unwrap expr of
        PAnything ->
            Unwrapped.PAnything

        PVar varName ->
            Unwrapped.PVar varName

        PRecord varNames ->
            Unwrapped.PRecord varNames

        PAlias p varName ->
            Unwrapped.PAlias (f p) varName

        PUnit ->
            Unwrapped.PUnit

        PTuple p1 p2 ->
            Unwrapped.PTuple (f p1) (f p2)

        PTuple3 p1 p2 p3 ->
            Unwrapped.PTuple3
                (f p1)
                (f p2)
                (f p3)

        PList ps ->
            Unwrapped.PList (List.map f ps)

        PCons p1 p2 ->
            Unwrapped.PCons (f p1) (f p2)

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
    let
        f =
            fromUnwrapped
    in
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

            Unwrapped.BinOp op e1 e2 ->
                BinOp op
                    (f e1)
                    (f e2)

            Unwrapped.Lambda { argument, body } ->
                Lambda
                    { argument = argument
                    , body = f body
                    }

            Unwrapped.Call { fn, argument } ->
                Call
                    { fn = f fn
                    , argument = f argument
                    }

            Unwrapped.If { test, then_, else_ } ->
                If
                    { test = f test
                    , then_ = f then_
                    , else_ = f else_
                    }

            Unwrapped.Let { bindings, body } ->
                Let
                    { bindings =
                        Dict.map
                            (always (Binding.map f))
                            bindings
                    , body = f body
                    }

            Unwrapped.List list ->
                List
                    (List.map f list)

            Unwrapped.Unit ->
                Unit

            Unwrapped.Tuple e1 e2 ->
                Tuple
                    (f e1)
                    (f e2)

            Unwrapped.Tuple3 e1 e2 e3 ->
                Tuple3
                    (f e1)
                    (f e2)
                    (f e3)

            Unwrapped.Record bindings ->
                Record <|
                    Dict.map
                        (always (Binding.map f))
                        bindings

            Unwrapped.RecordAccess e field ->
                RecordAccess (f e) field

            Unwrapped.Case e branches ->
                Case (f e) <|
                    List.NonEmpty.map
                        (\{ pattern, body } ->
                            { pattern = fromUnwrappedPattern pattern
                            , body = f body
                            }
                        )
                        branches

            Unwrapped.ConstructorValue rec ->
                ConstructorValue
                    { module_ = rec.module_
                    , name = rec.name
                    }


{-| Adds [**dummy** locations](Elm.Data.Located#dummyRegion) to the [Unwrapped.Pattern](Elm.AST.Canonical.Unwrapped#Pattern).
-}
fromUnwrappedPattern : Unwrapped.Pattern -> LocatedPattern
fromUnwrappedPattern pattern =
    let
        f =
            fromUnwrappedPattern
    in
    Located.located Located.dummyRegion <|
        case pattern of
            Unwrapped.PAnything ->
                PAnything

            Unwrapped.PVar varName ->
                PVar varName

            Unwrapped.PRecord varNames ->
                PRecord varNames

            Unwrapped.PAlias p varName ->
                PAlias (f p) varName

            Unwrapped.PUnit ->
                PUnit

            Unwrapped.PTuple p1 p2 ->
                PTuple (f p1) (f p2)

            Unwrapped.PTuple3 p1 p2 p3 ->
                PTuple3
                    (f p1)
                    (f p2)
                    (f p3)

            Unwrapped.PList ps ->
                PList (List.map f ps)

            Unwrapped.PCons p1 p2 ->
                PCons (f p1) (f p2)

            Unwrapped.PChar char ->
                PChar char

            Unwrapped.PString string ->
                PString string

            Unwrapped.PInt int ->
                PInt int

            Unwrapped.PFloat float ->
                PFloat float
