module InferTypesFuzz exposing (typeInference)

import AST.Canonical as Canonical
import AST.Canonical.Unwrapped as Unwrapped
import AST.Common.Literal as Literal exposing (Literal(..))
import AST.Common.Located as Located
import AST.Common.Type as Type exposing (Type)
import AST.Typed as Typed
import Common
import Common.Types as Types exposing (VarName(..))
import Dict.Any
import Error exposing (TypeError(..))
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Random exposing (Generator)
import Random.Extra as Random
import Shrink exposing (Shrinker)
import Stage.InferTypes
import Test exposing (Test, describe, fuzz, test)
import TestHelpers exposing (located)


typeInference : Test
typeInference =
    let
        fuzzExpr : Type -> Test
        fuzzExpr typeWanted =
            fuzz (exprOfType typeWanted) (dumpType typeWanted) <|
                \input ->
                    input
                        |> Canonical.fromUnwrapped
                        |> Stage.InferTypes.inferExpr
                        |> Result.map Located.unwrap
                        |> Result.map Tuple.second
                        |> Expect.equal (Ok typeWanted)

        fuzzExpressions : String -> List Type -> Test
        fuzzExpressions description types =
            types
                |> List.map fuzzExpr
                |> describe description
    in
    describe "Stage.InferType"
        [ describe "fuzz exprInfer"
            [ fuzzExpressions "literals"
                [ Type.Int
                , Type.Float
                , Type.Bool
                , Type.Char
                , Type.String
                , Type.Unit
                ]
            , fuzzExpressions "lists"
                [ Type.List Type.Unit
                , Type.List Type.Int
                , Type.List (Type.List Type.String)
                ]
            , fuzzExpressions "functions"
                [ Type.Function Type.Int Type.Int
                ]
            ]
        ]


exprOfType : Type -> Fuzzer Unwrapped.Expr
exprOfType targetType =
    Fuzz.custom
        (exprOfTypeWithDepth 3 targetType)
        exprShrinker


exprOfTypeWithDepth : Int -> Type -> Generator Unwrapped.Expr
exprOfTypeWithDepth depthLeft targetType =
    let
        pickAffordable ( cost, generator ) =
            if cost <= depthLeft then
                targetType
                    |> generator depthLeft
                    |> Just

            else
                Nothing
    in
    [ ( 1, ifExpr ) ]
        |> List.filterMap pickAffordable
        |> Random.choices (basicExprOfType targetType)


basicExprOfType : Type -> Generator Unwrapped.Expr
basicExprOfType targetType =
    let
        cannotFuzz details =
            let
                prefix =
                    "Cannot fuzz `" ++ dumpType targetType ++ "` expressions."

                message =
                    if String.isEmpty details then
                        prefix

                    else
                        prefix ++ " " ++ details
            in
            Debug.todo message
    in
    case targetType of
        Type.Int ->
            intExpr

        Type.Float ->
            floatExpr

        Type.Bool ->
            boolExpr

        Type.Char ->
            charExpr

        Type.String ->
            stringExpr

        Type.Unit ->
            unitExpr

        Type.List elementType ->
            if Type.isParametric elementType then
                -- Supporting parametric types might have weird interplay with recursive generation.
                -- We are leaving it off, at least for now.
                cannotFuzz "Only lists with non-parametric element types are supported."

            else
                listExpr 0 elementType

        Type.Function Type.Int Type.Int ->
            intToIntFunctionExpr

        Type.Function _ _ ->
            cannotFuzz "Only `Int -> Int` functions are supported."

        _ ->
            cannotFuzz ""


ifExpr : Int -> Type -> Generator Unwrapped.Expr
ifExpr depth targetType =
    let
        combine test then_ else_ =
            Unwrapped.If
                { test = test
                , then_ = then_
                , else_ = else_
                }

        subexpr =
            exprOfTypeWithDepth (depth - 1)
    in
    Random.map3 combine
        (subexpr Type.Bool)
        (subexpr targetType)
        (subexpr targetType)


intExpr : Generator Unwrapped.Expr
intExpr =
    Random.int Random.minInt Random.maxInt
        |> Random.map (literal Int)


floatExpr : Generator Unwrapped.Expr
floatExpr =
    -- Does not produce NaNs, but that should not be an issue for us.
    Random.float (-1.0 / 0.0) (1.0 / 0.0)
        |> Random.map (literal Float)


boolExpr : Generator Unwrapped.Expr
boolExpr =
    Random.bool
        |> Random.map (literal Bool)


charExpr : Generator Unwrapped.Expr
charExpr =
    Random.int 0 0x0010FFFF
        |> Random.map Char.fromCode
        |> Random.map (literal Char)


stringExpr : Generator Unwrapped.Expr
stringExpr =
    Random.int 0 0x0010FFFF
        |> Random.list 10
        |> Random.map (List.map Char.fromCode)
        |> Random.map String.fromList
        |> Random.map (literal String)


unitExpr : Generator Unwrapped.Expr
unitExpr =
    Unwrapped.Unit
        |> Random.constant


literal : (a -> Literal) -> a -> Unwrapped.Expr
literal wrap value =
    value
        |> wrap
        |> Unwrapped.Literal


listExpr : Int -> Type -> Generator Unwrapped.Expr
listExpr depth elementType =
    elementType
        |> exprOfTypeWithDepth depth
        |> Random.list 10
        |> Random.map Unwrapped.List


intToIntFunctionExpr : Generator Unwrapped.Expr
intToIntFunctionExpr =
    let
        combine argument intPart =
            lambda argument <|
                Unwrapped.Plus
                    (Unwrapped.Argument argument)
                    intPart

        intSubExpr =
            Type.Int |> exprOfTypeWithDepth 0
    in
    Random.map2 combine
        -- TODO: Later we will need something better to avoid shadowing.
        randomVarName
        intSubExpr


randomVarName : Generator VarName
randomVarName =
    let
        starters =
            "abcdefghijklnmoqprstuvwxyz"

        others =
            "ABCDEFGHIJKLMNOQPRSTUVQXYZ_0123456789"

        all =
            starters ++ others

        charFrom string =
            string
                |> String.toList
                |> List.map Random.constant
                |> Random.choices (Random.constant 'a')

        firstChar =
            charFrom starters

        rest =
            Random.list 5 <| charFrom all
    in
    Random.map2 (::) firstChar rest
        |> Random.map String.fromList
        |> Random.map VarName


dumpType : Type -> String
dumpType type_ =
    type_
        |> Type.toString Type.emptyState
        |> Tuple.first


{-| An expression shrinker that preserves the inferred type.
-}
exprShrinker : Shrinker Unwrapped.Expr
exprShrinker expr =
    case expr of
        Unwrapped.Literal lit ->
            lit
                |> shrinkLiteral
                |> Shrink.map Unwrapped.Literal

        Unwrapped.Plus left right ->
            shrinkPlus left right

        Unwrapped.List elements ->
            -- We are not using the default list shrinker here.
            -- It can turn a non-empty list empty.
            -- But the lists `[1]` and `[]` will have different types inferred.
            elements
                |> shrinkNonEmptyList exprShrinker
                |> Shrink.map Unwrapped.List

        Unwrapped.If { test, then_, else_ } ->
            shrinkIf test then_ else_

        Unwrapped.Lambda { argument, body } ->
            shrinkLambda argument body

        _ ->
            Shrink.noShrink expr


shrinkLiteral : Shrinker Literal
shrinkLiteral lit =
    case lit of
        Int i ->
            i |> Shrink.int |> Shrink.map Int

        Float f ->
            f |> Shrink.float |> Shrink.map Float

        Char c ->
            c |> Shrink.char |> Shrink.map Char

        Bool b ->
            b |> Shrink.bool |> Shrink.map Bool

        String s ->
            s |> Shrink.string |> Shrink.map String


{-| Shrinks a plus expression.

---

We cannot write a type annotation here.
The `LazyList a` type used by shrinkers is not exposed outside `elm-explorations/test`.

    shrinkPlus : Unwrapped.Expr -> Unwrapped.Expr -> LazyList Unwrapped.Expr

-}
shrinkPlus left right =
    ([ lazyMap2 Unwrapped.Plus
        (exprShrinker left)
        (singleton right)
     , lazyMap2 Unwrapped.Plus
        (singleton left)
        (exprShrinker right)
     ]
        |> List.map always
        |> concatShrink
    )
        -- The value built up to this point is a shrinker.
        -- We need to call it with an Unwrapped.Expr to get a lazy list.
        left



-- shrinkIf : Unwrapped.Expr -> Unwrapped.Expr -> Unwrapped.Expr ->


{-| We cannot write a type annotation here.
The `LazyList a` type used by shrinkers is not exposed outside `elm-explorations/test`.

    shrinkIf : Unwrapped.Expr -> Unwrapped.Expr -> Unwrapped.Expr -> LazyList Unwrapped.Expr

-}
shrinkIf test then_ else_ =
    let
        withTest shrunkTest =
            Unwrapped.If
                { test = shrunkTest
                , then_ = then_
                , else_ = else_
                }
    in
    ([ singleton then_
     , singleton else_
     , test |> exprShrinker |> Shrink.map withTest
     ]
        |> List.map always
        |> concatShrink
    )
        -- The value built up to this point is a shrinker.
        -- We need to call it with an Unwrapped.Expr to get a lazy list.
        test


{-| We cannot write a type annotation here.
The `LazyList a` type used by shrinkers is not exposed outside `elm-explorations/test`.

    shrinkLambda : VarName -> Unwrapped.Expr -> LazyList Unwrapped.Expr

-}
shrinkLambda argument body =
    body
        |> exprShrinker
        |> Shrink.map (lambda argument)


{-| Given a shrinker of elements, produces a shrinker of lists.

This is different from `Shrink.list` in that it keeps non-empty lists non-empty.

So if you try shriking `[]` you will get nothing.
And if you try shrinking `[1]` you might get `[0]`, but you will not get `[]`.

-}
shrinkNonEmptyList : Shrinker a -> Shrinker (List a)
shrinkNonEmptyList shrinkElement list =
    case list of
        first :: rest ->
            lazyMap2 (::)
                (shrinkElement first)
                (Shrink.list shrinkElement rest)

        [] ->
            Shrink.noShrink list


shrinkTo : a -> Shrinker a
shrinkTo shrunk _ =
    True
        |> Shrink.bool
        |> Shrink.map (always shrunk)


{-| Combines two lazy lists using a combining function.

---

We cannot write a type annotation here.
The `LazyList a` type used by shrinkers is not exposed outside `elm-explorations/test`.

    lazyMap2 : (a -> b -> c) -> LazyList a -> LazyList b -> LazyList c

-}
lazyMap2 f la lb =
    Shrink.map f la
        |> Shrink.andMap lb


{-| Produces a single element lazy list.

---

We cannot write a type annotation here.
The `LazyList a` type used by shrinkers is not exposed outside `elm-explorations/test`.

    singleton : a -> LazyList a

-}
singleton x =
    x |> shrinkTo x


concatShrink : List (Shrinker a) -> Shrinker a
concatShrink shrinkers =
    shrinkers
        |> List.foldl Shrink.merge Shrink.noShrink


lambda : VarName -> Unwrapped.Expr -> Unwrapped.Expr
lambda argument body =
    Unwrapped.Lambda
        { argument = argument
        , body = body
        }
