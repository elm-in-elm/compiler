module InferTypesFuzz exposing (typeInference)

import Elm.AST.Canonical as Canonical
import Elm.AST.Canonical.Unwrapped as CanonicalU
import Elm.AST.Typed as Typed
import Elm.Compiler.Error exposing (TypeError(..))
import Elm.Data.Located as Located
import Elm.Data.Type as Type exposing (Type)
import Elm.Data.VarName as VarName exposing (VarName)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Random exposing (Generator)
import Random.Extra as Random
import Shrink exposing (Shrinker)
import Shrink.Extra as Shrink
import Stage.InferTypes
import Test exposing (Test, describe, fuzz, test)
import TestHelpers exposing (dumpType)


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
        [ describe "inferExpr"
            [ fuzzExpressions "fuzz literals"
                [ Type.Int
                , Type.Float
                , Type.Bool
                , Type.Char
                , Type.String
                , Type.Unit
                ]
            , fuzzExpressions "fuzz lists"
                [ Type.List Type.Unit
                , Type.List Type.Int
                , Type.List (Type.List Type.String)
                ]
            , fuzzExpressions "fuzz functions"
                [ Type.Function Type.Int Type.Int
                ]
            , fuzzExpressions "fuzz tuples"
                [ Type.Tuple Type.Int Type.String
                , Type.Tuple Type.Bool Type.Char
                , Type.Tuple3 Type.Int Type.String Type.Bool
                , Type.Tuple3 Type.Unit Type.Char Type.Float
                ]
            ]
        ]


exprOfType : Type -> Fuzzer CanonicalU.Expr
exprOfType targetType =
    Fuzz.custom
        (exprOfTypeWithDepth 3 targetType)
        shrinkExpr


exprOfTypeWithDepth : Int -> Type -> Generator CanonicalU.Expr
exprOfTypeWithDepth depthLeft targetType =
    let
        basicExpr =
            targetType |> basicExprOfType depthLeft

        pickAffordable ( cost, generator ) =
            if cost <= depthLeft then
                targetType
                    |> generator depthLeft
                    |> Just

            else
                Nothing
    in
    -- TODO: When generating Ints we should be able to use Plus here.
    [ ( 1, ifExpr ) ]
        |> List.filterMap pickAffordable
        |> Random.choices basicExpr


basicExprOfType : Int -> Type -> Generator CanonicalU.Expr
basicExprOfType depthLeft targetType =
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
                elementType |> listExpr depthLeft

        Type.Function Type.Int Type.Int ->
            intToIntFunctionExpr

        Type.Function _ _ ->
            cannotFuzz "Only `Int -> Int` functions are supported."

        Type.Tuple firstType secondType ->
            ( firstType, secondType ) |> tupleExpr depthLeft

        Type.Tuple3 firstType secondType thirdType ->
            ( firstType, secondType, thirdType ) |> tuple3Expr depthLeft

        _ ->
            cannotFuzz ""


ifExpr : Int -> Type -> Generator CanonicalU.Expr
ifExpr depth targetType =
    let
        combine test then_ else_ =
            CanonicalU.If
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


intExpr : Generator CanonicalU.Expr
intExpr =
    Random.int Random.minInt Random.maxInt
        |> Random.map CanonicalU.Int


floatExpr : Generator CanonicalU.Expr
floatExpr =
    -- Does not produce NaNs, but that should not be an issue for us.
    Random.float (-1.0 / 0.0) (1.0 / 0.0)
        |> Random.map CanonicalU.Float


boolExpr : Generator CanonicalU.Expr
boolExpr =
    Random.bool
        |> Random.map CanonicalU.Bool


charExpr : Generator CanonicalU.Expr
charExpr =
    Random.int 0 0x0010FFFF
        |> Random.map Char.fromCode
        |> Random.map CanonicalU.Char


stringExpr : Generator CanonicalU.Expr
stringExpr =
    Random.int 0 0x0010FFFF
        |> Random.list 10
        |> Random.map (List.map Char.fromCode)
        |> Random.map String.fromList
        |> Random.map CanonicalU.String


unitExpr : Generator CanonicalU.Expr
unitExpr =
    CanonicalU.Unit
        |> Random.constant


listExpr : Int -> Type -> Generator CanonicalU.Expr
listExpr depthLeft elementType =
    elementType
        |> exprOfTypeWithDepth depthLeft
        |> Random.list 10
        |> Random.map CanonicalU.List


intToIntFunctionExpr : Generator CanonicalU.Expr
intToIntFunctionExpr =
    let
        combine argument intPart =
            lambda argument <|
                CanonicalU.Plus
                    (CanonicalU.Argument argument)
                    intPart

        intSubExpr =
            Type.Int |> exprOfTypeWithDepth 0
    in
    Random.map2 combine
        -- TODO: Later we will need something better to avoid shadowing.
        randomVarName
        intSubExpr


tupleExpr : Int -> ( Type, Type ) -> Generator CanonicalU.Expr
tupleExpr depthLeft ( firstType, secondType ) =
    Random.map2 CanonicalU.Tuple
        (firstType |> exprOfTypeWithDepth depthLeft)
        (secondType |> exprOfTypeWithDepth depthLeft)


tuple3Expr : Int -> ( Type, Type, Type ) -> Generator CanonicalU.Expr
tuple3Expr depthLeft ( firstType, secondType, thirdType ) =
    Random.map3 CanonicalU.Tuple3
        (firstType |> exprOfTypeWithDepth depthLeft)
        (secondType |> exprOfTypeWithDepth depthLeft)
        (thirdType |> exprOfTypeWithDepth depthLeft)


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


{-| An expression shrinker that preserves the inferred type.
-}
shrinkExpr : Shrinker CanonicalU.Expr
shrinkExpr expr =
    case expr |> Debug.log "\nshrinking... " of
        CanonicalU.Int i ->
            i |> Shrink.int |> Shrink.map CanonicalU.Int

        CanonicalU.Float f ->
            f |> Shrink.float |> Shrink.map CanonicalU.Float

        CanonicalU.Char c ->
            c |> Shrink.char |> Shrink.map CanonicalU.Char

        CanonicalU.Bool b ->
            b |> Shrink.bool |> Shrink.map CanonicalU.Bool

        CanonicalU.String s ->
            s |> Shrink.string |> Shrink.map CanonicalU.String

        CanonicalU.Plus left right ->
            shrinkPlus left right

        CanonicalU.List elements ->
            -- We are not using the default list shrinker here.
            -- It can turn a non-empty list empty.
            -- But the lists `[1]` and `[]` will have different types inferred.
            elements
                |> Shrink.listWithoutEmptying shrinkExpr
                |> Shrink.map CanonicalU.List

        CanonicalU.If { test, then_, else_ } ->
            shrinkIf test then_ else_

        CanonicalU.Lambda { argument, body } ->
            shrinkLambda argument body

        CanonicalU.Tuple first second ->
            shrinkTuple first second

        CanonicalU.Tuple3 first second third ->
            shrinkTuple3 first second third

        _ ->
            Shrink.noShrink expr


{-| Shrinks a plus expression.

---

We cannot write a type annotation here.
The `LazyList a` type used by shrinkers is not exposed outside `elm-explorations/test`.

    shrinkPlus : CanonicalU.Expr -> CanonicalU.Expr -> LazyList CanonicalU.Expr

-}
shrinkPlus left right =
    ([ Shrink.map2 CanonicalU.Plus
        (shrinkExpr left)
        (Shrink.singleton right)
     , Shrink.map2 CanonicalU.Plus
        (Shrink.singleton left)
        (shrinkExpr right)
     ]
        |> List.map always
        |> Shrink.mergeMany
    )
        -- The value built up to this point is a shrinker.
        -- We need to call it with an CanonicalU.Expr to get a lazy list.
        left


{-| We cannot write a type annotation here.
The `LazyList a` type used by shrinkers is not exposed outside `elm-explorations/test`.

    shrinkIf : CanonicalU.Expr -> CanonicalU.Expr -> CanonicalU.Expr -> LazyList CanonicalU.Expr

-}
shrinkIf test then_ else_ =
    let
        withTest shrunkTest =
            CanonicalU.If
                { test = shrunkTest
                , then_ = then_
                , else_ = else_
                }
    in
    ([ Shrink.singleton then_
     , Shrink.singleton else_
     , test |> shrinkExpr |> Shrink.map withTest
     ]
        |> List.map always
        |> Shrink.mergeMany
    )
        -- The value built up to this point is a shrinker.
        -- We need to call it with an CanonicalU.Expr to get a lazy list.
        test


{-| We cannot write a type annotation here.
The `LazyList a` type used by shrinkers is not exposed outside `elm-explorations/test`.

    shrinkLambda : VarName -> CanonicalU.Expr -> LazyList CanonicalU.Expr

-}
shrinkLambda argument body =
    body
        |> shrinkExpr
        |> Shrink.map (lambda argument)


{-| We cannot write a type annotation here.
The `LazyList a` type used by shrinkers is not exposed outside `elm-explorations/test`.

    shrinkTuple : CanonicalU.Expr -> CanonicalU.Expr -> LazyList CanonicalU.Expr

-}
shrinkTuple first second =
    ([ Shrink.map2 CanonicalU.Tuple
        (shrinkExpr first)
        (Shrink.singleton second)
     , Shrink.map2 CanonicalU.Tuple
        (Shrink.singleton first)
        (shrinkExpr second)
     ]
        |> List.map always
        |> Shrink.mergeMany
    )
        -- The value built up to this point is a shrinker.
        -- We need to call it with an CanonicalU.Expr to get a lazy list.
        first


{-| We cannot write a type annotation here.
The `LazyList a` type used by shrinkers is not exposed outside `elm-explorations/test`.

    shrinkTuple3 : CanonicalU.Expr -> CanonicalU.Expr -> CanonicalU.Expr -> LazyList CanonicalU.Expr

-}
shrinkTuple3 first second third =
    ([ Shrink.map3 CanonicalU.Tuple3
        (shrinkExpr first)
        (Shrink.singleton second)
        (Shrink.singleton third)
     , Shrink.map3 CanonicalU.Tuple3
        (Shrink.singleton first)
        (shrinkExpr second)
        (Shrink.singleton third)
     , Shrink.map3 CanonicalU.Tuple3
        (Shrink.singleton first)
        (Shrink.singleton second)
        (shrinkExpr third)
     ]
        |> List.map always
        |> Shrink.mergeMany
    )
        -- The value built up to this point is a shrinker.
        -- We need to call it with an CanonicalU.Expr to get a lazy list.
        first


lambda : VarName -> CanonicalU.Expr -> CanonicalU.Expr
lambda argument body =
    CanonicalU.Lambda
        { argument = argument
        , body = body
        }
