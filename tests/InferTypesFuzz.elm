module InferTypesFuzz exposing (typeInference)

import Dict exposing (Dict)
import Elm.AST.Canonical as Canonical
import Elm.AST.Canonical.Unwrapped as CanonicalU
import Elm.Compiler.Error exposing (TypeError(..))
import Elm.Data.Located as Located
import Elm.Data.Qualifiedness exposing (Qualified)
import Elm.Data.Type as Type
import Elm.Data.Type.Concrete as ConcreteType exposing (ConcreteType(..))
import Elm.Data.VarName exposing (VarName)
import Expect
import Fuzz exposing (Fuzzer)
import OurExtras.Tuple3 as Tuple3
import Random exposing (Generator)
import Random.Extra as Random
import Shrink exposing (Shrinker)
import Shrink.Extra as Shrink
import Stage.InferTypes
import Stage.InferTypes.SubstitutionMap as SubstitutionMap
import Test exposing (Test, describe, fuzz)
import TestHelpers exposing (dumpConcreteType)


typeInference : Test
typeInference =
    let
        fuzzExpr : ConcreteType Qualified -> Test
        fuzzExpr typeWanted =
            fuzz (exprOfType typeWanted) (dumpConcreteType typeWanted) <|
                \input ->
                    input
                        |> Canonical.fromUnwrapped
                        |> Stage.InferTypes.inferExpr Dict.empty 0 SubstitutionMap.empty
                        |> Result.map (Tuple3.first >> Located.unwrap >> Tuple.second)
                        |> Expect.equal (Ok (ConcreteType.toTypeOrId typeWanted))

        fuzzExpressions : String -> List (ConcreteType Qualified) -> Test
        fuzzExpressions description types =
            types
                |> List.map fuzzExpr
                |> describe description
    in
    describe "Stage.InferType"
        [ describe "inferExpr"
            [ fuzzExpressions "fuzz literals"
                [ Int
                , Float
                , Bool
                , Char
                , String
                , Unit
                ]
            , fuzzExpressions "fuzz lists"
                [ List Unit
                , List Int
                , List (List String)
                ]
            , fuzzExpressions "fuzz functions"
                [ Function { from = Int, to = Int }
                ]
            , fuzzExpressions "fuzz tuples"
                [ Tuple Int String
                , Tuple Bool Char
                , Tuple3 Int String Bool
                , Tuple3 Unit Char Float
                ]
            , fuzzExpressions "fuzz records"
                [ Record Dict.empty
                , Record (Dict.fromList [ ( "a", Int ) ])
                , Record (Dict.fromList [ ( "a", Int ), ( "b", String ) ])
                ]
            ]
        ]


exprOfType : ConcreteType Qualified -> Fuzzer CanonicalU.Expr
exprOfType targetType =
    Fuzz.custom
        (exprOfTypeWithDepth 3 targetType)
        shrinkExpr


exprOfTypeWithDepth : Int -> ConcreteType Qualified -> Generator CanonicalU.Expr
exprOfTypeWithDepth depthLeft targetType =
    let
        basicExpr =
            basicExprOfType depthLeft targetType

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


basicExprOfType : Int -> ConcreteType Qualified -> Generator CanonicalU.Expr
basicExprOfType depthLeft targetType =
    let
        cannotFuzz details =
            let
                prefix =
                    "Cannot fuzz `" ++ dumpConcreteType targetType ++ "` expressions."

                message =
                    if String.isEmpty details then
                        prefix

                    else
                        prefix ++ " " ++ details
            in
            Debug.todo message
    in
    case targetType of
        TypeVar _ ->
            cannotFuzz "Vars, by definition, cannot be supported"

        Int ->
            intExpr

        Float ->
            floatExpr

        Bool ->
            boolExpr

        Char ->
            charExpr

        String ->
            stringExpr

        Unit ->
            unitExpr

        List elementType ->
            if Type.isParametric (ConcreteType.toTypeOrId elementType) then
                {- Supporting parametric types might have weird interplay with recursive generation.
                   We are leaving it off, at least for now.
                -}
                cannotFuzz "Only lists with non-parametric element types are supported."

            else
                listExpr depthLeft elementType

        Function { from, to } ->
            case ( from, to ) of
                ( Int, Int ) ->
                    intToIntFunctionExpr

                _ ->
                    cannotFuzz "Only `Int -> Int` functions are supported."

        Tuple firstType secondType ->
            tupleExpr depthLeft ( firstType, secondType )

        Tuple3 firstType secondType thirdType ->
            tuple3Expr depthLeft ( firstType, secondType, thirdType )

        Record bindings ->
            recordExpr depthLeft bindings

        UserDefinedType _ ->
            {- TODO after Exprs contain constructors / record constructors, we
               can try to pick one specific subtype and generate it here
            -}
            cannotFuzz "TODO: Custom type constructors don't exist in Expr types yet"


ifExpr : Int -> ConcreteType Qualified -> Generator CanonicalU.Expr
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
        (subexpr Bool)
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


listExpr : Int -> ConcreteType Qualified -> Generator CanonicalU.Expr
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
            exprOfTypeWithDepth 0 Int
    in
    Random.map2 combine
        -- TODO: Later we will need something better to avoid shadowing.
        randomVarName
        intSubExpr


tupleExpr : Int -> ( ConcreteType Qualified, ConcreteType Qualified ) -> Generator CanonicalU.Expr
tupleExpr depthLeft ( firstType, secondType ) =
    Random.map2 CanonicalU.Tuple
        (firstType |> exprOfTypeWithDepth depthLeft)
        (secondType |> exprOfTypeWithDepth depthLeft)


tuple3Expr : Int -> ( ConcreteType Qualified, ConcreteType Qualified, ConcreteType Qualified ) -> Generator CanonicalU.Expr
tuple3Expr depthLeft ( firstType, secondType, thirdType ) =
    Random.map3 CanonicalU.Tuple3
        (firstType |> exprOfTypeWithDepth depthLeft)
        (secondType |> exprOfTypeWithDepth depthLeft)
        (thirdType |> exprOfTypeWithDepth depthLeft)


recordExpr : Int -> Dict VarName (ConcreteType Qualified) -> Generator CanonicalU.Expr
recordExpr depthLeft bindings =
    let
        typesGenerator =
            bindings
                |> Dict.values
                |> List.map (exprOfTypeWithDepth (depthLeft - 1))
                |> Random.combine

        namesGenerator =
            Dict.keys bindings |> Random.constant
    in
    Random.map2
        (\names exprs ->
            List.map2
                (\name expr ->
                    ( name, { name = name, body = expr } )
                )
                names
                exprs
                |> Dict.fromList
                |> CanonicalU.Record
        )
        namesGenerator
        typesGenerator


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
    let
        nope =
            Shrink.noShrink expr
    in
    case Debug.log "\nshrinking..." expr of
        CanonicalU.Int i ->
            i |> Shrink.int |> Shrink.map CanonicalU.Int

        CanonicalU.Float f ->
            f |> Shrink.float |> Shrink.map CanonicalU.Float

        CanonicalU.Char c ->
            c |> Shrink.char |> Shrink.map CanonicalU.Char

        CanonicalU.String s ->
            s |> Shrink.string |> Shrink.map CanonicalU.String

        CanonicalU.Bool b ->
            b |> Shrink.bool |> Shrink.map CanonicalU.Bool

        CanonicalU.Var _ ->
            nope

        CanonicalU.Argument _ ->
            nope

        CanonicalU.Plus left right ->
            shrinkPlus left right

        CanonicalU.Cons x xs ->
            shrinkCons x xs

        CanonicalU.Lambda { argument, body } ->
            shrinkLambda argument body

        CanonicalU.Call { fn, argument } ->
            shrinkCall fn argument

        CanonicalU.If { test, then_, else_ } ->
            shrinkIf test then_ else_

        CanonicalU.Let _ ->
            -- TODO take a stab at this? Do we actually even generate these?
            nope

        CanonicalU.List elements ->
            -- We are not using the default list shrinker here.
            -- It can turn a non-empty list empty.
            -- But the lists `[1]` and `[]` will have different types inferred.
            elements
                |> Shrink.listWithoutEmptying shrinkExpr
                |> Shrink.map CanonicalU.List

        CanonicalU.Unit ->
            -- not possible
            nope

        CanonicalU.Tuple first second ->
            shrinkTuple first second

        CanonicalU.Tuple3 first second third ->
            shrinkTuple3 first second third

        CanonicalU.Record _ ->
            -- TODO take a stab at this? Do we actually even generate these?
            nope

        CanonicalU.Case _ _ ->
            -- TODO take a stab at this? Do we actually even generate these?
            nope


{-| Shrinks a plus expression.

---

We cannot write a type annotation here.
The `LazyList a` type used by shrinkers is not exposed outside `elm-explorations/test`.

    shrinkPlus : CanonicalU.Expr -> CanonicalU.Expr -> LazyList CanonicalU.Expr

-}
shrinkPlus : CanonicalU.Expr -> CanonicalU.Expr -> LazyList CanonicalU.Expr
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


{-| Shrinks a cons expression.

---

We cannot write a type annotation here.
The `LazyList a` type used by shrinkers is not exposed outside `elm-explorations/test`.

    shrinkCons : CanonicalU.Expr -> CanonicalU.Expr -> LazyList CanonicalU.Expr

-}
shrinkCons : CanonicalU.Expr -> CanonicalU.Expr -> LazyList CanonicalU.Expr
shrinkCons x xs =
    ([ Shrink.map2 CanonicalU.Cons
        (shrinkExpr x)
        (Shrink.singleton xs)
     , Shrink.map2 CanonicalU.Cons
        (Shrink.singleton x)
        (shrinkExpr xs)
     ]
        |> List.map always
        |> Shrink.mergeMany
    )
        -- The value built up to this point is a shrinker.
        -- We need to call it with an CanonicalU.Expr to get a lazy list.
        x


{-| Shrinks a call expression.

---

We cannot write a type annotation here.
The `LazyList a` type used by shrinkers is not exposed outside `elm-explorations/test`.

    shrinkCall : CanonicalU.Expr -> CanonicalU.Expr -> LazyList CanonicalU.Expr

-}
shrinkCall : CanonicalU.Expr -> CanonicalU.Expr -> LazyList CanonicalU.Expr
shrinkCall fn arg =
    ([ Shrink.map2 call
        (shrinkExpr fn)
        (Shrink.singleton arg)
     , Shrink.map2 call
        (Shrink.singleton fn)
        (shrinkExpr arg)
     ]
        |> List.map always
        |> Shrink.mergeMany
    )
        -- The value built up to this point is a shrinker.
        -- We need to call it with an CanonicalU.Expr to get a lazy list.
        fn


{-| We cannot write a type annotation here.
The `LazyList a` type used by shrinkers is not exposed outside `elm-explorations/test`.

    shrinkIf : CanonicalU.Expr -> CanonicalU.Expr -> CanonicalU.Expr -> LazyList CanonicalU.Expr

-}
shrinkIf : CanonicalU.Expr -> CanonicalU.Expr -> CanonicalU.Expr -> LazyList CanonicalU.Expr
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
shrinkLambda : VarName -> CanonicalU.Expr -> LazyList CanonicalU.Expr
shrinkLambda argument body =
    body
        |> shrinkExpr
        |> Shrink.map (lambda argument)


{-| We cannot write a type annotation here.
The `LazyList a` type used by shrinkers is not exposed outside `elm-explorations/test`.

    shrinkTuple : CanonicalU.Expr -> CanonicalU.Expr -> LazyList CanonicalU.Expr

-}
shrinkTuple : CanonicalU.Expr -> CanonicalU.Expr -> LazyList CanonicalU.Expr
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
shrinkTuple3 : CanonicalU.Expr -> CanonicalU.Expr -> CanonicalU.Expr -> LazyList CanonicalU.Expr
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


call : CanonicalU.Expr -> CanonicalU.Expr -> CanonicalU.Expr
call fn argument =
    CanonicalU.Call
        { fn = fn
        , argument = argument
        }
