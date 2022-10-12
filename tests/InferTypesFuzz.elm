module InferTypesFuzz exposing (typeInference)

import Dict exposing (Dict)
import Elm.AST.Canonical as Canonical
import Elm.AST.Canonical.Unwrapped as CanonicalU
import Elm.Data.Located as Located
import Elm.Data.Qualifiedness exposing (Qualified)
import Elm.Data.Type as Type
import Elm.Data.Type.Concrete as ConcreteType exposing (ConcreteType(..))
import Elm.Data.VarName exposing (VarName)
import Expect
import Fuzz exposing (Fuzzer)
import Stage.InferTypes
import Stage.InferTypes.Environment as Env
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
                        |> Stage.InferTypes.inferExpr
                            Dict.empty
                            0
                            Env.empty
                            SubstitutionMap.empty
                        |> Result.map (Tuple.first >> Located.unwrap >> Tuple.second)
                        |> Expect.equal (Ok (ConcreteType.toTypeOrId typeWanted))

        fuzzExpressions : String -> List (ConcreteType Qualified) -> Test
        fuzzExpressions description types =
            types
                |> List.map fuzzExpr
                |> describe description
    in
    describe "Stage.InferTypesFuzz"
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
    exprOfTypeWithDepth 3 targetType


exprOfTypeWithDepth : Int -> ConcreteType Qualified -> Fuzzer CanonicalU.Expr
exprOfTypeWithDepth depthLeft targetType =
    let
        basicExpr =
            basicExprOfType depthLeft targetType

        pickAffordable ( cost, fuzzer ) =
            if cost <= depthLeft then
                targetType
                    |> fuzzer depthLeft
                    |> Just

            else
                Nothing
    in
    -- TODO: When generating Ints we should be able to use Plus here.
    [ ( 1, ifExpr )
    ]
        |> List.filterMap pickAffordable
        |> (::) basicExpr
        |> Fuzz.oneOf


basicExprOfType : Int -> ConcreteType Qualified -> Fuzzer CanonicalU.Expr
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


ifExpr : Int -> ConcreteType Qualified -> Fuzzer CanonicalU.Expr
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
    Fuzz.map3 combine
        (subexpr Bool)
        (subexpr targetType)
        (subexpr targetType)


intExpr : Fuzzer CanonicalU.Expr
intExpr =
    Fuzz.int
        |> Fuzz.map CanonicalU.Int


floatExpr : Fuzzer CanonicalU.Expr
floatExpr =
    -- Does not produce NaNs, but that should not be an issue for us.
    Fuzz.float
        |> Fuzz.map CanonicalU.Float


boolExpr : Fuzzer CanonicalU.Expr
boolExpr =
    Fuzz.bool
        |> Fuzz.map CanonicalU.Bool


charExpr : Fuzzer CanonicalU.Expr
charExpr =
    Fuzz.char
        |> Fuzz.map CanonicalU.Char


stringExpr : Fuzzer CanonicalU.Expr
stringExpr =
    Fuzz.stringOfLengthBetween 1 5
        |> Fuzz.map CanonicalU.String


unitExpr : Fuzzer CanonicalU.Expr
unitExpr =
    CanonicalU.Unit
        |> Fuzz.constant


listExpr : Int -> ConcreteType Qualified -> Fuzzer CanonicalU.Expr
listExpr depthLeft elementType =
    elementType
        |> exprOfTypeWithDepth depthLeft
        |> Fuzz.listOfLengthBetween 1 5
        |> Fuzz.map CanonicalU.List


intToIntFunctionExpr : Fuzzer CanonicalU.Expr
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
    Fuzz.map2 combine
        -- TODO: Later we will need something better to avoid shadowing.
        randomVarName
        intSubExpr


tupleExpr : Int -> ( ConcreteType Qualified, ConcreteType Qualified ) -> Fuzzer CanonicalU.Expr
tupleExpr depthLeft ( firstType, secondType ) =
    Fuzz.map2 CanonicalU.Tuple
        (firstType |> exprOfTypeWithDepth depthLeft)
        (secondType |> exprOfTypeWithDepth depthLeft)


tuple3Expr : Int -> ( ConcreteType Qualified, ConcreteType Qualified, ConcreteType Qualified ) -> Fuzzer CanonicalU.Expr
tuple3Expr depthLeft ( firstType, secondType, thirdType ) =
    Fuzz.map3 CanonicalU.Tuple3
        (firstType |> exprOfTypeWithDepth depthLeft)
        (secondType |> exprOfTypeWithDepth depthLeft)
        (thirdType |> exprOfTypeWithDepth depthLeft)


recordExpr : Int -> Dict VarName (ConcreteType Qualified) -> Fuzzer CanonicalU.Expr
recordExpr depthLeft bindings =
    let
        typesFuzzer =
            bindings
                |> Dict.values
                |> List.map (exprOfTypeWithDepth (depthLeft - 1))
                |> Fuzz.sequence

        namesFuzzer =
            Dict.keys bindings |> Fuzz.constant
    in
    Fuzz.map2
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
        namesFuzzer
        typesFuzzer


randomVarName : Fuzzer VarName
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
                |> Fuzz.oneOfValues

        firstChar =
            charFrom starters

        rest =
            Fuzz.listOfLengthBetween 1 5 <| charFrom all
    in
    Fuzz.map2 (::) firstChar rest
        |> Fuzz.map String.fromList


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
