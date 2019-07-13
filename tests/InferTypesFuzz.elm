module InferTypesFuzz exposing (randomExprFromType)

import AST.Canonical as Canonical
import AST.Common.Literal exposing (Literal(..))
import AST.Common.Type as Type exposing (Type)
import Common.Types exposing (VarName(..))
import Fuzz exposing (Fuzzer)


randomExprFromType : Type -> Fuzzer Canonical.Expr
randomExprFromType targetType =
    let
        cannotFuzz details =
            let
                prefix =
                    "Cannot fuzz `" ++ dumpType targetType ++ "` expressions."

                message =
                    if details |> String.isEmpty then
                        prefix

                    else
                        prefix ++ " " ++ details
            in
            message |> Debug.todo
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
            if elementType |> Type.isNotParametric then
                listExpr elementType

            else
                cannotFuzz "Only lists with non-parametric element types are supported."

        Type.Function Type.Int Type.Int ->
            intToIntFunctionExpr

        Type.Function _ _ ->
            cannotFuzz "Only `Int -> Int` functions are supported."

        _ ->
            cannotFuzz ""


intExpr : Fuzzer Canonical.Expr
intExpr =
    Fuzz.int
        |> Fuzz.map Int
        |> Fuzz.map Canonical.Literal


floatExpr : Fuzzer Canonical.Expr
floatExpr =
    Fuzz.float
        |> Fuzz.map Float
        |> Fuzz.map Canonical.Literal


boolExpr : Fuzzer Canonical.Expr
boolExpr =
    Fuzz.bool
        |> Fuzz.map Bool
        |> Fuzz.map Canonical.Literal


charExpr : Fuzzer Canonical.Expr
charExpr =
    Fuzz.intRange 0 0x0010FFFF
        |> Fuzz.map Char.fromCode
        |> Fuzz.map Char
        |> Fuzz.map Canonical.Literal


stringExpr : Fuzzer Canonical.Expr
stringExpr =
    Fuzz.intRange 0 0x0010FFFF
        |> Fuzz.list
        |> Fuzz.map (List.map Char.fromCode)
        |> Fuzz.map String.fromList
        |> Fuzz.map String
        |> Fuzz.map Canonical.Literal


unitExpr : Fuzzer Canonical.Expr
unitExpr =
    Canonical.Unit |> Fuzz.constant


listExpr : Type -> Fuzzer Canonical.Expr
listExpr elementType =
    let
        elementExpr =
            elementType |> randomExprFromType
    in
    elementExpr
        |> Fuzz.list
        |> Fuzz.map2 (::) elementExpr
        |> Fuzz.map Canonical.List


intToIntFunctionExpr : Fuzzer Canonical.Expr
intToIntFunctionExpr =
    let
        wrapBody argumentName expr =
            Canonical.Lambda
                { argument = argumentName
                , body = Canonical.Plus expr (Canonical.Argument argumentName)
                }
    in
    Fuzz.map2 wrapBody
        -- TODO: Later we will need something better to avoid shadowing.
        randomVarName
        (Type.Int |> randomExprFromType)


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
                |> List.map Fuzz.constant
                |> Fuzz.oneOf

        firstChar =
            charFrom starters

        rest =
            Fuzz.list <| charFrom all
    in
    Fuzz.map2 (::) firstChar rest
        |> Fuzz.map String.fromList
        |> Fuzz.map VarName


dumpType : Type -> String
dumpType type_ =
    type_
        |> Type.toString Type.emptyState
        |> Tuple.first
