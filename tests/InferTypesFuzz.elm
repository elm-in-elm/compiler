module InferTypesFuzz exposing (exprTyped)

import AST.Canonical as Canonical
import AST.Common.Literal exposing (Literal(..))
import AST.Common.Type as Type exposing (Type)
import Common.Types exposing (VarName(..))
import Fuzz exposing (Fuzzer)
import Random exposing (Generator)
import Random.Extra as Random
import Shrink exposing (Shrinker)


exprTyped : Type -> Fuzzer Canonical.Expr
exprTyped targetType =
    Fuzz.custom
        (exprGenerator targetType)
        exprShrinker


exprGenerator : Type -> Generator Canonical.Expr
exprGenerator targetType =
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


intExpr : Generator Canonical.Expr
intExpr =
    Random.int Random.minInt Random.maxInt
        |> Random.map (literal Int)


floatExpr : Generator Canonical.Expr
floatExpr =
    -- Does not produce NaNs, but that should not be an issue for us.
    Random.float (-1.0 / 0.0) (1.0 / 0.0)
        |> Random.map (literal Float)


boolExpr : Generator Canonical.Expr
boolExpr =
    Random.bool
        |> Random.map (literal Bool)


charExpr : Generator Canonical.Expr
charExpr =
    Random.int 0 0x0010FFFF
        |> Random.map Char.fromCode
        |> Random.map (literal Char)


stringExpr : Generator Canonical.Expr
stringExpr =
    Random.int 0 0x0010FFFF
        |> Random.list 10
        |> Random.map (List.map Char.fromCode)
        |> Random.map String.fromList
        |> Random.map (literal String)


unitExpr : Generator Canonical.Expr
unitExpr =
    Canonical.Unit |> Random.constant


literal : (a -> Literal) -> a -> Canonical.Expr
literal wrap value =
    value
        |> wrap
        |> Canonical.Literal


listExpr : Type -> Generator Canonical.Expr
listExpr elementType =
    elementType
        |> exprGenerator
        |> Random.list 10
        |> Random.map Canonical.List


intToIntFunctionExpr : Generator Canonical.Expr
intToIntFunctionExpr =
    let
        wrapBody argumentName expr =
            Canonical.Lambda
                { argument = argumentName
                , body = Canonical.Plus expr (Canonical.Argument argumentName)
                }

        bodyExpr =
            Type.Int |> exprGenerator
    in
    Random.map2 wrapBody
        -- TODO: Later we will need something better to avoid shadowing.
        randomVarName
        bodyExpr


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


exprShrinker : Shrinker Canonical.Expr
exprShrinker expr =
    case expr of
        Canonical.Literal lit ->
            lit
                |> shrinkLiteral
                |> Shrink.map Canonical.Literal

        Canonical.Plus left right ->
            expr
                |> shrinkPlus left right

        Canonical.List elements ->
            elements
                |> shrinkNonEmptyList exprShrinker
                |> Shrink.map Canonical.List

        Canonical.If { then_, else_ } ->
            [ then_, else_ ]
                |> List.map shrinkTo
                |> concatShrink
                -- Lesser hack.
                |> shrink expr

        Canonical.Lambda { argument, body } ->
            expr
                |> shrinkLambda argument body

        _ ->
            expr |> Shrink.noShrink


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


shrinkPlus : Canonical.Expr -> Canonical.Expr -> Shrinker Canonical.Expr
shrinkPlus left right _ =
    -- TODO: The Shrink docs were misleading here. Consider reporting an issue.
    exprShrinker left
        |> Shrink.map Canonical.Plus
        |> Shrink.andMap (exprShrinker right)


shrinkLambda : VarName -> Canonical.Expr -> Shrinker Canonical.Expr
shrinkLambda argument body _ =
    let
        combine shrunkBody =
            Canonical.Lambda
                { argument = argument
                , body = shrunkBody
                }
    in
    body
        |> exprShrinker
        |> Shrink.map combine


shrinkNonEmptyList : Shrinker a -> Shrinker (List a)
shrinkNonEmptyList shrinkElement list =
    case list of
        [] ->
            list |> Shrink.noShrink

        first :: rest ->
            -- TODO: The Shrink docs were misleading here. Consider reporting an issue.
            shrinkElement first
                |> Shrink.map (::)
                |> Shrink.andMap (Shrink.list shrinkElement rest)


shrinkTo : a -> Shrinker a
shrinkTo shrunk _ =
    True
        |> Shrink.bool
        |> Shrink.map (always shrunk)


{-| We cannot write a type annotation here.
The `LazyList a` type used by shrinkers is not exposed anywhere.
-}
shrink expr shrinker =
    shrinker expr


concatShrink : List (Shrinker a) -> Shrinker a
concatShrink shrinkers =
    shrinkers
        |> List.foldl Shrink.merge Shrink.noShrink
