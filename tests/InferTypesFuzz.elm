module InferTypesFuzz exposing (exprTyped)

import AST.Canonical as Canonical
import AST.Common.Literal exposing (Literal(..))
import AST.Common.Located as Located
import AST.Common.Type as Type exposing (Type)
import Common.Types exposing (VarName(..))
import Fuzz exposing (Fuzzer)
import Random exposing (Generator)
import Random.Extra as Random
import Shrink exposing (Shrinker)
import TestHelpers exposing (located)


exprTyped : Type -> Fuzzer Canonical.LocatedExpr
exprTyped targetType =
    Fuzz.custom
        (exprGenerator targetType)
        exprShrinker


exprGenerator : Type -> Generator Canonical.LocatedExpr
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


intExpr : Generator Canonical.LocatedExpr
intExpr =
    Random.int Random.minInt Random.maxInt
        |> Random.map (literal Int)


floatExpr : Generator Canonical.LocatedExpr
floatExpr =
    -- Does not produce NaNs, but that should not be an issue for us.
    Random.float (-1.0 / 0.0) (1.0 / 0.0)
        |> Random.map (literal Float)


boolExpr : Generator Canonical.LocatedExpr
boolExpr =
    Random.bool
        |> Random.map (literal Bool)


charExpr : Generator Canonical.LocatedExpr
charExpr =
    Random.int 0 0x0010FFFF
        |> Random.map Char.fromCode
        |> Random.map (literal Char)


stringExpr : Generator Canonical.LocatedExpr
stringExpr =
    Random.int 0 0x0010FFFF
        |> Random.list 10
        |> Random.map (List.map Char.fromCode)
        |> Random.map String.fromList
        |> Random.map (literal String)


unitExpr : Generator Canonical.LocatedExpr
unitExpr =
    Canonical.Unit
        |> located
        |> Random.constant


literal : (a -> Literal) -> a -> Canonical.LocatedExpr
literal wrap value =
    value
        |> wrap
        |> Canonical.Literal
        |> located


listExpr : Type -> Generator Canonical.LocatedExpr
listExpr elementType =
    elementType
        |> exprGenerator
        |> Random.list 10
        |> Random.map Canonical.List
        |> Random.map located


intToIntFunctionExpr : Generator Canonical.LocatedExpr
intToIntFunctionExpr =
    let
        combine argument intPart =
            Canonical.lambda argument <|
                located <|
                    Canonical.Plus
                        (located <| Canonical.Argument argument)
                        intPart

        intSubExpr =
            Type.Int |> exprGenerator
    in
    Random.map2 combine
        -- TODO: Later we will need something better to avoid shadowing.
        randomVarName
        intSubExpr
        |> Random.map located


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


exprShrinker : Shrinker Canonical.LocatedExpr
exprShrinker expr =
    case Located.unwrap expr of
        Canonical.Literal lit ->
            lit
                |> shrinkLiteral
                |> Shrink.map Canonical.Literal
                |> Shrink.map located

        Canonical.Plus left right ->
            expr
                |> Debug.log "shrinking (+)"
                |> shrinkPlus left right

        Canonical.List elements ->
            elements
                |> shrinkNonEmptyList exprShrinker
                |> Shrink.map Canonical.List
                |> Shrink.map located

        Canonical.If { then_, else_ } ->
            [ then_, else_ ]
                |> List.map shrinkTo
                |> concatShrink
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


shrinkPlus : Canonical.LocatedExpr -> Canonical.LocatedExpr -> Shrinker Canonical.LocatedExpr
shrinkPlus left right _ =
    lazyMap2 Canonical.Plus
        (exprShrinker left)
        (exprShrinker right)
        |> Shrink.map located


shrinkLambda : VarName -> Canonical.LocatedExpr -> Shrinker Canonical.LocatedExpr
shrinkLambda argument body _ =
    body
        |> exprShrinker
        |> Shrink.map (Canonical.lambda argument)
        |> Shrink.map located


shrinkNonEmptyList : Shrinker a -> Shrinker (List a)
shrinkNonEmptyList shrinkElement list =
    case list of
        [] ->
            list |> Shrink.noShrink

        first :: rest ->
            lazyMap2 (::)
                (shrinkElement first)
                (Shrink.list shrinkElement rest)


shrinkTo : a -> Shrinker a
shrinkTo shrunk _ =
    True
        |> Shrink.bool
        |> Shrink.map (always shrunk)


{-| Calls a shrinker with the supplied argument.

Sometimes you build up a shrinker based on the value to shrink.
Then you need to invoke it.
Without `shrink` that is

    (value
        |> someFunc
        |> someOtherFunc
    )
        value

With `shrink` it becomes

    value
        |> someFunc
        |> someOtherFunc
        |> shrink value

---

We cannot write a type annotation here.
The `LazyList a` type used by shrinkers is not exposed outside `elm-explorations/test`.

    shrink : a -> Shrinker a -> Shrinker a

-}
shrink expr shrinker =
    shrinker expr


{-| Combines two lazy lists using a combining function.

---

We cannot write a type annotation here.
The `LazyList a` type used by shrinkers is not exposed outside `elm-explorations/test`.

    lazyMap2 : (a -> b -> c) -> LazyList a -> LazyList b -> LazyList c

-}
lazyMap2 f la lb =
    Shrink.map f la
        |> Shrink.andMap lb


concatShrink : List (Shrinker a) -> Shrinker a
concatShrink shrinkers =
    shrinkers
        |> List.foldl Shrink.merge Shrink.noShrink
