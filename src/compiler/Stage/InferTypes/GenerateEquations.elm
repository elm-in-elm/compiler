module Stage.InferTypes.GenerateEquations exposing (generateEquations)

{-| Stage 2

We try to bind various subexpressions together here.
For example consider this:

    42

This is an Integer (or a `number`, we don't care here for the sake of simplicity).

    foo 42

What can we say about `foo`? It's a function from Integer (as evidenced by 42),
but we don't know to what. Right now the best we could say is `foo : Int -> a`.

    foo 42 == "abc"

Suddenly everything's clear: the whole thing is Bool because of `==`, and thanks
to the right side of `==` being a String and the two sides having to match in types,
we know `foo 42` is a String too, and thus `foo : Int -> String`.

The "two sides of `==` have to match" insight could be expressed here as something like

    equals leftType rightType

The goal of this module is to build the list of such equations for each
subexpression.

-}

import AST.Common.Literal as Literal
import AST.Common.Located as Located
import AST.Common.Type as Type
import AST.Typed as Typed
import AssocList as Dict
import Data.VarName exposing (VarName)
import Stage.InferTypes.IdSource as IdSource exposing (IdSource)
import Stage.InferTypes.TypeEquation exposing (TypeEquation, equals)
import Transform


generateEquations : IdSource -> Typed.LocatedExpr -> ( List TypeEquation, IdSource )
generateEquations idSource located =
    let
        ( expr, type_ ) =
            Located.unwrap located
    in
    case expr of
        Typed.Literal (Literal.Int _) ->
            -- integer is an integer ¯\_(ツ)_/¯
            ( [ equals type_ Type.Int ]
            , idSource
            )

        Typed.Literal (Literal.Float _) ->
            -- float is a float
            ( [ equals type_ Type.Float ]
            , idSource
            )

        Typed.Literal (Literal.Char _) ->
            -- char is a char
            ( [ equals type_ Type.Char ]
            , idSource
            )

        Typed.Literal (Literal.String _) ->
            -- string is a string
            ( [ equals type_ Type.String ]
            , idSource
            )

        Typed.Literal (Literal.Bool _) ->
            -- bool is a bool
            ( [ equals type_ Type.Bool ]
            , idSource
            )

        Typed.Argument _ ->
            -- we can't make any assumptions here
            ( [], idSource )

        Typed.Var _ ->
            -- we can't make any assumptions here
            ( [], idSource )

        Typed.Plus left right ->
            let
                ( _, leftType ) =
                    Located.unwrap left

                ( _, rightType ) =
                    Located.unwrap right

                ( leftEquations, idSource1 ) =
                    generateEquations idSource left

                ( rightEquations, idSource2 ) =
                    generateEquations idSource1 right
            in
            ( -- for expression `a + b`:
              [ equals leftType Type.Int -- type of `a` is Int
              , equals rightType Type.Int -- type of `b` is Int
              , equals type_ Type.Int -- type of `a + b` is Int
              ]
                ++ leftEquations
                ++ rightEquations
            , idSource2
            )

        Typed.Cons left right ->
            let
                ( _, leftType ) =
                    Located.unwrap left

                ( _, rightType ) =
                    Located.unwrap right

                ( leftEquations, idSource1 ) =
                    generateEquations idSource left

                ( rightEquations, idSource2 ) =
                    generateEquations idSource1 right
            in
            ( -- For expression a :: [ b ]:
              [ equals rightType (Type.List leftType) -- type of b is a List a
              , equals type_ rightType -- a :: [ b ] is a List b
              ]
                ++ leftEquations
                ++ rightEquations
            , idSource2
            )

        Typed.Lambda { body, argument } ->
            let
                ( _, bodyType ) =
                    Located.unwrap body

                ( argumentId, idSource1 ) =
                    IdSource.increment idSource

                ( bodyEquations, idSource2 ) =
                    generateEquations idSource1 body

                usages =
                    findArgumentUsages argument body

                usageEquations =
                    generateArgumentUsageEquations argumentId usages
            in
            ( -- type of `\arg -> body` is (arg -> body)
              equals type_ (Type.Function (Type.Var argumentId) bodyType)
                -- type of the argument is the same as the type of all the children usages of that argument
                :: usageEquations
                ++ bodyEquations
            , idSource2
            )

        Typed.Call { fn, argument } ->
            let
                ( _, fnType ) =
                    Located.unwrap fn

                ( _, argumentType ) =
                    Located.unwrap argument

                ( fnEquations, idSource1 ) =
                    generateEquations idSource fn

                ( argumentEquations, idSource2 ) =
                    generateEquations idSource1 argument
            in
            ( -- for expression `a b`:
              -- type of `a` is (argumentType -> resultType)
              equals fnType (Type.Function argumentType type_)
                :: fnEquations
                ++ argumentEquations
            , idSource2
            )

        Typed.If { test, then_, else_ } ->
            let
                ( _, testType ) =
                    Located.unwrap test

                ( _, thenType ) =
                    Located.unwrap then_

                ( _, elseType ) =
                    Located.unwrap else_

                ( testEquations, idSource1 ) =
                    generateEquations idSource test

                ( thenEquations, idSource2 ) =
                    generateEquations idSource1 then_

                ( elseEquations, idSource3 ) =
                    generateEquations idSource2 else_
            in
            ( -- for expression `if a then b else c`:
              [ equals testType Type.Bool -- type of `a` is Bool
              , equals thenType elseType -- types of `b` and `c` are the same
              , equals thenType type_ -- types of `b` and `if a then b else c` are the same
              ]
                ++ testEquations
                ++ thenEquations
                ++ elseEquations
            , idSource3
            )

        Typed.Let { bindings, body } ->
            let
                ( _, bodyType ) =
                    Located.unwrap body

                ( bodyEquations, idSource1 ) =
                    generateEquations idSource body

                ( bindingEquations, idSource2 ) =
                    List.foldl
                        (\binding ( acc, currentIdSource ) ->
                            let
                                ( equations, nextIdSource ) =
                                    generateEquations currentIdSource binding.body
                            in
                            ( equations ++ acc
                            , nextIdSource
                            )
                        )
                        ( [], idSource1 )
                        (Dict.values bindings)
            in
            ( -- for expression `let x = a, y = b in c` (pardon the comma):
              -- type of the whole let and type of `c` are the same
              equals bodyType type_
                :: bodyEquations
                ++ bindingEquations
            , idSource2
            )

        Typed.Unit ->
            -- unit is unit
            ( [ equals type_ Type.Unit ]
            , idSource
            )

        Typed.List items ->
            let
                ( listParamId, idSource1 ) =
                    IdSource.increment idSource

                listParamType =
                    Type.Var listParamId

                ( bodyEquations, idSource2 ) =
                    List.foldr
                        (\item ( acc, currentIdSource ) ->
                            let
                                ( _, itemType ) =
                                    Located.unwrap item

                                ( equations, nextIdSource ) =
                                    generateEquations currentIdSource item
                            in
                            ( equals itemType listParamType
                                :: equations
                                ++ acc
                            , nextIdSource
                            )
                        )
                        ( [], idSource1 )
                        items
            in
            ( -- for expression `[ a, b, c ]`
              -- the `x` in `List x` type and types of all the items are the same
              equals type_ (Type.List listParamType)
                :: bodyEquations
            , idSource2
            )

        Typed.Tuple fst snd ->
            let
                ( _, fstType ) =
                    Located.unwrap fst

                ( _, sndType ) =
                    Located.unwrap snd

                ( fstEquations, idSource1 ) =
                    generateEquations idSource fst

                ( sndEquations, idSource2 ) =
                    generateEquations idSource1 snd
            in
            ( equals type_ (Type.Tuple fstType sndType)
                :: fstEquations
                ++ sndEquations
            , idSource2
            )

        Typed.Tuple3 fst snd trd ->
            let
                ( _, fstType ) =
                    Located.unwrap fst

                ( _, sndType ) =
                    Located.unwrap snd

                ( _, trdType ) =
                    Located.unwrap trd

                ( fstEquations, idSource1 ) =
                    generateEquations idSource fst

                ( sndEquations, idSource2 ) =
                    generateEquations idSource1 snd

                ( trdEquations, idSource3 ) =
                    generateEquations idSource2 trd
            in
            ( equals type_ (Type.Tuple3 fstType sndType trdType)
                :: fstEquations
                ++ sndEquations
                ++ trdEquations
            , idSource3
            )


findArgumentUsages : VarName -> Typed.LocatedExpr -> List Typed.LocatedExpr
findArgumentUsages argument bodyExpr =
    bodyExpr
        |> Transform.children Typed.recursiveChildren
        |> List.filter (Typed.isArgument argument)


generateArgumentUsageEquations : Int -> List Typed.LocatedExpr -> List TypeEquation
generateArgumentUsageEquations argumentId usages =
    let
        argumentType =
            Type.Var argumentId
    in
    List.map
        (Typed.getType >> equals argumentType)
        usages
