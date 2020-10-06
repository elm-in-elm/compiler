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

import Dict
import Elm.AST.Typed as Typed
import Elm.Data.Located as Located
import Elm.Data.Operator as Operator exposing (Operator)
import Elm.Data.Qualifiedness exposing (Qualified)
import Elm.Data.Type as Type exposing (Type(..), TypeOrId(..))
import Elm.Data.VarName exposing (VarName)
import Stage.InferTypes.TypeEquation exposing (TypeEquation, equals)
import Transform


{-| The ID returned here is usually just passed over from the recursive
`generateEquations` calls. Rarely do you need to actually increment the number
yourself.

But there are such cases - see eg. List or Lambda. It is because of them that we
need to pass the ID as one of the return values.

-}
generateEquations : Int -> Typed.LocatedExpr -> ( List TypeEquation, Int )
generateEquations currentId located =
    let
        ( expr, type_ ) =
            Located.unwrap located
    in
    case expr of
        Typed.Int _ ->
            -- integer is an integer ¯\_(ツ)_/¯
            ( [ equals type_ (Type Int) ]
            , currentId
            )

        Typed.Float _ ->
            -- float is a float
            ( [ equals type_ (Type Float) ]
            , currentId
            )

        Typed.Char _ ->
            -- char is a char
            ( [ equals type_ (Type Char) ]
            , currentId
            )

        Typed.String _ ->
            -- string is a string
            ( [ equals type_ (Type String) ]
            , currentId
            )

        Typed.Bool _ ->
            -- bool is a bool
            ( [ equals type_ (Type Bool) ]
            , currentId
            )

        Typed.Argument _ ->
            -- we can't make any assumptions here
            ( [], currentId )

        Typed.Var _ ->
            -- we can't make any assumptions here
            ( [], currentId )

        Typed.Operator op left right ->
            let
                ( _, leftType ) =
                    Located.unwrap left

                ( _, rightType ) =
                    Located.unwrap right

                ( leftEquations, id1 ) =
                    generateEquations currentId left

                ( rightEquations, id2 ) =
                    generateEquations id1 right

                opEquations =
                    case op of
                        Operator.Add ->
                            [ equals leftType (Type Int) -- type of `a` is Int
                            , equals rightType (Type Int) -- type of `b` is Int
                            , equals type_ (Type Int) -- type of `a + b` is Int
                            ]

                        Operator.Cons ->
                            [ equals rightType (Type (List leftType)) -- type of b is a List a
                            , equals type_ rightType -- a :: [ b ] is a List b
                            ]

                        _ ->
                            Debug.todo "handle typing of other operators"
            in
            ( opEquations
                -- for expression `a OP b`:
                ++ leftEquations
                ++ rightEquations
            , id2
            )

        Typed.Lambda { body, argument } ->
            {- We need to increment the ID here because lambda arguments didn't
               get their IDs in the AssignIds phase (Strings aren't Exprs).

               If we didn't increment the ID, the generated equations could
               mix up the different arguments in different lambdas. For example:

               \a b -> a + b

               (which is, after desugaring, \a -> (\b -> a + b))

               would generate (among other equations) something like:

               "a" argument == Var 5
               "b" argument == Var 5

               Which isn't correct. So we increment the ID here!
            -}
            let
                argumentId =
                    currentId

                id1 =
                    currentId + 1

                ( _, bodyType ) =
                    Located.unwrap body

                ( bodyEquations, id2 ) =
                    generateEquations id1 body

                usages =
                    findArgumentUsages argument body

                usageEquations =
                    generateArgumentUsageEquations argumentId usages
            in
            ( -- type of `\arg -> body` is (arg -> body)
              equals type_ (Type (Function { from = Id currentId, to = bodyType }))
                -- type of the argument is the same as the type of all the children usages of that argument
                :: usageEquations
                ++ bodyEquations
            , id2
            )

        Typed.Call { fn, argument } ->
            let
                ( _, fnType ) =
                    Located.unwrap fn

                ( _, argumentType ) =
                    Located.unwrap argument

                ( fnEquations, id1 ) =
                    generateEquations currentId fn

                ( argumentEquations, id2 ) =
                    generateEquations id1 argument
            in
            ( -- for expression `a b`:
              -- type of `a` is (argumentType -> resultType)
              equals fnType (Type (Function { from = argumentType, to = type_ }))
                :: fnEquations
                ++ argumentEquations
            , id2
            )

        Typed.If { test, then_, else_ } ->
            let
                ( _, testType ) =
                    Located.unwrap test

                ( _, thenType ) =
                    Located.unwrap then_

                ( _, elseType ) =
                    Located.unwrap else_

                ( testEquations, id1 ) =
                    generateEquations currentId test

                ( thenEquations, id2 ) =
                    generateEquations id1 then_

                ( elseEquations, id3 ) =
                    generateEquations id2 else_
            in
            ( -- for expression `if a then b else c`:
              [ equals testType (Type Bool) -- type of `a` is Bool
              , equals thenType elseType -- types of `b` and `c` are the same
              , equals thenType type_ -- types of `b` and `if a then b else c` are the same
              ]
                ++ testEquations
                ++ thenEquations
                ++ elseEquations
            , id3
            )

        Typed.Let { bindings, body } ->
            let
                ( _, bodyType ) =
                    Located.unwrap body

                ( bodyEquations, id1 ) =
                    generateEquations currentId body

                ( bindingEquations, id2 ) =
                    List.foldl
                        (\binding ( acc, currentId_ ) ->
                            let
                                ( equations, nextId ) =
                                    generateEquations currentId_ binding.body
                            in
                            ( equations ++ acc
                            , nextId
                            )
                        )
                        ( [], id1 )
                        (Dict.values bindings)
            in
            ( -- for expression `let x = a, y = b in c` (pardon the comma):
              -- type of the whole let and type of `c` are the same
              equals bodyType type_
                :: bodyEquations
                ++ bindingEquations
            , id2
            )

        Typed.Unit ->
            -- unit is unit
            ( [ equals type_ (Type Unit) ]
            , currentId
            )

        Typed.List items ->
            {- The list type parameter needs extra ID so that we can
               bind the items' types to it... so we create one here.
            -}
            let
                id1 =
                    currentId + 1

                listParamType =
                    Id currentId

                ( bodyEquations, id2 ) =
                    List.foldr
                        (\item ( acc, currentId_ ) ->
                            let
                                ( _, itemType ) =
                                    Located.unwrap item

                                ( equations, nextId ) =
                                    generateEquations currentId_ item
                            in
                            ( equals itemType listParamType
                                :: equations
                                ++ acc
                            , nextId
                            )
                        )
                        ( [], id1 )
                        items
            in
            ( -- for expression `[ a, b, c ]`
              -- the `x` in `List x` type and types of all the items are the same
              equals type_ (Type (List listParamType))
                :: bodyEquations
            , id2
            )

        Typed.Tuple first second ->
            let
                ( _, firstType ) =
                    Located.unwrap first

                ( _, secondType ) =
                    Located.unwrap second

                ( firstEquations, id1 ) =
                    generateEquations currentId first

                ( secondEquations, id2 ) =
                    generateEquations id1 second
            in
            ( equals type_ (Type (Tuple firstType secondType))
                :: firstEquations
                ++ secondEquations
            , id2
            )

        Typed.Tuple3 first second third ->
            let
                ( _, firstType ) =
                    Located.unwrap first

                ( _, secondType ) =
                    Located.unwrap second

                ( _, thirdType ) =
                    Located.unwrap third

                ( firstEquations, id1 ) =
                    generateEquations currentId first

                ( secondEquations, id2 ) =
                    generateEquations id1 second

                ( thirdEquations, id3 ) =
                    generateEquations id2 third
            in
            ( equals type_ (Type (Tuple3 firstType secondType thirdType))
                :: firstEquations
                ++ secondEquations
                ++ thirdEquations
            , id3
            )

        Typed.Record bindings ->
            let
                bindingTypes =
                    Dict.map
                        (\_ binding ->
                            Located.unwrap binding.body |> Tuple.second
                        )
                        bindings

                ( bindingEquations, id1 ) =
                    List.foldl
                        (\binding ( acc, currentId_ ) ->
                            let
                                ( equations, nextId ) =
                                    generateEquations currentId_ binding.body
                            in
                            ( equations ++ acc
                            , nextId
                            )
                        )
                        ( [], currentId )
                        (Dict.values bindings)
            in
            ( equals type_ (Type (Record bindingTypes))
                :: bindingEquations
            , id1
            )

        Typed.Case test branches ->
            let
                ( _, testType ) =
                    Located.unwrap test

                ( testEquations, id1 ) =
                    generateEquations currentId test

                ( branchesEquations, newId ) =
                    List.foldl
                        (\{ pattern, body } ( acc, currentId_ ) ->
                            let
                                ( _, patternType ) =
                                    Located.unwrap pattern

                                ( _, bodyType ) =
                                    Located.unwrap body

                                ( patternEquations, bodyId ) =
                                    generatePatternEquations currentId_ pattern

                                ( bodyEquations, nextId ) =
                                    generateEquations bodyId body
                            in
                            ( acc
                                --pattern type must be equal the test type
                                ++ [ equals testType patternType

                                   -- bodies types must be equal the overall type
                                   , equals type_ bodyType
                                   ]
                                ++ patternEquations
                                ++ bodyEquations
                            , nextId
                            )
                        )
                        ( [], id1 )
                        branches
            in
            ( testEquations ++ branchesEquations
            , newId
            )


findArgumentUsages : VarName -> Typed.LocatedExpr -> List Typed.LocatedExpr
findArgumentUsages argument bodyExpr =
    bodyExpr
        |> Transform.children Typed.recursiveChildren
        |> List.filter (isArgument argument)


isArgument : VarName -> Typed.LocatedExpr -> Bool
isArgument name locatedExpr =
    case Typed.getExpr locatedExpr of
        Typed.Argument argName ->
            argName == name

        _ ->
            False


generateArgumentUsageEquations : Int -> List Typed.LocatedExpr -> List TypeEquation
generateArgumentUsageEquations argumentId usages =
    let
        argumentType : TypeOrId Qualified
        argumentType =
            Type.Id argumentId
    in
    List.map
        (Typed.getTypeOrId >> equals argumentType)
        usages


generatePatternEquations : Int -> Typed.LocatedPattern -> ( List TypeEquation, Int )
generatePatternEquations currentId located =
    let
        ( pattern, type_ ) =
            Located.unwrap located
    in
    case pattern of
        Typed.PAnything ->
            -- we can't make any assumptions here
            ( [], currentId )

        Typed.PVar _ ->
            -- we can't make any assumptions here
            ( [], currentId )

        Typed.PRecord _ ->
            {- TODO:
               We know it's a record and have at least these keys.

               Do something like Typed.Lambda's argument for each key?

               Which is:
               Assign a new ID to each key because it didn't
               get one in the AssignIds phase (Strings aren't Exprs).

               Then in case of `Case` expr, check this branch's body for
               usage of the key name and generate equals equations?
            -}
            ( [], currentId )

        Typed.PAlias aliasPattern _ ->
            {- TODO:
               aliasPattern and aliasName have the same type.

               Do something like Typed.Lambda's argument for the aliasName?

            -}
            let
                _ =
                    Located.unwrap aliasPattern

                ( aliasPatternEquations, id1 ) =
                    generatePatternEquations currentId aliasPattern
            in
            ( aliasPatternEquations
            , id1
            )

        Typed.PUnit ->
            -- unit is unit
            ( [ equals type_ (Type Type.Unit) ]
            , currentId
            )

        Typed.PTuple first second ->
            let
                ( _, firstType ) =
                    Located.unwrap first

                ( _, secondType ) =
                    Located.unwrap second

                ( firstEquations, id1 ) =
                    generatePatternEquations currentId first

                ( secondEquations, id2 ) =
                    generatePatternEquations id1 second
            in
            ( equals type_ (Type (Tuple firstType secondType))
                :: firstEquations
                ++ secondEquations
            , id2
            )

        Typed.PTuple3 first second third ->
            let
                ( _, firstType ) =
                    Located.unwrap first

                ( _, secondType ) =
                    Located.unwrap second

                ( _, thirdType ) =
                    Located.unwrap third

                ( firstEquations, id1 ) =
                    generatePatternEquations currentId first

                ( secondEquations, id2 ) =
                    generatePatternEquations id1 second

                ( thirdEquations, id3 ) =
                    generatePatternEquations id2 third
            in
            ( equals type_ (Type (Tuple3 firstType secondType thirdType))
                :: firstEquations
                ++ secondEquations
                ++ thirdEquations
            , id3
            )

        Typed.PList items ->
            {- The list type parameter needs extra ID so that we can
               bind the items' types to it... so we create one here.
            -}
            let
                id1 =
                    currentId + 1

                listParamType =
                    Id currentId

                ( bodyEquations, id2 ) =
                    List.foldr
                        (\item ( acc, currentId_ ) ->
                            let
                                ( _, itemType ) =
                                    Located.unwrap item

                                ( equations, nextId ) =
                                    generatePatternEquations currentId_ item
                            in
                            ( equals itemType listParamType
                                :: equations
                                ++ acc
                            , nextId
                            )
                        )
                        ( [], id1 )
                        items
            in
            ( -- for expression `[ a, b, c ]`
              -- the `x` in `List x` type and types of all the items are the same
              equals type_ (Type (List listParamType))
                :: bodyEquations
            , id2
            )

        Typed.PCons left right ->
            let
                ( _, leftType ) =
                    Located.unwrap left

                ( _, rightType ) =
                    Located.unwrap right

                ( leftEquations, id1 ) =
                    generatePatternEquations currentId left

                ( rightEquations, id2 ) =
                    generatePatternEquations id1 right
            in
            ( -- For expression a :: b:
              [ equals rightType (Type (List leftType)) -- type of `b` is a `List a`
              , equals type_ rightType -- type of `a :: b` is `List a`
              ]
                ++ leftEquations
                ++ rightEquations
            , id2
            )

        Typed.PBool _ ->
            ( [ equals type_ (Type Bool) ]
            , currentId
            )

        Typed.PChar _ ->
            ( [ equals type_ (Type Char) ]
            , currentId
            )

        Typed.PString _ ->
            ( [ equals type_ (Type String) ]
            , currentId
            )

        Typed.PInt _ ->
            ( [ equals type_ (Type Int) ]
            , currentId
            )

        Typed.PFloat _ ->
            ( [ equals type_ (Type Float) ]
            , currentId
            )
