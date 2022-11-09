module Stage.InferTypes.GenerateEquations exposing
    ( generateEquationsAcrossDeclarations
    , generateLocalEquations
    )

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
import Elm.Data.Qualifiedness exposing (Qualified)
import Elm.Data.Type as Type exposing (Id, Type(..), TypeOrId(..))
import Elm.Data.VarName exposing (VarName)
import List.NonEmpty
import OurExtras.List as List
import Stage.InferTypes.Environment as Env exposing (Environment)
import Stage.InferTypes.TypeEquation exposing (TypeEquation, equals)
import Transform


{-| The ID returned here is usually just passed over from the recursive
`generateLocalEquations` calls. Rarely do you need to actually increment the number
yourself.

But there are such cases - see eg. List or Lambda. It is because of them that we
need to pass the ID as one of the return values.

---

We're passing in the "environment" of used vars and what we've learned about them
from their usage. In a later step we'll connect all these together to hopefully
arrive at some better conclusions than can be found locally.

-}
generateLocalEquations : Id -> Environment -> Typed.LocatedExpr -> ( Id, Environment, List TypeEquation )
generateLocalEquations currentId env located =
    let
        ( expr, type_ ) =
            Located.unwrap located
    in
    case expr of
        Typed.Int _ ->
            -- integer is an integer ¯\_(ツ)_/¯
            ( currentId
            , env
            , [ equals type_ (Type Int) ]
            )

        Typed.Float _ ->
            -- float is a float
            ( currentId
            , env
            , [ equals type_ (Type Float) ]
            )

        Typed.Char _ ->
            -- char is a char
            ( currentId
            , env
            , [ equals type_ (Type Char) ]
            )

        Typed.String _ ->
            -- string is a string
            ( currentId
            , env
            , [ equals type_ (Type String) ]
            )

        Typed.Bool _ ->
            -- bool is a bool
            ( currentId
            , env
            , [ equals type_ (Type Bool) ]
            )

        Typed.Argument _ ->
            -- TODO we need to make sure argument usages use their inferred (from function) types
            ( currentId
            , env
            , []
            )

        Typed.Var name ->
            {- We can't learn much here, but we can note in our environment what
               ID have we given to the specific (ModuleName, VarName) in the
               current expression.

               When we later assign IDs to definitions, it will allow us to link
               them together.
            -}
            ( currentId
            , env |> Env.add name type_
            , []
            )

        Typed.BinOp op left right ->
            let
                ( id1, env1, leftEquations ) =
                    generateLocalEquations currentId env left

                ( id2, env2, rightEquations ) =
                    generateLocalEquations id1 env1 right
            in
            ( id2
            , env2
            , [-- TODO the operator should be linked to its definition
               -- TODO the binop is a function: left -> right -> result
              ]
                ++ leftEquations
                ++ rightEquations
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

                ( id2, env1, bodyEquations ) =
                    generateLocalEquations id1 env body

                usages =
                    findArgumentUsages argument body

                usageEquations =
                    generateArgumentUsageEquations argumentId usages
            in
            ( id2
            , env1
            , -- type of `\arg -> body` is (arg -> body)
              equals type_ (Type (Function { from = Id currentId, to = bodyType }))
                -- type of the argument is the same as the type of all the children usages of that argument
                :: usageEquations
                ++ bodyEquations
            )

        Typed.Call { fn, argument } ->
            let
                ( _, fnType ) =
                    Located.unwrap fn

                ( _, argumentType ) =
                    Located.unwrap argument

                ( id1, env1, fnEquations ) =
                    generateLocalEquations currentId env fn

                ( id2, env2, argumentEquations ) =
                    generateLocalEquations id1 env1 argument
            in
            ( id2
            , env2
            , -- for expression `a b`:
              -- type of `a` is (argumentType -> resultType)
              equals fnType (Type (Function { from = argumentType, to = type_ }))
                :: fnEquations
                ++ argumentEquations
            )

        Typed.If { test, then_, else_ } ->
            let
                ( _, testType ) =
                    Located.unwrap test

                ( _, thenType ) =
                    Located.unwrap then_

                ( _, elseType ) =
                    Located.unwrap else_

                ( id1, env1, testEquations ) =
                    generateLocalEquations currentId env test

                ( id2, env2, thenEquations ) =
                    generateLocalEquations id1 env1 then_

                ( id3, env3, elseEquations ) =
                    generateLocalEquations id2 env2 else_
            in
            ( id3
            , env3
            , -- for expression `if a then b else c`:
              [ equals testType (Type Bool) -- type of `a` is Bool
              , equals thenType elseType -- types of `b` and `c` are the same
              , equals thenType type_ -- types of `b` and `if a then b else c` are the same
              ]
                ++ testEquations
                ++ thenEquations
                ++ elseEquations
            )

        Typed.Let { bindings, body } ->
            let
                ( _, bodyType ) =
                    Located.unwrap body

                ( id1, env1, bodyEquations ) =
                    generateLocalEquations currentId env body

                ( id2, env2, bindingEquations ) =
                    List.foldl
                        (\binding ( currentId_, currentEnv, acc ) ->
                            let
                                ( nextId, nextEnv, equations ) =
                                    generateLocalEquations currentId_ currentEnv binding.body
                            in
                            ( nextId
                            , nextEnv
                            , equations ++ acc
                            )
                        )
                        ( id1, env1, [] )
                        (Dict.values bindings)
            in
            ( id2
            , env2
            , -- for expression `let x = a, y = b in c` (pardon the comma):
              -- type of the whole let and type of `c` are the same
              equals bodyType type_
                :: bodyEquations
                ++ bindingEquations
            )

        Typed.Unit ->
            -- unit is unit
            ( currentId
            , env
            , [ equals type_ (Type Unit) ]
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

                ( id2, env1, bodyEquations ) =
                    List.foldr
                        (\item ( currentId_, currentEnv, acc ) ->
                            let
                                ( _, itemType ) =
                                    Located.unwrap item

                                ( nextId, nextEnv, equations ) =
                                    generateLocalEquations currentId_ currentEnv item
                            in
                            ( nextId
                            , nextEnv
                            , equals itemType listParamType
                                :: equations
                                ++ acc
                            )
                        )
                        ( id1, env, [] )
                        items
            in
            ( id2
            , env1
            , -- for expression `[ a, b, c ]`
              -- the `x` in `List x` type and types of all the items are the same
              equals type_ (Type (List listParamType))
                :: bodyEquations
            )

        Typed.Tuple first second ->
            let
                ( _, firstType ) =
                    Located.unwrap first

                ( _, secondType ) =
                    Located.unwrap second

                ( id1, env1, firstEquations ) =
                    generateLocalEquations currentId env first

                ( id2, env2, secondEquations ) =
                    generateLocalEquations id1 env1 second
            in
            ( id2
            , env2
            , equals type_ (Type (Tuple firstType secondType))
                :: firstEquations
                ++ secondEquations
            )

        Typed.Tuple3 first second third ->
            let
                ( _, firstType ) =
                    Located.unwrap first

                ( _, secondType ) =
                    Located.unwrap second

                ( _, thirdType ) =
                    Located.unwrap third

                ( id1, env1, firstEquations ) =
                    generateLocalEquations currentId env first

                ( id2, env2, secondEquations ) =
                    generateLocalEquations id1 env1 second

                ( id3, env3, thirdEquations ) =
                    generateLocalEquations id2 env2 third
            in
            ( id3
            , env3
            , equals type_ (Type (Tuple3 firstType secondType thirdType))
                :: firstEquations
                ++ secondEquations
                ++ thirdEquations
            )

        Typed.Record bindings ->
            let
                bindingTypes =
                    Dict.map
                        (\_ binding ->
                            Located.unwrap binding.body |> Tuple.second
                        )
                        bindings

                ( id1, env1, bindingEquations ) =
                    List.foldl
                        (\binding ( currentId_, currentEnv, acc ) ->
                            let
                                ( nextId, nextEnv, equations ) =
                                    generateLocalEquations currentId_ currentEnv binding.body
                            in
                            ( nextId
                            , nextEnv
                            , equations ++ acc
                            )
                        )
                        ( currentId, env, [] )
                        (Dict.values bindings)
            in
            ( id1
            , env1
            , equals type_ (Type (Record bindingTypes))
                :: bindingEquations
            )

        Typed.RecordAccess e field ->
            {- TODO

               State.do State.getNextIdAndTick <| \extensibleRecordId ->
               State.do State.getNextIdAndTick <| \resultId ->
               finish
                   [ ( type_, Type.id resultId, "Record access = the field = the result" )
                   , ( NodeV2.type_ record
                     , Type.mono <|
                           ExtensibleRecord
                               { type_ = Type.id_ extensibleRecordId
                               , fields =
                                   Dict.singleton
                                       (NodeV2.value fieldNameNode)
                                       (Type.id_ resultId)
                               }
                     , "Record access: left is a record"
                     )
                   ]
            -}
            -- TODO make the `e` into a record, make the final expr into its field
            generateLocalEquations currentId env e

        Typed.Case test branches ->
            let
                ( _, testType ) =
                    Located.unwrap test

                ( id1, env1, testEquations ) =
                    generateLocalEquations currentId env test

                ( id2, env2, branchesEquations ) =
                    List.foldl
                        (\{ pattern, body } ( currentId_, currentEnv, acc ) ->
                            let
                                ( _, patternType ) =
                                    Located.unwrap pattern

                                ( _, bodyType ) =
                                    Located.unwrap body

                                ( patternEquations, bodyId ) =
                                    generatePatternEquations currentId_ pattern

                                ( nextId, nextEnv, bodyEquations ) =
                                    generateLocalEquations bodyId currentEnv body
                            in
                            ( nextId
                            , nextEnv
                            , acc
                                --pattern type must be equal the test type
                                ++ [ equals testType patternType

                                   -- bodies types must be equal the overall type
                                   , equals type_ bodyType
                                   ]
                                ++ patternEquations
                                ++ bodyEquations
                            )
                        )
                        ( id1, env1, [] )
                        (List.NonEmpty.toList branches)
            in
            ( id2
            , env2
            , testEquations ++ branchesEquations
            )

        Typed.ConstructorValue _ ->
            -- TODO how to get the type definition and its arguments into scope here?
            -- [ equals type_ thatUserDefinedType ]
            ( currentId
            , env
            , []
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


generateArgumentUsageEquations : Id -> List Typed.LocatedExpr -> List TypeEquation
generateArgumentUsageEquations argumentId usages =
    let
        argumentType : TypeOrId Qualified
        argumentType =
            Type.Id argumentId
    in
    List.map
        (Typed.getTypeOrId >> equals argumentType)
        usages


generatePatternEquations : Id -> Typed.LocatedPattern -> ( List TypeEquation, Id )
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


generateEquationsAcrossDeclarations : Environment -> List TypeEquation
generateEquationsAcrossDeclarations env =
    env
        |> Dict.values
        |> List.fastConcatMap (List.mapConsecutivePairs equals)
