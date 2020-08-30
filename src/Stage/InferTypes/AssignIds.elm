module Stage.InferTypes.AssignIds exposing (assignIds)

{-| Stage 1

Gives every subexpression an unique auto-incremented ID and converts
from Canonical.Expr to Typed.Expr.

Example:

Input (forgive the nonsense AST):

    Canonical.If
        { test =
            Canonical.Plus
                (Canonical.Int 1)
                (Canonical.Int 2)
        , then_ = Canonical.Unit
        , else_ = Canonical.Bool True
        }

Output:

    ( Typed.If
        { test =
            ( Typed.Plus
                ( Typed.Int 1
                , Var 0
                )
                ( Typed.Int 2
                , Var 1
                )
            , Var 2
            )
        , then_ = ( Typed.Unit, Var 3 )
        , else_ = ( Typed.Bool True, Var 4 )
        }
    , Var 5
    )

-}

import Dict
import Elm.AST.Canonical as Canonical
import Elm.AST.Typed as Typed
import Elm.Data.Located as Located
import Elm.Data.Qualifiedness exposing (Qualified)
import Elm.Data.Type as Type exposing (TypeOrId)


assignIds : Int -> Canonical.LocatedExpr -> ( Typed.LocatedExpr, Int )
assignIds currentId locatedCanonicalExpr =
    let
        ( typedExpr, newId ) =
            assignIdsHelp currentId (Located.unwrap locatedCanonicalExpr)
    in
    {- Keep location, for error context -}
    ( Located.replaceWith typedExpr locatedCanonicalExpr
    , newId
    )


assignId : Int -> a -> ( ( a, TypeOrId Qualified ), Int )
assignId currentId located =
    ( ( located, Type.Id currentId ), currentId + 1 )


assignIdsHelp : Int -> Canonical.Expr -> ( Typed.Expr, Int )
assignIdsHelp currentId located =
    let
        f =
            assignIds
    in
    {- Be careful when dealing with the ids, they all have to be distinct.
       Enable the "unused variable" warning from elm-analyze may help you
       to detect created but unused ids.
    -}
    case located of
        {- With literals, we could plug their final type in right here
           (no solving needed!) but let's be uniform and do everything through
           the constraint solver in stages 2 and 3.
        -}
        Canonical.Int int ->
            assignId currentId (Typed.Int int)

        Canonical.Float float ->
            assignId currentId (Typed.Float float)

        Canonical.Char char ->
            assignId currentId (Typed.Char char)

        Canonical.String string ->
            assignId currentId (Typed.String string)

        Canonical.Bool bool ->
            assignId currentId (Typed.Bool bool)

        -- We remember argument's IDs so that we can later use them in Lambda
        Canonical.Argument name ->
            assignId currentId (Typed.Argument name)

        Canonical.Var name ->
            assignId currentId (Typed.Var name)

        Canonical.Plus e1 e2 ->
            let
                ( e1_, id1 ) =
                    f currentId e1

                ( e2_, id2 ) =
                    f id1 e2
            in
            assignId id2 (Typed.Plus e1_ e2_)

        Canonical.Cons e1 e2 ->
            let
                ( e1_, id1 ) =
                    f currentId e1

                ( e2_, id2 ) =
                    f id1 e2
            in
            assignId id2 (Typed.Cons e1_ e2_)

        Canonical.Lambda { argument, body } ->
            let
                ( body_, id1 ) =
                    f currentId body
            in
            assignId id1
                (Typed.Lambda
                    { argument = argument
                    , body =
                        body_
                    }
                )

        Canonical.Call { fn, argument } ->
            let
                ( fn_, id1 ) =
                    f currentId fn

                ( argument_, id2 ) =
                    f id1 argument
            in
            assignId id2
                (Typed.Call
                    { fn = fn_
                    , argument = argument_
                    }
                )

        Canonical.If { test, then_, else_ } ->
            let
                ( test_, id1 ) =
                    f currentId test

                ( then__, id2 ) =
                    f id1 then_

                ( else__, id3 ) =
                    f id2 else_
            in
            assignId id3
                (Typed.If
                    { test = test_
                    , then_ = then__
                    , else_ = else__
                    }
                )

        Canonical.Let { bindings, body } ->
            {- We don't thread the full (VarName, Binding) thing to Id
               as that would bloat the type signatures of IdGenerator too much.

               We unwrap the exprs from the bindings and then carefully put them
               back together in the same order.
            -}
            let
                bindingsList =
                    Dict.toList bindings

                ( body_, id1 ) =
                    f currentId body

                ( bindingBodiesList, id2 ) =
                    List.foldl
                        (\( name, binding ) ( acc, runningId ) ->
                            let
                                ( body__, nextId ) =
                                    f runningId binding.body

                                newElt =
                                    ( name, { name = name, body = body__ } )
                            in
                            ( newElt :: acc
                            , nextId
                            )
                        )
                        ( [], id1 )
                        bindingsList
            in
            assignId id2
                (Typed.Let
                    { bindings =
                        Dict.fromList bindingBodiesList
                    , body = body_
                    }
                )

        Canonical.Unit ->
            assignId currentId Typed.Unit

        Canonical.List items ->
            let
                ( items_, newId ) =
                    List.foldr
                        (\item ( acc, runningId ) ->
                            let
                                ( item_, nextId ) =
                                    f runningId item
                            in
                            ( item_ :: acc
                            , nextId
                            )
                        )
                        ( [], currentId )
                        items
            in
            assignId newId (Typed.List items_)

        Canonical.Tuple e1 e2 ->
            let
                ( e1_, id1 ) =
                    f currentId e1

                ( e2_, id2 ) =
                    f id1 e2
            in
            assignId id2 (Typed.Tuple e1_ e2_)

        Canonical.Tuple3 e1 e2 e3 ->
            let
                ( e1_, id1 ) =
                    f currentId e1

                ( e2_, id2 ) =
                    f id1 e2

                ( e3_, id3 ) =
                    f id2 e3
            in
            assignId id3 (Typed.Tuple3 e1_ e2_ e3_)

        Canonical.Record bindings ->
            let
                bindingsList =
                    Dict.toList bindings

                ( bindingBodiesList, newId ) =
                    List.foldl
                        (\( name, binding ) ( acc, runningId ) ->
                            let
                                ( body__, nextId ) =
                                    f runningId binding.body

                                newElt =
                                    ( name, { name = name, body = body__ } )
                            in
                            ( newElt :: acc
                            , nextId
                            )
                        )
                        ( [], currentId )
                        bindingsList
            in
            assignId newId <|
                Typed.Record (Dict.fromList bindingBodiesList)

        Canonical.Case e branches ->
            let
                ( e_, id1 ) =
                    f currentId e

                ( branches_, newId ) =
                    List.foldr
                        (\{ pattern, body } ( acc, runningId ) ->
                            let
                                ( typedPattern, bodyId ) =
                                    assignPatternIds runningId pattern

                                ( typedBody, nextId ) =
                                    f bodyId body
                            in
                            ( { pattern = typedPattern
                              , body = typedBody
                              }
                                :: acc
                            , nextId
                            )
                        )
                        ( [], id1 )
                        branches
            in
            assignId newId <|
                Typed.Case e_ branches_

        Canonical.Shader shader ->
            assignId currentId (Typed.Shader shader)


assignPatternIds : Int -> Canonical.LocatedPattern -> ( Typed.LocatedPattern, Int )
assignPatternIds currentId locatedCanonicalPattern =
    let
        ( typedPattern, newId ) =
            assignPatternIdsHelp currentId (Located.unwrap locatedCanonicalPattern)
    in
    {- Keep location, for error context -}
    ( Located.replaceWith typedPattern locatedCanonicalPattern
    , newId
    )


assignPatternIdsHelp : Int -> Canonical.Pattern -> ( Typed.Pattern, Int )
assignPatternIdsHelp currentId located =
    let
        f =
            assignPatternIds
    in
    case located of
        Canonical.PAnything ->
            assignId currentId Typed.PAnything

        Canonical.PVar varName ->
            assignId currentId (Typed.PVar varName)

        Canonical.PRecord varNames ->
            assignId currentId (Typed.PRecord varNames)

        Canonical.PAlias pattern varName ->
            let
                ( pattern_, id1 ) =
                    f currentId pattern
            in
            assignId id1 (Typed.PAlias pattern_ varName)

        Canonical.PUnit ->
            assignId currentId Typed.PUnit

        Canonical.PTuple pattern1 pattern2 ->
            let
                ( pattern1_, id1 ) =
                    f currentId pattern1

                ( pattern2_, id2 ) =
                    f id1 pattern2
            in
            assignId id2 (Typed.PTuple pattern1_ pattern2_)

        Canonical.PTuple3 pattern1 pattern2 pattern3 ->
            let
                ( pattern1_, id1 ) =
                    f currentId pattern1

                ( pattern2_, id2 ) =
                    f id1 pattern2

                ( pattern3_, id3 ) =
                    f id2 pattern3
            in
            assignId id3 (Typed.PTuple3 pattern1_ pattern2_ pattern3_)

        Canonical.PList items ->
            let
                ( items_, newId ) =
                    List.foldr
                        (\item ( acc, runningId ) ->
                            let
                                ( item_, nextId ) =
                                    f runningId item
                            in
                            ( item_ :: acc
                            , nextId
                            )
                        )
                        ( [], currentId )
                        items
            in
            assignId newId (Typed.PList items_)

        Canonical.PCons pattern1 pattern2 ->
            let
                ( pattern1_, id1 ) =
                    f currentId pattern1

                ( pattern2_, id2 ) =
                    f id1 pattern2
            in
            assignId id2 (Typed.PCons pattern1_ pattern2_)

        Canonical.PBool bool ->
            assignId currentId (Typed.PBool bool)

        Canonical.PChar char ->
            assignId currentId (Typed.PChar char)

        Canonical.PString string ->
            assignId currentId (Typed.PString string)

        Canonical.PInt int ->
            assignId currentId (Typed.PInt int)

        Canonical.PFloat float ->
            assignId currentId (Typed.PFloat float)
