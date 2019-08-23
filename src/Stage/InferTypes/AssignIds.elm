module Stage.InferTypes.AssignIds exposing (assignIds)

{-| Stage 1

Gives every subexpression an unique auto-incremented ID and converts
from Canonical.Expr to Typed.Expr.

Example:

Input (forgive the nonsense AST):

    Canonical.If
        { test =
            Canonical.Plus
                (Canonical.Literal (Int 1))
                (Canonical.Literal (Int 2))
        , then_ = Canonical.Unit
        , else_ = Canonical.Literal (Bool True)
        }

Output:

    ( Typed.If
        { test =
            ( Typed.Plus
                ( Typed.Literal (Int 1)
                , Var 0
                )
                ( Typed.Literal (Int 2)
                , Var 1
                )
            , Var 2
            )
        , then_ = ( Typed.Unit, Var 3 )
        , else_ = ( Typed.Literal (Bool True), Var 4 )
        }
    , Var 5
    )

-}

import AST.Canonical as Canonical
import AST.Common.Located as Located
import AST.Common.Type as Type
import AST.Typed as Typed
import AssocList as Dict
import Stage.InferTypes.IdSource as IdSource exposing (IdSource)


assignIds : Canonical.LocatedExpr -> ( Typed.LocatedExpr, IdSource )
assignIds located =
    assignIdsWith IdSource.empty located


assignIdsWith : IdSource -> Canonical.LocatedExpr -> ( Typed.LocatedExpr, IdSource )
assignIdsWith idSource locatedCanonicalExpr =
    let
        ( typedExpr, idSource_ ) =
            assignIdsWithHelp idSource (Located.unwrap locatedCanonicalExpr)
    in
    {- Keep location, for error context -}
    ( Located.replaceWith typedExpr locatedCanonicalExpr
    , idSource_
    )


assignId : IdSource -> Typed.Expr_ -> ( Typed.Expr, IdSource )
assignId idSource located =
    IdSource.one idSource (\id -> ( located, Type.Var id ))


assignIdsWithHelp : IdSource -> Canonical.Expr -> ( Typed.Expr, IdSource )
assignIdsWithHelp idSource located =
    case located of
        {- With literals, we could plug their final type in right here
           (no solving needed!) but let's be uniform and do everything through
           the constraint solver in stages 2 and 3.
        -}
        Canonical.Literal literal ->
            assignId idSource (Typed.Literal literal)

        -- We remember argument's IDs so that we can later use them in Lambda
        Canonical.Argument name ->
            assignId idSource (Typed.Argument name)

        Canonical.Var name ->
            assignId idSource (Typed.Var name)

        Canonical.Plus e1 e2 ->
            let
                ( e1_, idSource1 ) =
                    assignIdsWith idSource e1

                ( e2_, idSource2 ) =
                    assignIdsWith idSource1 e2
            in
            assignId idSource2 (Typed.Plus e1_ e2_)

        Canonical.Cons e1 e2 ->
            let
                ( e1_, idSource1 ) =
                    assignIdsWith idSource e1

                ( e2_, idSource2 ) =
                    assignIdsWith idSource1 e2
            in
            assignId idSource2 (Typed.Cons e1_ e2_)

        Canonical.Lambda { argument, body } ->
            let
                ( body_, idSource1 ) =
                    assignIdsWith idSource body
            in
            assignId idSource1 (Typed.lambda argument body_)

        Canonical.Call { fn, argument } ->
            let
                ( fn_, idSource1 ) =
                    assignIdsWith idSource fn

                ( argument_, idSource2 ) =
                    assignIdsWith idSource1 argument
            in
            assignId idSource2
                (Typed.Call
                    { fn = fn_
                    , argument = argument_
                    }
                )

        Canonical.If { test, then_, else_ } ->
            let
                ( test_, idSource1 ) =
                    assignIdsWith idSource test

                ( then__, idSource2 ) =
                    assignIdsWith idSource1 then_

                ( else__, idSource3 ) =
                    assignIdsWith idSource2 else_
            in
            assignId idSource3
                (Typed.If
                    { test = test_
                    , then_ = then__
                    , else_ = else__
                    }
                )

        Canonical.Let { bindings, body } ->
            {- We don't thread the full (VarName, Binding) thing to IdSource
               as that would bloat the type signatures of IdGenerator too much.

               We unwrap the exprs from the bindings and then carefully put them
               back together in the same order (see the List.map2 below).
            -}
            let
                bindingsList =
                    Dict.toList bindings

                ( body_, idSource1 ) =
                    assignIdsWith idSource body

                ( bindingBodiesList, idSource2 ) =
                    List.foldl
                        (\( _, binding ) ( acc, currentIdSource ) ->
                            let
                                ( body__, nextIdSource ) =
                                    assignIdsWith currentIdSource binding.body
                            in
                            ( body__ :: acc
                            , nextIdSource
                            )
                        )
                        ( [], idSource1 )
                        bindingsList
            in
            assignId idSource2
                (Typed.let_
                    (Dict.fromList
                        (List.map2
                            (\( name, _ ) body__ -> ( name, { name = name, body = body__ } ))
                            bindingsList
                            bindingBodiesList
                        )
                    )
                    body_
                )

        Canonical.Unit ->
            assignId idSource Typed.Unit

        Canonical.List items ->
            let
                ( items_, idSource1 ) =
                    List.foldr
                        (\item ( acc, currentIdSource ) ->
                            let
                                ( item_, nextIdSource ) =
                                    assignIdsWith currentIdSource item
                            in
                            ( item_ :: acc
                            , nextIdSource
                            )
                        )
                        ( [], idSource )
                        items
            in
            assignId idSource1 (Typed.List items_)

        Canonical.Tuple e1 e2 ->
            let
                ( e1_, idSource1 ) =
                    assignIdsWith idSource e1

                ( e2_, idSource2 ) =
                    assignIdsWith idSource1 e2
            in
            assignId idSource2 (Typed.Tuple e1_ e2_)

        Canonical.Tuple3 e1 e2 e3 ->
            let
                ( e1_, idSource1 ) =
                    assignIdsWith idSource e1

                ( e2_, idSource2 ) =
                    assignIdsWith idSource1 e2

                ( e3_, idSource3 ) =
                    assignIdsWith idSource2 e3
            in
            assignId idSource3 (Typed.Tuple3 e1_ e2_ e3_)
