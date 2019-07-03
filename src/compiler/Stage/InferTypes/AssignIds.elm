module Stage.InferTypes.AssignIds exposing (toIdGenerator)

import AST.Canonical as Canonical
import AST.Typed as Typed
import Common
import Dict.Any
import Stage.InferTypes.IdSource as Id exposing (IdGenerator)


toIdGenerator : Canonical.Expr -> IdGenerator Typed.Expr_
toIdGenerator expr =
    case expr of
        {- With literals, we could plug their final type in right here
           (no solving needed!) but let's be uniform and do everything through
           the constraint solver in stages 2 and 3.
        -}
        Canonical.Literal literal ->
            Id.constant (Typed.Literal literal)

        -- We remember argument's IDs so that we can later use them in Lambda
        Canonical.Argument name ->
            Id.constant (Typed.Argument name)
                |> Id.rememberVar name

        Canonical.Var name ->
            Id.constant (Typed.Var name)

        Canonical.Plus e1 e2 ->
            Id.map2 Typed.Plus
                (toIdGenerator e1)
                (toIdGenerator e2)

        Canonical.Lambda { argument, body } ->
            Id.map1AndVar (Typed.lambda argument)
                (toIdGenerator body)
                argument

        Canonical.Call { fn, argument } ->
            Id.map2
                (\fn_ argument_ ->
                    Typed.Call
                        { fn = fn_
                        , argument = argument_
                        }
                )
                (toIdGenerator fn)
                (toIdGenerator argument)

        Canonical.If { test, then_, else_ } ->
            Id.map3
                (\test_ then__ else__ ->
                    Typed.If
                        { test = test_
                        , then_ = then__
                        , else_ = else__
                        }
                )
                (toIdGenerator test)
                (toIdGenerator then_)
                (toIdGenerator else_)

        Canonical.Let { bindings, body } ->
            {- We don't thread the full (VarName, Binding) thing to IdSource
               as that would bloat the type signatures of IdGenerator too much.

               We unwrap the exprs from the bindings and then carefully put them
               back together in the same order (see the List.map2 below).
            -}
            let
                bindingsList =
                    Dict.Any.toList bindings
            in
            Id.map1AndMultiple
                (\bindingsList_ body_ ->
                    Typed.let_
                        (Dict.Any.fromList
                            Common.varNameToString
                            (List.map2 (\( name, _ ) body__ -> ( name, { name = name, body = body__ } ))
                                bindingsList
                                bindingsList_
                            )
                        )
                        body_
                )
                (List.map (Tuple.second >> .body >> toIdGenerator) bindingsList)
                (toIdGenerator body)

        Canonical.Unit ->
            Id.constant Typed.Unit
