module NoSlowConcat exposing (rule)

{-| Make sure we use OurExtras.List.fastConcat and OurExtras.List.fastConcatMap
instead of List.concat and List.concatMap.

To be used with <https://package.elm-lang.org/packages/jfmengels/elm-review/latest/>


# Rule

@docs rule

-}

import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Node as Node exposing (Node)
import Review.Rule as Rule exposing (Error, Rule)


{-| Make sure we use OurExtras.List.fastConcat and OurExtras.List.fastConcatMap
instead of List.concat and List.concatMap.
If you want to use this rule, add it to `config : List Rule` in `review/src/ReviewConfig.elm`
-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "NoSlowConcat" ()
        |> Rule.withSimpleExpressionVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


expressionVisitor : Node Expression -> List (Error {})
expressionVisitor node =
    case Node.value node of
        Application (fn :: _) ->
            case Node.value fn of
                FunctionOrValue [ "List" ] "concat" ->
                    [ Rule.error
                        { message = "Slow List.concat function used"
                        , details =
                            [ "Please use OurExtras.List.fastConcat instead!"
                            , "Preferably `import OurExtras.List as List` and then `List.fastConcat`"
                            ]
                        }
                        (Node.range node)
                    ]

                FunctionOrValue [ "List" ] "concatMap" ->
                    [ Rule.error
                        { message = "Slow List.concatMap function used"
                        , details =
                            [ "Please use OurExtras.List.fastConcatMap instead!"
                            , "Preferably `import OurExtras.List as List` and then `List.fastConcatMap`"
                            ]
                        }
                        (Node.range node)
                    ]

                FunctionOrValue [ "List", "NonEmpty" ] "concat" ->
                    [ Rule.error
                        { message = "Slow List.NonEmpty.concat function used"
                        , details = [ "Please use OurExtras.List.NonEmpty.fastConcat instead!" ]
                        }
                        (Node.range node)
                    ]

                FunctionOrValue [ "List", "NonEmpty" ] "concatMap" ->
                    [ Rule.error
                        { message = "Slow List.NonEmpty.concatMap function used"
                        , details = [ "Please use OurExtras.List.NonEmpty.fastConcatMap instead!" ]
                        }
                        (Node.range node)
                    ]

                _ ->
                    []

        _ ->
            []
