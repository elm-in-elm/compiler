module NoParserBacktrackable exposing (rule)

{-| Make sure we don't use `Parser.backtrackable` in our parser code.

To be used with <https://package.elm-lang.org/packages/jfmengels/elm-review/latest/>


# Rule

@docs rule

-}

import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Node as Node exposing (Node)
import Review.Rule as Rule exposing (Error, Rule)


{-| Make sure we don't use `Parser.backtrackable` in our parser code.
If you want to use this rule, add it to `config : List Rule` in `review/src/ReviewConfig.elm`
-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "NoParserBacktrackable" ()
        |> Rule.withSimpleExpressionVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


expressionVisitor : Node Expression -> List (Error {})
expressionVisitor node =
    case Node.value node of
        Application (fn :: _) ->
            case Node.value fn of
                FunctionOrValue modules "backtrackable" ->
                    if
                        List.member modules
                            [ [ "P" ]
                            , [ "Parser" ]
                            , [ "Parser", "Advanced" ]
                            ]
                    then
                        [ Rule.error
                            { message = "Parser.backtrackable function used"
                            , details = [ "Please don't use this function. Any parser using it can be rewritten into one that doesn't use it." ]
                            }
                            (Node.range node)
                        ]

                    else
                        []

                _ ->
                    []

        _ ->
            []
