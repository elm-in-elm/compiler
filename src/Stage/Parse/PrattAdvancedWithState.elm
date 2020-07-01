module Stage.Parse.PrattAdvancedWithState exposing
    ( Config
    , constant
    , expression
    , infixLeft
    , infixRight
    , literal
    , postfix
    , postfixWithOperatorResult
    , prefix
    , subExpression
    )

{-| Everything here works just like in the
[`Pratt.Advanced`](/packages/dmy/elm-pratt-parser/latest/Pratt-Advanced)
module.

This modification allows to work with the Stage.Parse.AdvancedWithState module.

-}

import Stage.Parse.AdvancedWithState as P exposing (Parser)


type Config c x s e
    = Config
        { oneOf : List (Config c x s e -> Parser c x s e)
        , andThenOneOf : List (Config c x s e -> ( Int, e -> Parser c x s e ))
        , spaces : Parser c x s ()
        }


expression :
    { oneOf : List (Config c x s e -> Parser c x s e)
    , andThenOneOf : List (Config c x s e -> ( Int, e -> Parser c x s e ))
    , spaces : Parser c x s ()
    }
    -> Parser c x s e
expression config =
    subExpression 0 <|
        Config
            { oneOf = config.oneOf
            , andThenOneOf = config.andThenOneOf
            , spaces = config.spaces
            }


subExpression : Int -> Config c x s e -> Parser c x s e
subExpression precedence ((Config conf) as config) =
    P.succeed identity
        |> P.ignore conf.spaces
        |> P.keep (P.lazy (\_ -> P.oneOf <| List.map ((|>) config) conf.oneOf))
        |> P.andThen (\leftExpression -> P.loop ( config, precedence, leftExpression ) expressionHelp)


expressionHelp : ( Config c x s e, Int, e ) -> Parser c x s (P.Step ( Config c x s e, Int, e ) e)
expressionHelp ( (Config conf) as config, precedence, leftExpression ) =
    P.succeed identity
        |> P.ignore conf.spaces
        |> P.keep
            (P.oneOf
                [ P.map
                    (\expr -> P.Loop ( config, precedence, expr ))
                    (operation config precedence leftExpression)
                , P.succeed (P.Done leftExpression)
                ]
            )


operation : Config c x s e -> Int -> e -> Parser c x s e
operation ((Config conf) as config) precedence leftExpression =
    P.oneOf <|
        List.filterMap
            (\toOperation -> filter (toOperation config) precedence leftExpression)
            conf.andThenOneOf


filter : ( Int, e -> Parser c x s e ) -> Int -> e -> Maybe (Parser c x s e)
filter ( precedence, parser ) currentPrecedence leftExpression =
    if precedence > currentPrecedence then
        Just (parser leftExpression)

    else
        Nothing


literal : Parser c x s e -> Config c x s e -> Parser c x s e
literal =
    always


constant : Parser c x s () -> e -> Config c x s e -> Parser c x s e
constant constantParser e _ =
    P.map (always e) constantParser


prefix : Int -> Parser c x s () -> (e -> e) -> Config c x s e -> Parser c x s e
prefix precedence operator apply config =
    P.succeed apply
        |> P.ignore operator
        |> P.keep (subExpression precedence config)


infixLeft : Int -> Parser c x s () -> (e -> e -> e) -> Config c x s e -> ( Int, e -> Parser c x s e )
infixLeft precedence =
    infixHelp ( precedence, precedence )


infixRight : Int -> Parser c x s () -> (e -> e -> e) -> Config c x s e -> ( Int, e -> Parser c x s e )
infixRight precedence =
    -- To get right associativity, we use (precedence - 1) for the
    -- right precedence.
    infixHelp ( precedence, precedence - 1 )


infixHelp : ( Int, Int ) -> Parser c x s () -> (e -> e -> e) -> Config c x s e -> ( Int, e -> Parser c x s e )
infixHelp ( leftPrecedence, rightPrecedence ) operator apply config =
    ( leftPrecedence
    , \left ->
        P.succeed (apply left)
            |> P.ignore operator
            |> P.keep (subExpression rightPrecedence config)
    )


postfix : Int -> Parser c x s () -> (e -> e) -> Config c x s e -> ( Int, e -> Parser c x s e )
postfix precedence operator apply _ =
    ( precedence
    , \left -> P.map (\_ -> apply left) operator
    )


{-| Taken from [dmy/elm-pratt-parser](https://package.elm-lang.org/packages/dmy/elm-pratt-parser/latest/Pratt-Advanced#postfix),
made to accept the operator parser result.

It differs from an _infix_ expression by not having left _and_ right expressions.
It has only a left expression and an operator, eg.: 180º (the degree (`º`)
symbol is the postfix operator).

It can be used to parse Elm's aliasing expressions, like `{ foo } as bar`,
since only the `{ foo }` is a pattern expression, but we also need the `bar`
string, which is not another expression.

-}
postfixWithOperatorResult : Int -> Parser c x s a -> (e -> a -> e) -> Config c x s e -> ( Int, e -> Parser c x s e )
postfixWithOperatorResult precedence operator apply _ =
    ( precedence
    , \left -> P.map (apply left) operator
    )
