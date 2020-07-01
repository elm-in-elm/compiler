module Stage.Parse.AdvancedWithState exposing
    ( DeadEnd
    , Nestable(..)
    , Parser
    , Step(..)
    , Token(..)
    , Trailing(..)
    , andThen
    , backtrackable
    , chompIf
    , chompUntil
    , chompUntilEndOr
    , chompWhile
    , commit
    , end
    , float
    , getChompedString
    , getCol
    , getIndent
    , getOffset
    , getPosition
    , getRow
    , getSource
    , ignore
    , inContext
    , int
    , keep
    , keyword
    , lazy
    , lineComment
    , loop
    , map
    , mapChompedString
    , multiComment
    , number
    , oneOf
    , problem
    , run
    , sequence
    , spaces
    , succeed
    , symbol
    , token
    , updateState
    , variable
    , withIndent
    , withState
    )

{-| Everything here works just like in the
[`Parser.Advanced`](/packages/elm/parser/latest/Parser-Advanced)
module, except that there are no infixes `(|=)` and `(|.)`.

You can use the pipe operator and the `keep` and `ignore` functions,
respectively, to replace them. Eg.:

    succeed identity
        |> keep myParser
        |> ignore spaces

This modification of the `Parser.Advanced` module allows to keep a state in your parser.
Two functions were added to handle the state: `updateState` and `withState`.

-}

import Char
import Parser.Advanced as PA
import Set


type Parser context problem state value
    = Parser (state -> PA.Parser context problem ( state, value ))


run : Parser c x s a -> s -> String -> Result (List (DeadEnd c x)) ( s, a )
run (Parser parse) state src =
    PA.run (parse state) src


type alias DeadEnd context problem =
    { row : Int
    , col : Int
    , problem : problem
    , contextStack : List { row : Int, col : Int, context : context }
    }


succeed : a -> Parser c x s a
succeed a =
    Parser <| \s -> PA.succeed ( s, a )


problem : x -> Parser c x s a
problem x =
    Parser <| \s -> PA.problem x


map : (a -> b) -> Parser c x s a -> Parser c x s b
map func (Parser parse) =
    Parser <| \s0 -> PA.map (\( s1, a ) -> ( s1, func a )) (parse s0)


map2 : (a -> b -> value) -> Parser c x s a -> Parser c x s b -> Parser c x s value
map2 func (Parser parseA) (Parser parseB) =
    Parser <|
        \s0 ->
            parseA s0
                |> PA.andThen
                    (\( s1, a ) ->
                        parseB s1
                            |> PA.map (\( s2, b ) -> ( s2, func a b ))
                    )


keeper : Parser c x s (a -> b) -> Parser c x s a -> Parser c x s b
keeper funcParser argParser =
    map2 (<|) funcParser argParser


keep : Parser c x s a -> Parser c x s (a -> b) -> Parser c x s b
keep argParser funcParser =
    map2 (<|) funcParser argParser


ignorer : Parser c x s keep -> Parser c x s ignore -> Parser c x s keep
ignorer keepParser ignoreParser =
    map2 always keepParser ignoreParser


ignore : Parser c x s ignore -> Parser c x s keep -> Parser c x s keep
ignore ignoreParser keepParser =
    map2 always keepParser ignoreParser


andThen : (a -> Parser c x s b) -> Parser c x s a -> Parser c x s b
andThen callback (Parser parse) =
    Parser <|
        \s0 ->
            parse s0
                |> PA.andThen (\( s1, a ) -> (\(Parser x) -> x s1) (callback a))


lazy : (() -> Parser c x s a) -> Parser c x s a
lazy thunk =
    Parser (\s -> PA.lazy (\() -> (\(Parser t_) -> t_ s) (thunk ())))


oneOf : List (Parser c x s a) -> Parser c x s a
oneOf parsers =
    Parser <| \s -> PA.oneOf (List.map (\(Parser a) -> a s) parsers)


type Step state a
    = Loop state
    | Done a


loop : state -> (state -> Parser c x s (Step state a)) -> Parser c x s a
loop state callback =
    let
        wrapper ( oldS, v ) =
            let
                (Parser p) =
                    callback v
            in
            p oldS
                |> PA.map
                    (\( newState, r ) ->
                        case r of
                            Loop l ->
                                PA.Loop ( newState, l )

                            Done d ->
                                PA.Done ( newState, d )
                    )
    in
    Parser <| \s -> PA.loop ( s, state ) wrapper


backtrackable : Parser c x s a -> Parser c x s a
backtrackable (Parser parse) =
    Parser <| \s -> PA.backtrackable (parse s)


fromParserAdvanced : PA.Parser c x a -> Parser c x s a
fromParserAdvanced p =
    Parser <| \s -> PA.map (\a -> ( s, a )) p


commit : a -> Parser c x s a
commit a =
    fromParserAdvanced (PA.commit a)


symbol : Token x -> Parser c x s ()
symbol (Token str expecting) =
    fromParserAdvanced (PA.symbol (PA.Token str expecting))


keyword : Token x -> Parser c x s ()
keyword (Token str expecting) =
    fromParserAdvanced (PA.keyword (PA.Token str expecting))


type Token x
    = Token String x


token : Token x -> Parser c x s ()
token (Token str expecting) =
    fromParserAdvanced (PA.token (PA.Token str expecting))


int : x -> x -> Parser c x s Int
int expecting invalid =
    fromParserAdvanced (PA.int expecting invalid)


float : x -> x -> Parser c x s Float
float expecting invalid =
    fromParserAdvanced (PA.float expecting invalid)


number :
    { int : Result x (Int -> a)
    , hex : Result x (Int -> a)
    , octal : Result x (Int -> a)
    , binary : Result x (Int -> a)
    , float : Result x (Float -> a)
    , invalid : x
    , expecting : x
    }
    -> Parser c x s a
number c =
    fromParserAdvanced (PA.number c)


end : x -> Parser c x s ()
end x =
    fromParserAdvanced (PA.end x)


getChompedString : Parser c x s a -> Parser c x s String
getChompedString parser =
    mapChompedString always parser


mapChompedString : (String -> a -> b) -> Parser c x s a -> Parser c x s b
mapChompedString func (Parser parse) =
    Parser <|
        \s0 ->
            PA.mapChompedString (\str ( s1, a ) -> ( s1, func str a )) (parse s0)


chompIf : (Char -> Bool) -> x -> Parser c x s ()
chompIf isGood expecting =
    fromParserAdvanced (PA.chompIf isGood expecting)


chompWhile : (Char -> Bool) -> Parser c x s ()
chompWhile isGood =
    fromParserAdvanced (PA.chompWhile isGood)


chompUntil : Token x -> Parser c x s ()
chompUntil (Token str expecting) =
    fromParserAdvanced (PA.chompUntil (PA.Token str expecting))


chompUntilEndOr : String -> Parser c x s ()
chompUntilEndOr str =
    fromParserAdvanced (PA.chompUntilEndOr str)


inContext : context -> Parser context x s a -> Parser context x s a
inContext context (Parser parse) =
    Parser <| \s -> PA.inContext context (parse s)


getIndent : Parser c x s Int
getIndent =
    fromParserAdvanced PA.getIndent


withIndent : Int -> Parser c x s a -> Parser c x s a
withIndent newIndent (Parser parse) =
    Parser <| \s -> PA.withIndent newIndent (parse s)


getPosition : Parser c x s ( Int, Int )
getPosition =
    fromParserAdvanced PA.getPosition


getRow : Parser c x s Int
getRow =
    fromParserAdvanced PA.getRow


getCol : Parser c x s Int
getCol =
    fromParserAdvanced PA.getCol


getOffset : Parser c x s Int
getOffset =
    fromParserAdvanced PA.getOffset


getSource : Parser c x s String
getSource =
    fromParserAdvanced PA.getSource


variable :
    { start : Char -> Bool
    , inner : Char -> Bool
    , reserved : Set.Set String
    , expecting : x
    }
    -> Parser c x s String
variable i =
    fromParserAdvanced (PA.variable i)


sequence :
    { start : Token x
    , separator : Token x
    , end : Token x
    , spaces : Parser c x s ()
    , item : Parser c x s a
    , trailing : Trailing
    }
    -> Parser c x s (List a)
sequence i =
    skip (token i.start) <|
        skip i.spaces <|
            sequenceEnd (token i.end) i.spaces i.item (token i.separator) i.trailing


type Trailing
    = Forbidden
    | Optional
    | Mandatory


skip : Parser c x s ignore -> Parser c x s keep -> Parser c x s keep
skip ignoreParser keepParser =
    map2 (\_ b -> b) ignoreParser keepParser


sequenceEnd : Parser c x s () -> Parser c x s () -> Parser c x s a -> Parser c x s () -> Trailing -> Parser c x s (List a)
sequenceEnd ender ws parseItem sep trailing =
    let
        chompRest item =
            case trailing of
                Forbidden ->
                    loop [ item ] (sequenceEndForbidden ender ws parseItem sep)

                Optional ->
                    loop [ item ] (sequenceEndOptional ender ws parseItem sep)

                Mandatory ->
                    ignorer
                        (skip ws <|
                            skip sep <|
                                skip ws <|
                                    loop [ item ] (sequenceEndMandatory ws parseItem sep)
                        )
                        ender
    in
    oneOf
        [ parseItem |> andThen chompRest
        , ender |> map (\_ -> [])
        ]


sequenceEndForbidden : Parser c x s () -> Parser c x s () -> Parser c x s a -> Parser c x s () -> List a -> Parser c x s (Step (List a) (List a))
sequenceEndForbidden ender ws parseItem sep revItems =
    let
        chompRest item =
            sequenceEndForbidden ender ws parseItem sep (item :: revItems)
    in
    skip ws <|
        oneOf
            [ skip sep <| skip ws <| map (\item -> Loop (item :: revItems)) parseItem
            , ender |> map (\_ -> Done (List.reverse revItems))
            ]


sequenceEndOptional : Parser c x s () -> Parser c x s () -> Parser c x s a -> Parser c x s () -> List a -> Parser c x s (Step (List a) (List a))
sequenceEndOptional ender ws parseItem sep revItems =
    let
        parseEnd =
            map (\_ -> Done (List.reverse revItems)) ender
    in
    skip ws <|
        oneOf
            [ skip sep <|
                skip ws <|
                    oneOf
                        [ parseItem |> map (\item -> Loop (item :: revItems))
                        , parseEnd
                        ]
            , parseEnd
            ]


sequenceEndMandatory : Parser c x s () -> Parser c x s a -> Parser c x s () -> List a -> Parser c x s (Step (List a) (List a))
sequenceEndMandatory ws parseItem sep revItems =
    oneOf
        [ map (\item -> Loop (item :: revItems)) <|
            ignorer parseItem (ignorer ws (ignorer sep ws))
        , map (\_ -> Done (List.reverse revItems)) (succeed ())
        ]


spaces : Parser c x s ()
spaces =
    fromParserAdvanced PA.spaces


lineComment : Token x -> Parser c x s ()
lineComment (Token start expecting) =
    fromParserAdvanced (PA.lineComment (PA.Token start expecting))


type Nestable
    = NotNestable
    | Nestable


multiComment : Token x -> Token x -> Nestable -> Parser c x s ()
multiComment (Token open openExp) (Token close closeExp) nestable =
    fromParserAdvanced <|
        PA.multiComment (PA.Token open openExp) (PA.Token close closeExp) <|
            case nestable of
                NotNestable ->
                    PA.NotNestable

                Nestable ->
                    PA.Nestable


withState : (s -> Parser c x s a) -> Parser c x s a
withState f =
    Parser <|
        \state ->
            (\(Parser p) -> p state) (f state)


updateState : (s -> s) -> Parser c x s ()
updateState f =
    Parser <|
        \state -> PA.succeed ( f state, () )
