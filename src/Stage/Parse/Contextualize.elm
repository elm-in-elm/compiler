module Stage.Parse.Contextualize exposing (..)

{-| Here we define parsers. A parser is function

    type alias Parser =
        Lexer.LexItem -> ParseResult

that takes one lexed item and produces either an error or some state (it can also
choose to skip the lexed item).

-}

import Elm.AST.Frontend as Frontend exposing (Expr(..), LocatedExpr, LocatedPattern, Pattern(..))
import Elm.Data.Declaration
import Elm.Data.Exposing
import Elm.Data.Located as Located exposing (Located)
import Elm.Data.Module exposing (Module, ModuleType(..))
import Elm.Data.ModuleName exposing (ModuleName)
import Elm.Data.Qualifiedness as Qualifiedness exposing (PossiblyQualified(..))
import Elm.Data.Type.Concrete as ConcreteType exposing (ConcreteType)
import Elm.Data.TypeAnnotation exposing (TypeAnnotation)
import Elm.Data.VarName exposing (VarName)
import Stage.Parse.Lexer as Lexer exposing (LexItem(..))
import Stage.Parse.Token as Token exposing (Keyword)


{-| An 'block' starts with an unindented line and continues until the next
unindented line.

In the following Program

    module Main exposing (..)

    -- 0

    import Dict


    -- 1
    type
        A
        -- 2
        = A -- 2

    a : A

    -- 3
    a =
        -- 4
        A

    -- 4

There are five blocks.

-}
type Block
    = Module
        { ty : Elm.Data.Module.ModuleType
        , name : Elm.Data.ModuleName.ModuleName
        , exposingList : Elm.Data.Exposing.Exposing
        }
    | TypeAlias
        { ty : Token.TypeOrConstructor
        , expr : ConcreteType PossiblyQualified
        }
    | CustomType
        { ty : VarName
        , constructors : List (Elm.Data.Declaration.Constructor PossiblyQualified)
        }


type State
    = State_BlockStart
    | State_Error_Recovery
    | State_BlockFirstItem BlockFirstItem
    | State_BlockTypeAlias BlockTypeAlias
    | State_BlockCustomType BlockCustomType


type ParseResult
    = ParseResult_Ok State
    | ParseResult_Err Error
    | ParseResult_Skip
    | ParseResult_Panic String


type alias State_ =
    { previousBlocks : List (Result ( State, Error ) Block)
    , state : State
    }


type BlockFirstItem
    = BlockFirstItem_Type
    | BlockFirstItem_Module
    | BlockFirstItem_Name Token.ValueOrFunction


type BlockTypeAlias
    = BlockTypeAlias_Keywords
    | BlockTypeAlias_Named Token.TypeOrConstructor
    | BlockTypeAlias_NamedAssigns Token.TypeOrConstructor
    | BlockTypeAlias_Completish Token.TypeOrConstructor PartialTypeExpression2
    | BlockTypeAlias_Complete Token.TypeOrConstructor PartialTypeExpression


type BlockCustomType
    = BlockCustomType_Named Token.TypeOrConstructor
    | BlockCustomType_NamedAssigns Token.TypeOrConstructor


{-| Notes:

  - Type names can start with a lower case character as it may be generic. If
    it is generic there should be no args (we do not check this currently
    though :().

-}
type PartialTypeExpression
    = TypeExpression_NamedType
        { name : String
        , args : Stack PartialTypeExpression
        }
    | TypeExpression_Unit


partialTypeExpressionToConcreteType : PartialTypeExpression -> ConcreteType PossiblyQualified
partialTypeExpressionToConcreteType pte =
    case pte of
        TypeExpression_NamedType { name, args } ->
            ConcreteType.UserDefinedType
                { qualifiedness = Qualifiedness.PossiblyQualified Nothing
                , name = name
                , args =
                    args
                        |> toList partialTypeExpressionToConcreteType
                }

        TypeExpression_Unit ->
            ConcreteType.Unit


recoverErrors : ParseResult -> ParseResult
recoverErrors res =
    case res of
        ParseResult_Err _ ->
            ParseResult_Ok State_Error_Recovery

        _ ->
            res


parseResultFromMaybeResult : Maybe (Result Error State) -> ParseResult
parseResultFromMaybeResult x =
    case x of
        Just (Ok s) ->
            ParseResult_Ok s

        Just (Err e) ->
            ParseResult_Err e

        Nothing ->
            ParseResult_Skip


type TypeExpressionContext
    = TypeExpressionContext_Bracket Lexer.BracketType
    | TypeExpressionContext_Alias


type alias PartialTypeExpression2 =
    { stack : Stack ( TypeExpressionContext, Maybe PartialTypeExpression )
    , current : ( TypeExpressionContext, Maybe PartialTypeExpression )
    }


type TypeExpressionResult
    = TypeExpressionResult_Progress PartialTypeExpression2
    | TypeExpressionResult_Done PartialTypeExpression
    | TypeExpressionResult_Empty


type Error
    = Error_InvalidToken LexItem Expecting
    | Error_MisplacedKeyword Keyword
    | Error_BlockStartsWithTypeOrConstructor Token.TypeOrConstructor
    | Error_TypeNameStartsWithLowerCase Token.ValueOrFunction
    | Error_UnmatchedBracket Lexer.BracketType Lexer.BracketRole
    | Error_UnitTypeDoesTakeArgs PartialTypeExpression
    | Error_PartwayThroughTypeAlias
    | Error_Panic String


type Expecting
    = Expecting_Sigil Lexer.LexSigil
    | Expecting_Block
    | Expecting_TypeName


run : List LexItem -> List (Result ( State, Error ) Block)
run items =
    runHelp
        items
        { previousBlocks = []
        , state = State_BlockStart
        }


runHelp : List LexItem -> State_ -> List (Result ( State, Error ) Block)
runHelp items state =
    case items of
        item :: rest ->
            runHelp
                rest
                (case parseAnything state.state item of
                    ParseResult_Ok newState_ ->
                        { previousBlocks = state.previousBlocks
                        , state = newState_
                        }

                    ParseResult_Err error ->
                        { previousBlocks = Err ( state.state, error ) :: state.previousBlocks
                        , state = State_Error_Recovery
                        }

                    ParseResult_Panic error ->
                        -- TODO(harry): more violent error here
                        { previousBlocks =
                            Err ( state.state, Error_Panic error )
                                :: state.previousBlocks
                        , state = State_Error_Recovery
                        }

                    ParseResult_Skip ->
                        state
                )

        [] ->
            List.reverse
                (case blockFromState state.state of
                    Nothing ->
                        state.previousBlocks

                    Just newBlock ->
                        (newBlock |> Result.mapError (\err -> ( state.state, err )))
                            :: state.previousBlocks
                )


parseAnything : State -> LexItem -> ParseResult
parseAnything state =
    case state of
        State_Error_Recovery ->
            parseBlockStart
                >> recoverErrors

        State_BlockStart ->
            parseBlockStart

        State_BlockFirstItem BlockFirstItem_Type ->
            parseTypeBlock

        State_BlockFirstItem BlockFirstItem_Module ->
            Debug.todo ""

        State_BlockFirstItem (BlockFirstItem_Name name) ->
            Debug.todo ""

        State_BlockTypeAlias BlockTypeAlias_Keywords ->
            parseTypeAliasName

        State_BlockTypeAlias (BlockTypeAlias_Named name) ->
            parseAssignment
                (State_BlockTypeAlias (BlockTypeAlias_NamedAssigns name))

        State_BlockTypeAlias (BlockTypeAlias_NamedAssigns name) ->
            parserTypeExpr
                (\res ->
                    case res of
                        TypeExpressionResult_Progress expr ->
                            State_BlockTypeAlias (BlockTypeAlias_Completish name expr)

                        TypeExpressionResult_Done expr ->
                            State_BlockTypeAlias (BlockTypeAlias_Complete name expr)

                        TypeExpressionResult_Empty ->
                            Debug.todo ""
                )
                { current = ( TypeExpressionContext_Alias, Nothing )
                , stack = empty
                }

        State_BlockTypeAlias (BlockTypeAlias_Completish name exprSoFar) ->
            parserTypeExpr
                (\res ->
                    case res of
                        TypeExpressionResult_Progress expr ->
                            State_BlockTypeAlias (BlockTypeAlias_Completish name expr)

                        TypeExpressionResult_Done expr ->
                            State_BlockTypeAlias (BlockTypeAlias_Complete name expr)

                        TypeExpressionResult_Empty ->
                            Debug.todo ""
                )
                exprSoFar

        State_BlockTypeAlias (BlockTypeAlias_Complete name expr) ->
            Debug.todo ""

        State_BlockCustomType (BlockCustomType_Named name) ->
            parseAssignment
                (State_BlockCustomType (BlockCustomType_NamedAssigns name))

        State_BlockCustomType (BlockCustomType_NamedAssigns name) ->
            Debug.todo ""


{-|


### Panics

If the LexItem is a `Newlines` with indentation or is `Whitespace`.

-}
parseBlockStart : LexItem -> ParseResult
parseBlockStart item =
    case item of
        Lexer.Token str ->
            case Token.classifyToken str of
                Token.TokenKeyword Token.Type ->
                    ParseResult_Ok (State_BlockFirstItem BlockFirstItem_Type)

                Token.TokenKeyword Token.Module ->
                    ParseResult_Ok (State_BlockFirstItem BlockFirstItem_Module)

                Token.TokenKeyword other ->
                    ParseResult_Err (Error_MisplacedKeyword other)

                Token.TokenValueOrFunction valOrFunc ->
                    ParseResult_Ok (State_BlockFirstItem (BlockFirstItem_Name valOrFunc))

                Token.TokenTypeOrConstructor typeOrConstructor ->
                    ParseResult_Err (Error_BlockStartsWithTypeOrConstructor typeOrConstructor)

        Lexer.Newlines _ 0 ->
            ParseResult_Ok State_BlockStart

        Lexer.Newlines _ _ ->
            ParseResult_Panic "parseBlockStart expects a block but found some indented content"

        Lexer.Whitespace _ ->
            ParseResult_Panic "parseBlockStart expects a block but found some indented content"

        _ ->
            ParseResult_Err (Error_InvalidToken item Expecting_Block)


parseTypeBlock : LexItem -> ParseResult
parseTypeBlock item =
    case item of
        Lexer.Token str ->
            case Token.classifyToken str of
                Token.TokenKeyword Token.Alias ->
                    State_BlockTypeAlias BlockTypeAlias_Keywords
                        |> ParseResult_Ok

                Token.TokenKeyword other ->
                    Error_MisplacedKeyword other
                        |> ParseResult_Err

                Token.TokenTypeOrConstructor typeOrConstructor ->
                    State_BlockCustomType (BlockCustomType_Named typeOrConstructor)
                        |> ParseResult_Ok

                Token.TokenValueOrFunction valOrFunc ->
                    Error_TypeNameStartsWithLowerCase valOrFunc
                        |> ParseResult_Err

        Lexer.Newlines _ 0 ->
            -- TODO(harry): we might be partway through a custom type here,
            -- adjust error accordingly.
            Error_PartwayThroughTypeAlias
                |> ParseResult_Err

        Lexer.Newlines _ _ ->
            ParseResult_Skip

        Whitespace _ ->
            ParseResult_Skip

        _ ->
            -- TODO(harry) indicate that we could also be expecting the `alias`
            -- keyword.
            Error_InvalidToken item Expecting_TypeName
                |> ParseResult_Err


parseTypeAliasName : LexItem -> ParseResult
parseTypeAliasName item =
    case item of
        Lexer.Token str ->
            case Token.classifyToken str of
                Token.TokenKeyword other ->
                    Error_MisplacedKeyword other
                        |> ParseResult_Err

                Token.TokenTypeOrConstructor typeOrConstructor ->
                    State_BlockTypeAlias (BlockTypeAlias_Named typeOrConstructor)
                        |> ParseResult_Ok

                Token.TokenValueOrFunction valOrFunc ->
                    Error_TypeNameStartsWithLowerCase valOrFunc
                        |> ParseResult_Err

        Lexer.Newlines _ 0 ->
            Error_PartwayThroughTypeAlias
                |> ParseResult_Err

        Lexer.Newlines _ _ ->
            ParseResult_Skip

        Lexer.Whitespace _ ->
            ParseResult_Skip

        _ ->
            Error_InvalidToken item Expecting_TypeName
                |> ParseResult_Err


parseAssignment : State -> LexItem -> ParseResult
parseAssignment newState item =
    case item of
        Lexer.Sigil Lexer.Assign ->
            newState
                |> ParseResult_Ok

        Lexer.Newlines _ 0 ->
            -- TODO(harry): we might be partway through almsot anything here,
            -- adjust error accordingly.
            Error_PartwayThroughTypeAlias
                |> ParseResult_Err

        Lexer.Newlines _ _ ->
            ParseResult_Skip

        Lexer.Whitespace _ ->
            ParseResult_Skip

        _ ->
            Error_InvalidToken item (Expecting_Sigil Lexer.Assign)
                |> ParseResult_Err


parserTypeExpr :
    (TypeExpressionResult -> State)
    -> PartialTypeExpression2
    -> LexItem
    -> ParseResult
parserTypeExpr newState ({ stack, current } as prevExpr) item =
    case item of
        Lexer.Token str ->
            case current of
                ( context, Nothing ) ->
                    newState
                        ({ stack = stack
                         , current =
                            ( context
                            , Just
                                (TypeExpression_NamedType
                                    { name = str
                                    , args = empty
                                    }
                                )
                            )
                         }
                            |> TypeExpressionResult_Progress
                        )
                        |> ParseResult_Ok

                ( context, Just (TypeExpression_NamedType { name, args }) ) ->
                    -- TODO(harry): think about how this is fundamentally
                    -- similar to the definition of custom type constructors.
                    -- The value of `_context` is key I think.
                    newState
                        ({ stack = stack
                         , current =
                            ( context
                            , Just
                                (TypeExpression_NamedType
                                    { name = name
                                    , args =
                                        TypeExpression_NamedType { name = str, args = empty }
                                            |> pushOnto args
                                    }
                                )
                            )
                         }
                            |> TypeExpressionResult_Progress
                        )
                        |> ParseResult_Ok

                ( context, Just TypeExpression_Unit ) ->
                    Error_UnitTypeDoesTakeArgs
                        (TypeExpression_NamedType
                            { name = str
                            , args = empty
                            }
                        )
                        |> ParseResult_Err

        Lexer.Sigil (Lexer.Bracket Lexer.Round role) ->
            case role of
                Lexer.Open ->
                    newState
                        ({ stack =
                            current
                                |> pushOnto stack
                         , current = ( TypeExpressionContext_Bracket Lexer.Round, Nothing )
                         }
                            |> TypeExpressionResult_Progress
                        )
                        |> ParseResult_Ok

                Lexer.Close ->
                    (case current of
                        ( TypeExpressionContext_Bracket Lexer.Round, mexpr ) ->
                            case mexpr of
                                Nothing ->
                                    (case pop stack of
                                        Just ( ( newContext, newExprToAddTo ), newStack ) ->
                                            let
                                                rnewExpr =
                                                    case newExprToAddTo of
                                                        Nothing ->
                                                            Just TypeExpression_Unit
                                                                |> Ok

                                                        Just (TypeExpression_NamedType { name, args }) ->
                                                            Just
                                                                (TypeExpression_NamedType
                                                                    { name = name
                                                                    , args =
                                                                        TypeExpression_Unit
                                                                            |> pushOnto args
                                                                    }
                                                                )
                                                                |> Ok

                                                        Just TypeExpression_Unit ->
                                                            Error_UnitTypeDoesTakeArgs TypeExpression_Unit
                                                                |> Err
                                            in
                                            rnewExpr
                                                |> Result.map
                                                    (\newExpr ->
                                                        { stack = newStack
                                                        , current = ( newContext, newExpr )
                                                        }
                                                            |> TypeExpressionResult_Progress
                                                    )

                                        Nothing ->
                                            TypeExpressionResult_Done TypeExpression_Unit
                                                |> Ok
                                    )
                                        |> Result.map newState
                                        |> Just

                                Just expr ->
                                    newState
                                        (case pop stack of
                                            Just ( newCurrent, newStack ) ->
                                                { stack = newStack
                                                , current = Debug.todo "newCurrent"
                                                }
                                                    |> TypeExpressionResult_Progress

                                            Nothing ->
                                                TypeExpressionResult_Done expr
                                        )
                                        |> Ok
                                        |> Just

                        _ ->
                            -- TODO(harry): can we add information about the
                            -- bracket we are expecting here?
                            Error_UnmatchedBracket Lexer.Round Lexer.Close
                                |> Err
                                |> Just
                    )
                        |> parseResultFromMaybeResult

        Lexer.Newlines _ 0 ->
            newState (TypeExpressionResult_Progress prevExpr)
                |> ParseResult_Ok

        Lexer.Newlines _ _ ->
            ParseResult_Skip

        Lexer.Whitespace _ ->
            ParseResult_Skip

        _ ->
            Error_InvalidToken item (Expecting_Sigil Lexer.Assign)
                |> ParseResult_Err


blockFromState : State -> Maybe (Result Error Block)
blockFromState state =
    case state of
        State_Error_Recovery ->
            Nothing

        State_BlockStart ->
            Nothing

        State_BlockFirstItem firstItem ->
            Debug.todo "handle incomplete block"

        State_BlockTypeAlias BlockTypeAlias_Keywords ->
            Error_PartwayThroughTypeAlias
                |> Err
                |> Just

        State_BlockTypeAlias (BlockTypeAlias_Named _) ->
            Error_PartwayThroughTypeAlias
                |> Err
                |> Just

        State_BlockTypeAlias (BlockTypeAlias_NamedAssigns _) ->
            Error_PartwayThroughTypeAlias
                |> Err
                |> Just

        State_BlockTypeAlias (BlockTypeAlias_Completish name { stack, current }) ->
            let
                -- _ =
                --     Debug.log "part" partialExpr
                ( context, mexpr ) =
                    current
            in
            -- TODO(harry): handle maybe incomplete block
            case ( pop stack, context, mexpr ) of
                ( Nothing, TypeExpressionContext_Alias, Just expr ) ->
                    { ty = name
                    , expr = partialTypeExpressionToConcreteType expr
                    }
                        |> TypeAlias
                        |> Ok
                        |> Just

                _ ->
                    Debug.todo "handle maybe incomplete block"

        State_BlockTypeAlias (BlockTypeAlias_Complete name expr) ->
            { ty = name
            , expr = partialTypeExpressionToConcreteType expr
            }
                |> TypeAlias
                |> Ok
                |> Just

        State_BlockCustomType firstItem ->
            Debug.todo "handle incomplete block"



-- HELPERS


type Stack a
    = Stack (List a)


empty : Stack a
empty =
    Stack []


singleton : a -> Stack a
singleton val =
    Stack [ val ]


pushOnto : Stack a -> a -> Stack a
pushOnto (Stack ls) val =
    Stack (val :: ls)


pop : Stack a -> Maybe ( a, Stack a )
pop (Stack ls) =
    case ls of
        last :: preceding ->
            Just ( last, Stack preceding )

        [] ->
            Nothing


toList : (a -> b) -> Stack a -> List b
toList mapper (Stack ls) =
    List.foldl
        (\curr prev -> mapper curr :: prev)
        []
        ls
