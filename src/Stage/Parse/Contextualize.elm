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
    | ParseResult_Complete Block
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
    | TypeExpression_Bracketed PartialTypeExpression


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

        TypeExpression_Bracketed ty ->
            partialTypeExpressionToConcreteType ty


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


type alias PartialTypeExpression2 =
    { root : Maybe PartialTypeExpression
    , bracketStack : Stack (Maybe PartialTypeExpression)
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
    | Error_TypeDoesNotTakeArgs PartialTypeExpression PartialTypeExpression
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
            case parseAnything state.state item of
                ParseResult_Ok newState_ ->
                    runHelp
                        rest
                        { previousBlocks = state.previousBlocks
                        , state = newState_
                        }

                ParseResult_Complete block ->
                    runHelp
                        rest
                        { previousBlocks = Ok block :: state.previousBlocks
                        , state = State_BlockStart
                        }

                ParseResult_Err error ->
                    runHelp
                        rest
                        { previousBlocks = Err ( state.state, error ) :: state.previousBlocks
                        , state =
                            case item of
                                Lexer.Newlines _ 0 ->
                                    State_BlockStart

                                _ ->
                                    State_Error_Recovery
                        }

                ParseResult_Panic error ->
                    -- TODO(harry): more violent error here
                    runHelp
                        [{- An empty list to abort parsing. -}]
                        { previousBlocks =
                            Err ( state.state, Error_Panic error )
                                :: state.previousBlocks
                        , state = State_Error_Recovery
                        }

                ParseResult_Skip ->
                    runHelp
                        rest
                        state

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
            \item ->
                case item of
                    Lexer.Newlines _ 0 ->
                        State_BlockStart
                            |> ParseResult_Ok

                    _ ->
                        State_Error_Recovery
                            |> ParseResult_Ok

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
                                |> ParseResult_Ok

                        TypeExpressionResult_Done expr ->
                            { ty = name
                            , expr = partialTypeExpressionToConcreteType expr
                            }
                                |> TypeAlias
                                |> ParseResult_Complete

                        TypeExpressionResult_Empty ->
                            Debug.todo ""
                )
                { root = Nothing
                , bracketStack = empty
                }

        State_BlockTypeAlias (BlockTypeAlias_Completish name exprSoFar) ->
            parserTypeExpr
                (\res ->
                    case res of
                        TypeExpressionResult_Progress expr ->
                            State_BlockTypeAlias (BlockTypeAlias_Completish name expr)
                                |> ParseResult_Ok

                        TypeExpressionResult_Done expr ->
                            { ty = name
                            , expr = partialTypeExpressionToConcreteType expr
                            }
                                |> TypeAlias
                                |> ParseResult_Complete

                        TypeExpressionResult_Empty ->
                            Debug.todo ""
                )
                exprSoFar

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
            ParseResult_Panic "parseBlockStart expects a block but found some indented content."

        Lexer.Whitespace _ ->
            ParseResult_Panic "parseBlockStart expects a block but found some whitespace"

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
    (TypeExpressionResult -> ParseResult)
    -> PartialTypeExpression2
    -> LexItem
    -> ParseResult
parserTypeExpr newState ({ bracketStack, root } as prevExpr) item =
    case item of
        Lexer.Token str ->
            let
                newType =
                    TypeExpression_NamedType
                        { name = str
                        , args = empty
                        }
            in
            case pop bracketStack of
                Nothing ->
                    case root of
                        Nothing ->
                            { bracketStack = empty
                            , root = Just newType
                            }
                                |> TypeExpressionResult_Progress
                                |> newState

                        Just (TypeExpression_NamedType { name, args }) ->
                            { bracketStack = empty
                            , root =
                                Just
                                    (TypeExpression_NamedType
                                        { name = name
                                        , args =
                                            newType
                                                |> pushOnto args
                                        }
                                    )
                            }
                                |> TypeExpressionResult_Progress
                                |> newState

                        Just ((TypeExpression_Bracketed _) as ty) ->
                            Error_TypeDoesNotTakeArgs ty newType
                                |> ParseResult_Err

                        Just TypeExpression_Unit ->
                            Error_TypeDoesNotTakeArgs TypeExpression_Unit newType
                                |> ParseResult_Err

                Just ( Nothing, rest ) ->
                    { bracketStack =
                        Just newType
                            |> pushOnto rest
                    , root = root
                    }
                        |> TypeExpressionResult_Progress
                        |> newState

                Just ( Just (TypeExpression_NamedType { name, args }), rest ) ->
                    { bracketStack =
                        Just
                            (TypeExpression_NamedType
                                { name = name
                                , args =
                                    TypeExpression_NamedType { name = str, args = empty }
                                        |> pushOnto args
                                }
                            )
                            |> pushOnto rest
                    , root = root
                    }
                        |> TypeExpressionResult_Progress
                        |> newState

                Just ( Just ((TypeExpression_Bracketed _) as ty), _ ) ->
                    Error_TypeDoesNotTakeArgs
                        ty
                        (TypeExpression_NamedType
                            { name = str
                            , args = empty
                            }
                        )
                        |> ParseResult_Err

                Just ( Just TypeExpression_Unit, _ ) ->
                    Error_TypeDoesNotTakeArgs
                        TypeExpression_Unit
                        (TypeExpression_NamedType
                            { name = str
                            , args = empty
                            }
                        )
                        |> ParseResult_Err

        Lexer.Sigil (Lexer.Bracket Lexer.Round role) ->
            case role of
                Lexer.Open ->
                    { bracketStack =
                        Nothing
                            |> pushOnto bracketStack
                    , root = root
                    }
                        |> TypeExpressionResult_Progress
                        |> newState

                Lexer.Close ->
                    case pop bracketStack of
                        Just ( mexpr, poppedBracketStack ) ->
                            let
                                expr =
                                    case mexpr of
                                        Just expr_ ->
                                            TypeExpression_Bracketed expr_

                                        Nothing ->
                                            TypeExpression_Unit
                            in
                            case pop poppedBracketStack of
                                Just ( newExprToAddTo, rest ) ->
                                    let
                                        rnewExpr =
                                            case newExprToAddTo of
                                                Nothing ->
                                                    expr
                                                        |> Ok

                                                Just (TypeExpression_NamedType { name, args }) ->
                                                    { name = name
                                                    , args =
                                                        expr
                                                            |> pushOnto args
                                                    }
                                                        |> TypeExpression_NamedType
                                                        |> Ok

                                                Just TypeExpression_Unit ->
                                                    Error_TypeDoesNotTakeArgs TypeExpression_Unit expr
                                                        |> Err

                                                Just ((TypeExpression_Bracketed _) as ty) ->
                                                    Error_TypeDoesNotTakeArgs ty expr
                                                        |> Err
                                    in
                                    case rnewExpr of
                                        Ok newExpr ->
                                            { bracketStack =
                                                Just newExpr
                                                    |> pushOnto rest
                                            , root = root
                                            }
                                                |> TypeExpressionResult_Progress
                                                |> newState

                                        Err e ->
                                            ParseResult_Err e

                                Nothing ->
                                    case root of
                                        Nothing ->
                                            { bracketStack = empty
                                            , root = Just expr
                                            }
                                                |> TypeExpressionResult_Progress
                                                |> newState

                                        Just (TypeExpression_NamedType { name, args }) ->
                                            { bracketStack = empty
                                            , root =
                                                Just
                                                    (TypeExpression_NamedType
                                                        { name = name
                                                        , args =
                                                            expr
                                                                |> pushOnto args
                                                        }
                                                    )
                                            }
                                                |> TypeExpressionResult_Progress
                                                |> newState

                                        Just TypeExpression_Unit ->
                                            Error_TypeDoesNotTakeArgs TypeExpression_Unit expr
                                                |> ParseResult_Err

                                        Just ((TypeExpression_Bracketed _) as ty) ->
                                            Error_TypeDoesNotTakeArgs ty expr
                                                |> ParseResult_Err

                        _ ->
                            -- TODO(harry): can we add information about the
                            -- bracket we are expecting here?
                            Error_UnmatchedBracket Lexer.Round Lexer.Close
                                |> ParseResult_Err

        Lexer.Newlines _ 0 ->
            case ( pop prevExpr.bracketStack, prevExpr.root ) of
                ( Just _, _ ) ->
                    Error_PartwayThroughTypeAlias
                        |> ParseResult_Err

                ( Nothing, Just expr ) ->
                    TypeExpressionResult_Done expr
                        |> newState

                -- ( TypeExpressionContext_Record (Just expr) ) ->
                --     Debug.todo ""
                ( Nothing, Nothing ) ->
                    Error_PartwayThroughTypeAlias
                        |> ParseResult_Err

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

        State_BlockTypeAlias (BlockTypeAlias_Completish name { bracketStack, root }) ->
            case ( pop bracketStack, root ) of
                ( Nothing, Just expr ) ->
                    { ty = name
                    , expr = partialTypeExpressionToConcreteType expr
                    }
                        |> TypeAlias
                        |> Ok
                        |> Just

                _ ->
                    Error_PartwayThroughTypeAlias
                        |> Err
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
