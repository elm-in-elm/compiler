module Stage.Parse.Contextualize exposing (..)

{-| Here we define parsers. A parser is function

    type alias Parser =
        Lexer.LexItem -> ParseResult

that takes one lexed item and produces either an error or some state (it can also
choose to skip the lexed item).

-}

import Dict
import Elm.AST.Frontend as Frontend exposing (Expr(..), LocatedExpr, LocatedPattern, Pattern(..))
import Elm.Data.Declaration
import Elm.Data.Exposing
import Elm.Data.Located as Located exposing (Located)
import Elm.Data.Module exposing (Module, ModuleType(..))
import Elm.Data.ModuleName exposing (ModuleName)
import Elm.Data.Qualifiedness as Qualifiedness exposing (PossiblyQualified(..))
import Elm.Data.Type exposing (Type)
import Elm.Data.Type.Concrete as ConcreteType exposing (ConcreteType)
import Elm.Data.TypeAnnotation exposing (TypeAnnotation)
import Elm.Data.VarName exposing (VarName)
import List.NonEmpty.Zipper exposing (prev)
import Parser exposing (oneOf)
import Stage.Parse.Lexer as Lexer exposing (LexItem(..), LexSigil(..), newlinesParser)
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
    { previousBlocks : List RunResult
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
    | BlockTypeAlias_Completish Token.TypeOrConstructor PartialTypeExpressionLeaf
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
    | TypeExpression_Bracketed PartialTypeExpression
    | TypeExpression_Tuple PartialTypeExpression PartialTypeExpression (List PartialTypeExpression)
    | TypeExpression_Record (List ( String, PartialTypeExpression ))
    | TypeExpression_Function
        { firstInput : PartialTypeExpression
        , otherInputs : List PartialTypeExpression
        , output : PartialTypeExpression
        }


type LastEntryOfRecord
    = LastEntryOfRecord_Empty
    | LastEntryOfRecord_Key String
    | LastEntryOfRecord_KeyColon String
    | LastEntryOfRecord_KeyValue String PartialTypeExpression


type alias PartialRecord =
    { firstEntries : Stack ( String, PartialTypeExpression )
    , lastEntry : LastEntryOfRecord
    }


type NestingParentType
    = NestingParentType_Bracket (Stack PartialTypeExpression)
    | NestingParentType_PartialRecord
        { firstEntries : Stack ( String, PartialTypeExpression )
        , lastEntryName : String
        }
    | NestingParentType_TypeWithArgs
        { name : String
        , args : Stack PartialTypeExpression
        }
    | NestingParentType_Function
        { firstInput : PartialTypeExpression
        , otherInputs : Stack PartialTypeExpression
        }


type NestingLeafType
    = NestingLeafType_Bracket (Stack PartialTypeExpression) (Maybe PartialTypeExpression)
    | NestingLeafType_PartialRecord PartialRecord
    | NestingLeafType_TypeWithArgs
        { name : String
        , args : Stack PartialTypeExpression
        }
    | NestingLeafType_Function
        { firstInput : PartialTypeExpression
        , otherInputs : Stack PartialTypeExpression
        , output : Maybe PartialTypeExpression
        }
    | NestingLeafType_Expr PartialTypeExpression


type alias PartialTypeExpressionLeaf =
    { parents : List NestingParentType
    , nesting : NestingLeafType
    }


type TypeExpressionResult
    = TypeExpressionResult_Progress PartialTypeExpressionLeaf
    | TypeExpressionResult_Done PartialTypeExpression


type Error
    = Error_InvalidToken Expecting
    | Error_MisplacedKeyword Keyword
    | Error_BlockStartsWithTypeOrConstructor Token.TypeOrConstructor
    | Error_TypeNameStartsWithLowerCase Token.ValueOrFunction
    | Error_UnmatchedBracket Lexer.BracketType Lexer.BracketRole
    | Error_ExpectedColonWhilstParsingRecord
    | Error_ExpectedKeyWhilstParsingRecord
    | Error_TypeDoesNotTakeArgs PartialTypeExpression PartialTypeExpression
    | Error_TypeDoesNotTakeArgs2 PartialTypeExpression
    | Error_ExtraItemAfterBlock PartialTypeExpression Lexer.LexItem
    | Error_TooManyTupleArgs
        --
        (ConcreteType PossiblyQualified)
        (ConcreteType PossiblyQualified)
        (ConcreteType PossiblyQualified)
        (ConcreteType PossiblyQualified)
        (List (ConcreteType PossiblyQualified))
    | Error_PartwayThroughTypeAlias
    | Error_Panic String


type Expecting
    = Expecting_Sigil Lexer.LexSigil
    | Expecting_Block
    | Expecting_TypeName
      -- TODO(harry): reduce number of cases where we do not know what sigil we
      -- are expecting.
    | Expecting_Unknown



-- exported functions


type alias RunResult =
    Result
        { state : State
        , item : Maybe LexItem
        , error : Error
        }
        Block


run : List LexItem -> List RunResult
run items =
    runHelp
        items
        { previousBlocks = []
        , state = State_BlockStart
        }


runHelp : List LexItem -> State_ -> List RunResult
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
                        { previousBlocks =
                            Err
                                { state = state.state
                                , item = Just item
                                , error = error
                                }
                                :: state.previousBlocks
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
                            Err
                                { state = state.state
                                , item = Just item
                                , error = Error_Panic error
                                }
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
                        (newBlock
                            |> Result.mapError
                                (\err ->
                                    { state = state.state
                                    , item = Nothing
                                    , error = err
                                    }
                                )
                        )
                            :: state.previousBlocks
                )



-- parsers


parseAnything : State -> LexItem -> ParseResult
parseAnything state =
    let
        newTypeAliasState aliasName res =
            case res of
                TypeExpressionResult_Progress expr ->
                    State_BlockTypeAlias (BlockTypeAlias_Completish aliasName expr)
                        |> ParseResult_Ok

                TypeExpressionResult_Done expr ->
                    case partialTypeExpressionToConcreteType expr of
                        Ok concreteType ->
                            { ty = aliasName
                            , expr = concreteType
                            }
                                |> TypeAlias
                                |> ParseResult_Complete

                        Err (ToConcreteTypeError_TooManyTupleArgs a b c d e) ->
                            Error_TooManyTupleArgs a b c d e
                                |> ParseResult_Err
    in
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
            Debug.todo "BlockFirstItem_Module"

        State_BlockFirstItem (BlockFirstItem_Name name) ->
            Debug.todo "BlockFirstItem_Name"

        State_BlockTypeAlias BlockTypeAlias_Keywords ->
            parseTypeAliasName

        State_BlockTypeAlias (BlockTypeAlias_Named name) ->
            parseAssignment
                (State_BlockTypeAlias (BlockTypeAlias_NamedAssigns name))

        State_BlockTypeAlias (BlockTypeAlias_NamedAssigns name) ->
            parserTypeExprFromEmpty
                (newTypeAliasState name)

        State_BlockTypeAlias (BlockTypeAlias_Completish name exprSoFar) ->
            parserTypeExpr
                (newTypeAliasState name)
                exprSoFar

        State_BlockTypeAlias (BlockTypeAlias_Complete aliasName expr) ->
            let
                rBlock =
                    case partialTypeExpressionToConcreteType expr of
                        Ok concreteType ->
                            { ty = aliasName
                            , expr = concreteType
                            }
                                |> TypeAlias
                                |> Ok

                        Err (ToConcreteTypeError_TooManyTupleArgs a b c d e) ->
                            Error_TooManyTupleArgs a b c d e
                                |> Err
            in
            \item ->
                case item of
                    Lexer.Newlines _ 0 ->
                        case rBlock of
                            Ok block ->
                                block
                                    |> ParseResult_Complete

                            Err e ->
                                ParseResult_Err e

                    Lexer.Newlines _ _ ->
                        ParseResult_Skip

                    Whitespace _ ->
                        ParseResult_Skip

                    _ ->
                        Error_ExtraItemAfterBlock expr item
                            |> ParseResult_Err

        State_BlockCustomType (BlockCustomType_Named name) ->
            parseAssignment
                (State_BlockCustomType (BlockCustomType_NamedAssigns name))

        State_BlockCustomType (BlockCustomType_NamedAssigns name) ->
            Debug.todo "BlockCustomType_NamedAssigns"


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
            ParseResult_Err (Error_InvalidToken Expecting_Block)


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
            Error_InvalidToken Expecting_TypeName
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
            Error_InvalidToken Expecting_TypeName
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
            Error_InvalidToken (Expecting_Sigil Lexer.Assign)
                |> ParseResult_Err


parserTypeExprFromEmpty :
    (TypeExpressionResult -> ParseResult)
    -> LexItem
    -> ParseResult
parserTypeExprFromEmpty newState item =
    case item of
        Lexer.Token str ->
            { parents = []
            , nesting =
                NestingLeafType_TypeWithArgs
                    { name = str
                    , args = empty
                    }
            }
                |> TypeExpressionResult_Progress
                |> newState

        Lexer.Sigil (Lexer.Bracket Lexer.Round Lexer.Open) ->
            { parents = []
            , nesting = NestingLeafType_Bracket empty Nothing
            }
                |> TypeExpressionResult_Progress
                |> newState

        Lexer.Sigil (Lexer.Bracket role Lexer.Close) ->
            Error_UnmatchedBracket role Lexer.Close
                |> ParseResult_Err

        Lexer.Sigil (Lexer.Bracket Lexer.Curly Lexer.Open) ->
            { nesting =
                NestingLeafType_PartialRecord
                    { firstEntries = empty
                    , lastEntry = LastEntryOfRecord_Empty
                    }
            , parents = []
            }
                |> TypeExpressionResult_Progress
                |> newState

        Lexer.Sigil Lexer.Colon ->
            Error_InvalidToken Expecting_Unknown
                |> ParseResult_Err

        Lexer.Sigil Lexer.Comma ->
            Error_InvalidToken Expecting_Unknown
                |> ParseResult_Err

        Lexer.Sigil Lexer.ThinArrow ->
            Error_InvalidToken Expecting_Unknown
                |> ParseResult_Err

        Lexer.Newlines _ 0 ->
            Error_PartwayThroughTypeAlias
                |> ParseResult_Err

        Lexer.Newlines _ _ ->
            ParseResult_Skip

        Lexer.Whitespace _ ->
            ParseResult_Skip

        _ ->
            Error_InvalidToken Expecting_Unknown
                |> ParseResult_Err


parserTypeExpr :
    (TypeExpressionResult -> ParseResult)
    -> PartialTypeExpressionLeaf
    -> LexItem
    -> ParseResult
parserTypeExpr newState prevExpr item =
    case item of
        Lexer.Token str ->
            exprAppend prevExpr str
                |> partialTypeExpressionToParseResult newState

        Lexer.Sigil (Lexer.Bracket Lexer.Round Lexer.Open) ->
            leafToParents prevExpr
                |> Result.map
                    (\parents ->
                        { parents = parents
                        , nesting = NestingLeafType_Bracket empty Nothing
                        }
                    )
                |> partialTypeExpressionToParseResult newState

        Lexer.Sigil (Lexer.Bracket Lexer.Round Lexer.Close) ->
            let
                collapsedLeaf =
                    autoCollapseNesting CollapseLevel_Function prevExpr
            in
            case collapsedLeaf.nesting of
                NestingLeafType_Expr expr ->
                    Error_UnmatchedBracket Lexer.Round Lexer.Close
                        |> ParseResult_Err

                NestingLeafType_TypeWithArgs { name, args } ->
                    Debug.todo "Make this state impossible"

                NestingLeafType_Bracket argStack mLastExpression ->
                    let
                        rexpr =
                            if argStack /= empty && mLastExpression == Nothing then
                                -- We have a trailing comma!
                                Error_UnmatchedBracket Lexer.Round Lexer.Close
                                    |> Err

                            else
                                let
                                    fullArgsList =
                                        (case mLastExpression of
                                            Just expr ->
                                                expr |> pushOnto argStack

                                            Nothing ->
                                                argStack
                                        )
                                            |> toList (\x -> x)
                                in
                                case fullArgsList of
                                    [] ->
                                        TypeExpression_Unit
                                            |> Ok

                                    first :: [] ->
                                        TypeExpression_Bracketed first
                                            |> Ok

                                    first :: second :: rest ->
                                        TypeExpression_Tuple first second rest
                                            |> Ok
                    in
                    case rexpr of
                        Ok expr ->
                            case collapsedLeaf.parents of
                                nesting :: grandparents ->
                                    { nesting =
                                        case nesting of
                                            NestingParentType_PartialRecord { firstEntries, lastEntryName } ->
                                                NestingLeafType_PartialRecord
                                                    { firstEntries = firstEntries
                                                    , lastEntry = LastEntryOfRecord_KeyValue lastEntryName expr
                                                    }

                                            NestingParentType_Bracket els ->
                                                NestingLeafType_Bracket els (Just expr)

                                            NestingParentType_TypeWithArgs { name, args } ->
                                                NestingLeafType_TypeWithArgs
                                                    { name = name
                                                    , args = expr |> pushOnto args
                                                    }

                                            NestingParentType_Function { firstInput } ->
                                                NestingLeafType_Function
                                                    { firstInput = firstInput
                                                    , otherInputs = empty
                                                    , output = Just expr
                                                    }
                                    , parents = grandparents
                                    }
                                        |> TypeExpressionResult_Progress
                                        |> newState

                                [] ->
                                    { nesting = NestingLeafType_Expr expr
                                    , parents = []
                                    }
                                        |> TypeExpressionResult_Progress
                                        |> newState

                        Err e ->
                            ParseResult_Err e

                NestingLeafType_PartialRecord _ ->
                    Error_InvalidToken Expecting_Unknown
                        |> ParseResult_Err

                NestingLeafType_Function { output } ->
                    case output of
                        Nothing ->
                            Error_InvalidToken Expecting_Unknown
                                |> ParseResult_Err

                        Just _ ->
                            Debug.todo "Make this state impossible"

        Lexer.Sigil (Lexer.Bracket Lexer.Curly Lexer.Open) ->
            case leafToParents prevExpr of
                Ok newParents ->
                    { nesting =
                        NestingLeafType_PartialRecord
                            { firstEntries = empty
                            , lastEntry = LastEntryOfRecord_Empty
                            }
                    , parents = newParents
                    }
                        |> TypeExpressionResult_Progress
                        |> newState

                Err e ->
                    e
                        |> ParseResult_Err

        Lexer.Sigil Lexer.Colon ->
            appendColonTo prevExpr
                |> partialTypeExpressionToParseResult newState

        Lexer.Sigil Lexer.Comma ->
            appendCommaTo prevExpr
                |> partialTypeExpressionToParseResult newState

        Lexer.Sigil (Lexer.Bracket Lexer.Curly Lexer.Close) ->
            let
                collapsedLeaf =
                    autoCollapseNesting CollapseLevel_Function prevExpr
            in
            case collapsedLeaf.nesting of
                NestingLeafType_Expr expr ->
                    Error_UnmatchedBracket Lexer.Curly Lexer.Close
                        |> ParseResult_Err

                NestingLeafType_TypeWithArgs { name, args } ->
                    Debug.todo "Make this state impossible"

                NestingLeafType_Bracket argStack mLastExpression ->
                    Error_InvalidToken Expecting_Unknown
                        |> ParseResult_Err

                NestingLeafType_PartialRecord { firstEntries, lastEntry } ->
                    let
                        fromRecord recordEntries =
                            let
                                record =
                                    TypeExpression_Record recordEntries
                            in
                            case collapsedLeaf.parents of
                                [] ->
                                    { nesting = NestingLeafType_Expr record
                                    , parents = []
                                    }
                                        |> TypeExpressionResult_Progress
                                        |> newState

                                nesting :: grandparents ->
                                    (case nesting of
                                        -- We are within a nested bracket.
                                        NestingParentType_Bracket argStack ->
                                            { parents = grandparents
                                            , nesting = NestingLeafType_Bracket argStack (Just record)
                                            }

                                        NestingParentType_PartialRecord existingPartialRecord ->
                                            { parents = grandparents
                                            , nesting =
                                                NestingLeafType_PartialRecord
                                                    { firstEntries = existingPartialRecord.firstEntries
                                                    , lastEntry =
                                                        LastEntryOfRecord_KeyValue
                                                            existingPartialRecord.lastEntryName
                                                            record
                                                    }
                                            }

                                        NestingParentType_TypeWithArgs { name, args } ->
                                            { nesting =
                                                NestingLeafType_TypeWithArgs
                                                    { name = name
                                                    , args =
                                                        record
                                                            |> pushOnto args
                                                    }
                                            , parents = grandparents
                                            }

                                        NestingParentType_Function { firstInput, otherInputs } ->
                                            { nesting =
                                                NestingLeafType_Function
                                                    { firstInput = firstInput
                                                    , otherInputs = otherInputs
                                                    , output = Just record
                                                    }
                                            , parents = grandparents
                                            }
                                    )
                                        |> TypeExpressionResult_Progress
                                        |> newState
                    in
                    case lastEntry of
                        LastEntryOfRecord_KeyValue key value ->
                            ( key, value )
                                |> pushOnto firstEntries
                                |> toList (\x -> x)
                                |> fromRecord

                        LastEntryOfRecord_Empty ->
                            if firstEntries == empty then
                                []
                                    |> fromRecord

                            else
                                Error_InvalidToken Expecting_Unknown
                                    |> ParseResult_Err

                        _ ->
                            Error_InvalidToken Expecting_Unknown
                                |> ParseResult_Err

                NestingLeafType_Function { output } ->
                    case output of
                        Nothing ->
                            Error_InvalidToken Expecting_Unknown
                                |> ParseResult_Err

                        Just _ ->
                            Debug.todo "Make this state impossible"

        Lexer.Sigil Lexer.ThinArrow ->
            let
                getNewPartialRecord parents { firstEntries, lastEntry } =
                    case lastEntry of
                        LastEntryOfRecord_KeyValue key value ->
                            { parents = parents
                            , nesting =
                                NestingLeafType_PartialRecord
                                    { firstEntries = ( key, value ) |> pushOnto firstEntries
                                    , lastEntry = LastEntryOfRecord_Empty
                                    }
                            }
                                |> TypeExpressionResult_Progress
                                |> newState

                        _ ->
                            Error_InvalidToken Expecting_Unknown
                                |> ParseResult_Err
            in
            case (autoCollapseNesting CollapseLevel_TypeWithArgs prevExpr).nesting of
                NestingLeafType_Expr expr ->
                    { nesting =
                        NestingLeafType_Function
                            { firstInput = expr
                            , otherInputs = empty
                            , output = Nothing
                            }
                    , parents = []
                    }
                        |> TypeExpressionResult_Progress
                        |> newState

                NestingLeafType_Function { firstInput, otherInputs, output } ->
                    case output of
                        Nothing ->
                            Error_InvalidToken Expecting_Unknown
                                |> ParseResult_Err

                        Just output_ ->
                            { nesting =
                                NestingLeafType_Function
                                    { firstInput = firstInput
                                    , otherInputs = output_ |> pushOnto otherInputs
                                    , output = Nothing
                                    }
                            , parents = []
                            }
                                |> TypeExpressionResult_Progress
                                |> newState

                _ ->
                    Error_InvalidToken Expecting_Unknown
                        |> ParseResult_Err

        Lexer.Newlines _ 0 ->
            case (autoCollapseNesting CollapseLevel_Function prevExpr).nesting of
                NestingLeafType_Expr expr ->
                    TypeExpressionResult_Done expr
                        |> newState

                _ ->
                    Error_PartwayThroughTypeAlias
                        |> ParseResult_Err

        Lexer.Newlines _ _ ->
            ParseResult_Skip

        Lexer.Whitespace _ ->
            ParseResult_Skip

        _ ->
            Error_InvalidToken Expecting_Unknown
                |> ParseResult_Err


leafToParents : PartialTypeExpressionLeaf -> Result Error (List NestingParentType)
leafToParents { parents, nesting } =
    (case nesting of
        NestingLeafType_Expr expr ->
            -- Cannot nest unless there is a trailing comma!
            Error_TypeDoesNotTakeArgs2 expr
                |> Err

        NestingLeafType_Bracket _ (Just lastType) ->
            -- Cannot nest unless there is a trailing comma!
            Error_TypeDoesNotTakeArgs2 lastType
                |> Err

        NestingLeafType_Bracket els Nothing ->
            NestingParentType_Bracket els
                |> Ok

        NestingLeafType_PartialRecord { firstEntries, lastEntry } ->
            case lastEntry of
                LastEntryOfRecord_Empty ->
                    Error_ExpectedKeyWhilstParsingRecord
                        |> Err

                LastEntryOfRecord_Key _ ->
                    Error_ExpectedColonWhilstParsingRecord
                        |> Err

                LastEntryOfRecord_KeyColon key ->
                    NestingParentType_PartialRecord { firstEntries = firstEntries, lastEntryName = key }
                        |> Ok

                LastEntryOfRecord_KeyValue _ lastValueType ->
                    Error_TypeDoesNotTakeArgs2 lastValueType
                        |> Err

        NestingLeafType_TypeWithArgs details ->
            NestingParentType_TypeWithArgs details
                |> Ok

        NestingLeafType_Function { firstInput, otherInputs, output } ->
            case output of
                Nothing ->
                    NestingParentType_Function
                        { firstInput = firstInput
                        , otherInputs = otherInputs
                        }
                        |> Ok

                Just te ->
                    Error_TypeDoesNotTakeArgs2 te
                        |> Err
    )
        |> Result.map
            (\n -> n :: parents)


exprAppend :
    PartialTypeExpressionLeaf
    -> String
    -> Result Error PartialTypeExpressionLeaf
exprAppend ({ parents, nesting } as currentLeaf) token =
    let
        newType =
            TypeExpression_NamedType
                { name = token
                , args = empty
                }
    in
    case nesting of
        -- We are within a nested bracket.
        NestingLeafType_Bracket argStack mostNested ->
            case mostNested of
                Nothing ->
                    leafToParents currentLeaf
                        |> Result.map
                            (\newParents ->
                                { parents = newParents
                                , nesting =
                                    NestingLeafType_TypeWithArgs
                                        { name = token
                                        , args = empty
                                        }
                                }
                            )

                Just existingRoot ->
                    Error_TypeDoesNotTakeArgs TypeExpression_Unit newType
                        |> Err

        NestingLeafType_Expr expr ->
            Error_TypeDoesNotTakeArgs expr newType
                |> Err

        NestingLeafType_PartialRecord pr ->
            case pr.lastEntry of
                LastEntryOfRecord_Empty ->
                    { parents = parents
                    , nesting =
                        NestingLeafType_PartialRecord
                            { firstEntries = pr.firstEntries
                            , lastEntry = LastEntryOfRecord_Key token
                            }
                    }
                        |> Ok

                _ ->
                    leafToParents currentLeaf
                        |> Result.map
                            (\newParents ->
                                { parents = newParents
                                , nesting =
                                    NestingLeafType_TypeWithArgs
                                        { name = token
                                        , args = empty
                                        }
                                }
                            )

        NestingLeafType_TypeWithArgs { name, args } ->
            { nesting =
                NestingLeafType_TypeWithArgs
                    { name = name
                    , args =
                        newType
                            |> pushOnto args
                    }
            , parents = parents
            }
                |> Ok

        (NestingLeafType_Function { firstInput, output }) as lt ->
            Result.map2
                (\newOutput newParents ->
                    { parents = newParents
                    , nesting =
                        NestingLeafType_TypeWithArgs
                            { name = token
                            , args = empty
                            }
                    }
                )
                (case output of
                    Just outputExpr ->
                        Error_TypeDoesNotTakeArgs outputExpr newType
                            |> Err

                    Nothing ->
                        NestingLeafType_TypeWithArgs
                            { name = token
                            , args = empty
                            }
                            |> Ok
                )
                (leafToParents currentLeaf)


appendCommaTo : PartialTypeExpressionLeaf -> Result Error PartialTypeExpressionLeaf
appendCommaTo prevExpr =
    let
        collapsedLeaf =
            autoCollapseNesting CollapseLevel_Function prevExpr
    in
    case collapsedLeaf.nesting of
        NestingLeafType_PartialRecord { firstEntries, lastEntry } ->
            case lastEntry of
                LastEntryOfRecord_KeyValue key value ->
                    { parents = collapsedLeaf.parents
                    , nesting =
                        NestingLeafType_PartialRecord
                            { firstEntries = ( key, value ) |> pushOnto firstEntries
                            , lastEntry = LastEntryOfRecord_Empty
                            }
                    }
                        |> Ok

                _ ->
                    Error_InvalidToken Expecting_Unknown
                        |> Err

        NestingLeafType_Bracket argStack (Just expr) ->
            { nesting =
                NestingLeafType_Bracket (expr |> pushOnto argStack) Nothing
            , parents = collapsedLeaf.parents
            }
                |> Ok

        _ ->
            Error_InvalidToken Expecting_Unknown
                |> Err


appendColonTo : PartialTypeExpressionLeaf -> Result Error PartialTypeExpressionLeaf
appendColonTo prevExpr =
    case prevExpr.nesting of
        NestingLeafType_PartialRecord { firstEntries, lastEntry } ->
            case lastEntry of
                LastEntryOfRecord_Key key ->
                    { parents = prevExpr.parents
                    , nesting =
                        NestingLeafType_PartialRecord
                            { firstEntries = firstEntries
                            , lastEntry = LastEntryOfRecord_KeyColon key
                            }
                    }
                        |> Ok

                _ ->
                    Error_InvalidToken Expecting_Unknown
                        |> Err

        _ ->
            Error_InvalidToken Expecting_Unknown
                |> Err


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

        State_BlockTypeAlias (BlockTypeAlias_Completish aliasName partialExpr) ->
            case (autoCollapseNesting CollapseLevel_Function partialExpr).nesting of
                NestingLeafType_Expr expr ->
                    partialTypeExpressionToConcreteType expr
                        |> Result.map
                            (\conceteType ->
                                { ty = aliasName
                                , expr = conceteType
                                }
                                    |> TypeAlias
                            )
                        |> Result.mapError (\(ToConcreteTypeError_TooManyTupleArgs a b c d e) -> Error_TooManyTupleArgs a b c d e)
                        |> Just

                _ ->
                    Error_PartwayThroughTypeAlias
                        |> Err
                        |> Just

        State_BlockTypeAlias (BlockTypeAlias_Complete aliasName expr) ->
            case partialTypeExpressionToConcreteType expr of
                Ok concreteType ->
                    { ty = aliasName
                    , expr = concreteType
                    }
                        |> TypeAlias
                        |> Ok
                        |> Just

                Err (ToConcreteTypeError_TooManyTupleArgs a b c d e) ->
                    Error_TooManyTupleArgs a b c d e
                        |> Err
                        |> Just

        State_BlockCustomType firstItem ->
            Debug.todo "handle incomplete block"



-- helper functions


type TokenOrType
    = TokenOrType_Token String


addToPartialRecord :
    String
    -> PartialRecord
    -> Result Error PartialRecord
addToPartialRecord token { firstEntries, lastEntry } =
    let
        newType =
            TypeExpression_NamedType
                { name = token
                , args = empty
                }
    in
    case lastEntry of
        LastEntryOfRecord_Empty ->
            { firstEntries = firstEntries
            , lastEntry =
                LastEntryOfRecord_Key token
            }
                |> Ok

        LastEntryOfRecord_Key key ->
            Error_ExpectedColonWhilstParsingRecord
                |> Err

        LastEntryOfRecord_KeyColon key ->
            { firstEntries = firstEntries
            , lastEntry =
                LastEntryOfRecord_KeyValue key newType
            }
                |> Ok

        LastEntryOfRecord_KeyValue key value ->
            Error_TypeDoesNotTakeArgs value newType
                |> Err


type CollapseLevel
    = CollapseLevel_TypeWithArgs
    | CollapseLevel_Function


autoCollapseNesting : CollapseLevel -> PartialTypeExpressionLeaf -> PartialTypeExpressionLeaf
autoCollapseNesting collapseLevel pte =
    case pte.nesting of
        NestingLeafType_TypeWithArgs { name, args } ->
            let
                newTypeExpr =
                    TypeExpression_NamedType { name = name, args = args }
            in
            case pte.parents of
                nesting :: grandparents ->
                    { parents = grandparents
                    , nesting =
                        case nesting of
                            NestingParentType_TypeWithArgs _ ->
                                Debug.todo "Make this state impossible"

                            NestingParentType_Function { firstInput, otherInputs } ->
                                NestingLeafType_Function
                                    { firstInput = firstInput
                                    , otherInputs = otherInputs
                                    , output = Just newTypeExpr
                                    }

                            NestingParentType_Bracket els ->
                                NestingLeafType_Bracket els (Just newTypeExpr)

                            NestingParentType_PartialRecord { firstEntries, lastEntryName } ->
                                NestingLeafType_PartialRecord
                                    { firstEntries = firstEntries
                                    , lastEntry = LastEntryOfRecord_KeyValue lastEntryName newTypeExpr
                                    }
                    }
                        |> autoCollapseNesting collapseLevel

                [] ->
                    { nesting = NestingLeafType_Expr newTypeExpr
                    , parents = []
                    }

        NestingLeafType_Expr _ ->
            pte

        NestingLeafType_Bracket _ _ ->
            pte

        NestingLeafType_PartialRecord _ ->
            pte

        NestingLeafType_Function { firstInput, otherInputs, output } ->
            case ( collapseLevel, output ) of
                ( CollapseLevel_TypeWithArgs, _ ) ->
                    pte

                ( CollapseLevel_Function, Nothing ) ->
                    pte

                ( CollapseLevel_Function, Just outputExpr ) ->
                    let
                        newTypeExpr =
                            TypeExpression_Function
                                { firstInput = firstInput
                                , otherInputs = otherInputs |> toList (\x -> x)
                                , output = outputExpr
                                }
                    in
                    case pte.parents of
                        nesting :: grandparents ->
                            { parents = grandparents
                            , nesting =
                                case nesting of
                                    NestingParentType_TypeWithArgs _ ->
                                        Debug.todo "Make this state impossible"

                                    NestingParentType_Function _ ->
                                        Debug.todo "Make this state impossible"

                                    NestingParentType_Bracket els ->
                                        NestingLeafType_Bracket els (Just newTypeExpr)

                                    NestingParentType_PartialRecord { firstEntries, lastEntryName } ->
                                        NestingLeafType_PartialRecord
                                            { firstEntries = firstEntries
                                            , lastEntry = LastEntryOfRecord_KeyValue lastEntryName newTypeExpr
                                            }
                            }
                                |> autoCollapseNesting collapseLevel

                        [] ->
                            { nesting = NestingLeafType_Expr newTypeExpr
                            , parents = []
                            }


{-| TODO(harry): We can add things to a tuple too! Rename this function
appropriately.

TODO(harry): We can inline this function.

-}
partialTypeExpressionToParseResult : (TypeExpressionResult -> ParseResult) -> Result Error PartialTypeExpressionLeaf -> ParseResult
partialTypeExpressionToParseResult newState rnewPartialType =
    case rnewPartialType of
        Ok newPartialType ->
            TypeExpressionResult_Progress newPartialType
                |> newState

        Err e ->
            ParseResult_Err e


type ToConcreteTypeError
    = ToConcreteTypeError_TooManyTupleArgs
        --
        (ConcreteType PossiblyQualified)
        (ConcreteType PossiblyQualified)
        (ConcreteType PossiblyQualified)
        (ConcreteType PossiblyQualified)
        (List (ConcreteType PossiblyQualified))


{-| TODO(harry): custom error message here
-}
partialTypeExpressionToConcreteType : PartialTypeExpression -> Result ToConcreteTypeError (ConcreteType PossiblyQualified)
partialTypeExpressionToConcreteType pte =
    case pte of
        TypeExpression_NamedType { name, args } ->
            args
                |> toList partialTypeExpressionToConcreteType
                |> collectList (\x -> x)
                |> Result.map
                    (\goodArgs ->
                        { qualifiedness = Qualifiedness.PossiblyQualified Nothing
                        , name = name
                        , args = goodArgs
                        }
                            |> ConcreteType.UserDefinedType
                    )

        TypeExpression_Unit ->
            ConcreteType.Unit
                |> Ok

        TypeExpression_Bracketed ty ->
            partialTypeExpressionToConcreteType ty

        TypeExpression_Tuple first second [] ->
            Result.map2
                ConcreteType.Tuple
                (partialTypeExpressionToConcreteType first)
                (partialTypeExpressionToConcreteType second)

        TypeExpression_Tuple first second (third :: []) ->
            Result.map3
                ConcreteType.Tuple3
                (partialTypeExpressionToConcreteType first)
                (partialTypeExpressionToConcreteType second)
                (partialTypeExpressionToConcreteType third)

        TypeExpression_Tuple first second (third :: fouth :: rest) ->
            Result.map5
                ToConcreteTypeError_TooManyTupleArgs
                (partialTypeExpressionToConcreteType first)
                (partialTypeExpressionToConcreteType second)
                (partialTypeExpressionToConcreteType third)
                (partialTypeExpressionToConcreteType fouth)
                (rest
                    |> collectList partialTypeExpressionToConcreteType
                )
                |> Result.andThen Err

        TypeExpression_Record keyValues ->
            keyValues
                |> collectList
                    (\( key, value ) ->
                        partialTypeExpressionToConcreteType value
                            |> Result.map (\concreteValue -> ( key, concreteValue ))
                    )
                |> Result.map
                    (\goodKeyValues ->
                        Dict.fromList goodKeyValues
                            |> ConcreteType.Record
                    )

        TypeExpression_Function functionTypeExpr ->
            let
                folder :
                    ConcreteType a
                    -> (ConcreteType a -> ConcreteType a)
                    -> (ConcreteType a -> ConcreteType a)
                folder nextArg createConcreteFunction someOutput =
                    createConcreteFunction
                        (ConcreteType.Function
                            { from = nextArg
                            , to = someOutput
                            }
                        )
            in
            Result.map3
                (\concreteFirstInput concreteOtherInputs concreteOutput ->
                    ConcreteType.Function
                        { from = concreteFirstInput
                        , to =
                            List.foldl
                                (\arg te ->
                                    ConcreteType.Function
                                        { from = arg
                                        , to = te
                                        }
                                )
                                concreteOutput
                                concreteOtherInputs
                        }
                )
                (partialTypeExpressionToConcreteType functionTypeExpr.firstInput)
                (functionTypeExpr.otherInputs
                    |> List.reverse
                    |> collectList partialTypeExpressionToConcreteType
                )
                (partialTypeExpressionToConcreteType functionTypeExpr.output)


recoverErrors : ParseResult -> ParseResult
recoverErrors res =
    case res of
        ParseResult_Err _ ->
            ParseResult_Ok State_Error_Recovery

        _ ->
            res


collectList : (a -> Result e o) -> List a -> Result e (List o)
collectList =
    collectListHelp []


collectListHelp : List o -> (a -> Result e o) -> List a -> Result e (List o)
collectListHelp new func old =
    case old of
        curr :: rest ->
            case func curr of
                Ok o ->
                    collectListHelp (o :: new) func rest

                Err e ->
                    Err e

        [] ->
            Ok (List.reverse new)


parseResultFromMaybeResult : Maybe (Result Error State) -> ParseResult
parseResultFromMaybeResult x =
    case x of
        Just (Ok s) ->
            ParseResult_Ok s

        Just (Err e) ->
            ParseResult_Err e

        Nothing ->
            ParseResult_Skip



-- stacks


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


reverseToList : Stack a -> List a
reverseToList (Stack ls) =
    ls
