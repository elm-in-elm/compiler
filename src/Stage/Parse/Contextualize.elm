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
import Elm.Data.Operator as Operator exposing (Operator)
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
    | ValueDeclaration
        { name : Located Token.ValueOrFunctionOrGenericType

        -- TODO(harry): these could be patterns!
        , args : List (Located Token.ValueOrFunctionOrGenericType)

        -- This key's name is hard coded into parser-tests/Update.elm
        , valueExpr__ : Frontend.LocatedExpr
        }
    | TypeAlias
        { ty : Token.TypeOrConstructor
        , genericArgs : List Token.ValueOrFunctionOrGenericType
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
    | State_BlockValueDeclaration BlockValueDeclaration


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


type BlockTypeAlias
    = BlockTypeAlias_Keywords
    | BlockTypeAlias_Named Token.TypeOrConstructor (Stack Token.ValueOrFunctionOrGenericType)
    | BlockTypeAlias_NamedAssigns Token.TypeOrConstructor (List Token.ValueOrFunctionOrGenericType)
    | BlockTypeAlias_Completish Token.TypeOrConstructor (List Token.ValueOrFunctionOrGenericType) PartialTypeExpressionLeaf


type BlockCustomType
    = BlockCustomType_Named Token.TypeOrConstructor (Stack Token.ValueOrFunctionOrGenericType)
    | BlockCustomType_NamedAssigns Token.TypeOrConstructor (List Token.ValueOrFunctionOrGenericType)


type BlockValueDeclaration
    = BlockValueDeclaration_Named
        { name : Located Token.ValueOrFunctionOrGenericType
        , args : Stack (Located Token.ValueOrFunctionOrGenericType)
        }
    | BlockValueDeclaration_NamedAssigns
        { name : Located Token.ValueOrFunctionOrGenericType
        , args : List (Located Token.ValueOrFunctionOrGenericType)
        }
    | BlockValueDeclaration_Completish
        { name : Located Token.ValueOrFunctionOrGenericType
        , args : List (Located Token.ValueOrFunctionOrGenericType)
        , partialExpr : ExpressionNestingLeaf
        }



-- TYPE EXPRESSIONS --


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


type PartialResult progress done
    = PartialResult_Progress progress
    | PartialResult_Done done


type alias TypeExpressionResult =
    PartialResult PartialTypeExpressionLeaf PartialTypeExpression



-- EXPRESSIONS --


type ExpressionNestingParent
    = ExpressionNestingParent_Operator
        { op : Operator
        , lhs : Frontend.LocatedExpr
        , parent : Maybe ExpressionNestingParent
        }


type ExpressionNestingLeaf
    = ExpressionNestingLeaf_Operator
        { op : Operator
        , lhs : Frontend.LocatedExpr
        , rhs : Maybe Frontend.LocatedExpr
        , parent : Maybe ExpressionNestingParent
        }
    | ExpressionNestingLeafType_Expr Frontend.LocatedExpr


type alias ExpressionResult =
    PartialResult ExpressionNestingLeaf Frontend.LocatedExpr


type Error
    = Error_InvalidToken Expecting
    | Error_MisplacedKeyword Keyword
    | Error_BlockStartsWithTypeOrConstructor Token.TypeOrConstructor
      -- Type Expressions --
    | Error_TypeNameStartsWithLowerCase Token.ValueOrFunctionOrGenericType
    | Error_UnmatchedBracket Lexer.BracketType Lexer.BracketRole
    | Error_WrongClosingBracket
        { expecting : Lexer.BracketType
        , found : Lexer.BracketType
        }
    | Error_MissingFunctionReturnType
    | Error_ExpectedColonWhilstParsingRecord
    | Error_ExpectedKeyWhilstParsingRecord
    | Error_TypeDoesNotTakeArgs PartialTypeExpression PartialTypeExpression
    | Error_TypeDoesNotTakeArgs2 PartialTypeExpression
    | Error_ValueDoesNotTakeArgs Frontend.LocatedExpr
    | Error_ExtraItemAfterBlock PartialTypeExpression Lexer.LexItem
    | Error_TooManyTupleArgs
        --
        (ConcreteType PossiblyQualified)
        (ConcreteType PossiblyQualified)
        (ConcreteType PossiblyQualified)
        (ConcreteType PossiblyQualified)
        (List (ConcreteType PossiblyQualified))
      -- Expressions --
    | Error_InvalidNumericLiteral String
      -- Incomplete parsing --
    | Error_PartwayThroughTypeAlias
    | Error_PartwayThroughValueDeclaration
      -- Bugs in the parser --
    | Error_Panic String


type Expecting
    = Expecting_Sigil Lexer.LexSigil
    | Expecting_Block
    | Expecting_TypeName
    | Expecting_Identifier
      -- TODO(harry): reduce number of cases where we do not know what sigil we
      -- are expecting.
    | Expecting_Unknown



-- exported functions


type alias RunResult =
    Result
        { state : State
        , item : Maybe (Located LexItem)
        , error : Error
        }
        Block


run : List (Located LexItem) -> List RunResult
run items =
    runHelp
        items
        { previousBlocks = []
        , state = State_BlockStart
        }


runHelp : List (Located LexItem) -> State_ -> List RunResult
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
                            case Located.unwrap item of
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


parseAnything : State -> Located LexItem -> ParseResult
parseAnything state =
    let
        newTypeAliasState aliasName typeArgs res =
            case res of
                PartialResult_Progress expr ->
                    State_BlockTypeAlias (BlockTypeAlias_Completish aliasName typeArgs expr)
                        |> ParseResult_Ok

                PartialResult_Done expr ->
                    case partialTypeExpressionToConcreteType expr of
                        Ok concreteType ->
                            { ty = aliasName
                            , genericArgs = typeArgs
                            , expr = concreteType
                            }
                                |> TypeAlias
                                |> ParseResult_Complete

                        Err (ToConcreteTypeError_TooManyTupleArgs a b c d e) ->
                            Error_TooManyTupleArgs a b c d e
                                |> ParseResult_Err

        newExpressionState name args res =
            case res of
                PartialResult_Progress expr ->
                    State_BlockValueDeclaration
                        (BlockValueDeclaration_Completish
                            { name = name
                            , args = args
                            , partialExpr = expr
                            }
                        )
                        |> ParseResult_Ok

                PartialResult_Done expr ->
                    { name = name
                    , args = args
                    , valueExpr__ = expr
                    }
                        |> ValueDeclaration
                        |> ParseResult_Complete
    in
    case state of
        State_Error_Recovery ->
            \item ->
                case Located.unwrap item of
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

        State_BlockValueDeclaration (BlockValueDeclaration_Named { name, args }) ->
            parseLowercaseArgsOrAssignment
                (\newArg ->
                    State_BlockValueDeclaration
                        (BlockValueDeclaration_Named
                            { name = name
                            , args = newArg |> pushOnto args
                            }
                        )
                )
                (State_BlockValueDeclaration
                    (BlockValueDeclaration_NamedAssigns
                        { name = name
                        , args = args |> toList (\x -> x)
                        }
                    )
                )

        State_BlockValueDeclaration (BlockValueDeclaration_NamedAssigns { name, args }) ->
            parserExpressionFromEmpty
                (newExpressionState name args)

        State_BlockValueDeclaration (BlockValueDeclaration_Completish { name, args, partialExpr }) ->
            parserExpression
                (newExpressionState name args)
                partialExpr

        State_BlockTypeAlias BlockTypeAlias_Keywords ->
            parseTypeAliasName

        State_BlockTypeAlias (BlockTypeAlias_Named name typeArgs) ->
            parseLowercaseArgsOrAssignment
                (\newTypeArg ->
                    State_BlockTypeAlias
                        (BlockTypeAlias_Named
                            name
                            (Located.unwrap newTypeArg |> pushOnto typeArgs)
                        )
                )
                (State_BlockTypeAlias
                    (BlockTypeAlias_NamedAssigns
                        name
                        (typeArgs |> toList (\x -> x))
                    )
                )

        State_BlockTypeAlias (BlockTypeAlias_NamedAssigns name typeArgs) ->
            parserTypeExprFromEmpty
                (newTypeAliasState name typeArgs)

        State_BlockTypeAlias (BlockTypeAlias_Completish name typeArgs exprSoFar) ->
            parserTypeExpr
                (newTypeAliasState name typeArgs)
                exprSoFar

        State_BlockCustomType (BlockCustomType_Named name typeArgs) ->
            parseLowercaseArgsOrAssignment
                (\newTypeArg ->
                    State_BlockCustomType
                        (BlockCustomType_Named
                            name
                            (Located.unwrap newTypeArg |> pushOnto typeArgs)
                        )
                )
                (State_BlockCustomType
                    (BlockCustomType_NamedAssigns
                        name
                        (typeArgs |> toList (\x -> x))
                    )
                )

        State_BlockCustomType (BlockCustomType_NamedAssigns name typeArgs) ->
            Debug.todo "BlockCustomType_NamedAssigns"


{-|


### Panics

If the LexItem is a `Newlines` with indentation or is `Whitespace`.

-}
parseBlockStart : Located LexItem -> ParseResult
parseBlockStart item =
    let
        withCorrectLocation x =
            Located.map (\_ -> x) item
    in
    case Located.unwrap item of
        Lexer.Token str ->
            case Token.classifyToken str of
                Token.TokenKeyword Token.Type ->
                    ParseResult_Ok (State_BlockFirstItem BlockFirstItem_Type)

                Token.TokenKeyword Token.Module ->
                    ParseResult_Ok (State_BlockFirstItem BlockFirstItem_Module)

                Token.TokenKeyword other ->
                    ParseResult_Err (Error_MisplacedKeyword other)

                Token.TokenValueOrFunction valOrFunc ->
                    ParseResult_Ok
                        (State_BlockValueDeclaration
                            (BlockValueDeclaration_Named
                                { name = withCorrectLocation valOrFunc
                                , args = empty
                                }
                            )
                        )

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


parseTypeBlock : Located LexItem -> ParseResult
parseTypeBlock item =
    case Located.unwrap item of
        Lexer.Token str ->
            case Token.classifyToken str of
                Token.TokenKeyword Token.Alias ->
                    State_BlockTypeAlias BlockTypeAlias_Keywords
                        |> ParseResult_Ok

                Token.TokenKeyword other ->
                    Error_MisplacedKeyword other
                        |> ParseResult_Err

                Token.TokenTypeOrConstructor typeOrConstructor ->
                    State_BlockCustomType (BlockCustomType_Named typeOrConstructor empty)
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


parseTypeAliasName : Located LexItem -> ParseResult
parseTypeAliasName item =
    case Located.unwrap item of
        Lexer.Token str ->
            case Token.classifyToken str of
                Token.TokenKeyword other ->
                    Error_MisplacedKeyword other
                        |> ParseResult_Err

                Token.TokenTypeOrConstructor typeOrConstructor ->
                    State_BlockTypeAlias (BlockTypeAlias_Named typeOrConstructor empty)
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


parseLowercaseArgsOrAssignment : (Located Token.ValueOrFunctionOrGenericType -> State) -> State -> Located LexItem -> ParseResult
parseLowercaseArgsOrAssignment onTypeArg onAssignment item =
    let
        withCorrectLocation x =
            Located.map (\_ -> x) item
    in
    case Located.unwrap item of
        Lexer.Token str ->
            case Token.classifyToken str of
                Token.TokenKeyword kw ->
                    Error_MisplacedKeyword kw
                        |> ParseResult_Err

                Token.TokenTypeOrConstructor _ ->
                    Error_InvalidToken (Expecting_Sigil Lexer.Assign)
                        |> ParseResult_Err

                Token.TokenValueOrFunction argName ->
                    onTypeArg (withCorrectLocation argName)
                        |> ParseResult_Ok

        Lexer.Sigil Lexer.Assign ->
            onAssignment
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
    -> Located LexItem
    -> ParseResult
parserTypeExprFromEmpty newState item =
    case Located.unwrap item of
        Lexer.Token str ->
            { parents = []
            , nesting =
                NestingLeafType_TypeWithArgs
                    { name = str
                    , args = empty
                    }
            }
                |> PartialResult_Progress
                |> newState

        Lexer.Sigil (Lexer.Bracket Lexer.Round Lexer.Open) ->
            { parents = []
            , nesting = NestingLeafType_Bracket empty Nothing
            }
                |> PartialResult_Progress
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
                |> PartialResult_Progress
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
    -> Located LexItem
    -> ParseResult
parserTypeExpr newState prevExpr item =
    case Located.unwrap item of
        Lexer.Token str ->
            exprAppend prevExpr str
                |> partialExpressionToParseResult newState

        Lexer.Sigil (Lexer.Bracket Lexer.Round Lexer.Open) ->
            leafToParents prevExpr
                |> Result.map
                    (\parents ->
                        { parents = parents
                        , nesting = NestingLeafType_Bracket empty Nothing
                        }
                    )
                |> partialExpressionToParseResult newState

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
                    closeBracket argStack mLastExpression collapsedLeaf.parents
                        |> partialExpressionToParseResult newState

                NestingLeafType_PartialRecord _ ->
                    Error_WrongClosingBracket
                        { expecting = Lexer.Curly
                        , found = Lexer.Round
                        }
                        |> ParseResult_Err

                NestingLeafType_Function { output } ->
                    case output of
                        Nothing ->
                            Error_MissingFunctionReturnType
                                |> ParseResult_Err

                        Just _ ->
                            Debug.todo "Make this state impossible"

        Lexer.Sigil (Lexer.Bracket Lexer.Curly Lexer.Open) ->
            leafToParents prevExpr
                |> Result.map
                    (\newParents ->
                        { nesting =
                            NestingLeafType_PartialRecord
                                { firstEntries = empty
                                , lastEntry = LastEntryOfRecord_Empty
                                }
                        , parents = newParents
                        }
                    )
                |> partialExpressionToParseResult newState

        Lexer.Sigil Lexer.Colon ->
            appendColonTo prevExpr
                |> partialExpressionToParseResult newState

        Lexer.Sigil Lexer.Comma ->
            appendCommaTo prevExpr
                |> partialExpressionToParseResult newState

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
                    Error_WrongClosingBracket
                        { expecting = Lexer.Round
                        , found = Lexer.Curly
                        }
                        |> ParseResult_Err

                NestingLeafType_PartialRecord pr ->
                    closeRecord pr collapsedLeaf.parents
                        |> partialExpressionToParseResult newState

                NestingLeafType_Function { output } ->
                    case output of
                        Nothing ->
                            Error_MissingFunctionReturnType
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
                                |> PartialResult_Progress
                                |> newState

                        _ ->
                            Error_InvalidToken Expecting_Unknown
                                |> ParseResult_Err

                collapsedLeaf =
                    autoCollapseNesting CollapseLevel_TypeWithArgs prevExpr
            in
            case collapsedLeaf.nesting of
                NestingLeafType_Expr expr ->
                    { nesting =
                        NestingLeafType_Function
                            { firstInput = expr
                            , otherInputs = empty
                            , output = Nothing
                            }
                    , parents = []
                    }
                        |> PartialResult_Progress
                        |> newState

                NestingLeafType_TypeWithArgs {} ->
                    Debug.todo "make state impossible"

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
                            , parents = collapsedLeaf.parents
                            }
                                |> PartialResult_Progress
                                |> newState

                NestingLeafType_Bracket argStack (Just expr) ->
                    { nesting =
                        NestingLeafType_Function
                            { firstInput = expr
                            , otherInputs = empty
                            , output = Nothing
                            }
                    , parents = NestingParentType_Bracket argStack :: collapsedLeaf.parents
                    }
                        |> PartialResult_Progress
                        |> newState

                NestingLeafType_Bracket argStack Nothing ->
                    Error_InvalidToken Expecting_Unknown
                        |> ParseResult_Err

                NestingLeafType_PartialRecord { firstEntries, lastEntry } ->
                    case lastEntry of
                        LastEntryOfRecord_Empty ->
                            Error_InvalidToken Expecting_Identifier
                                |> ParseResult_Err

                        LastEntryOfRecord_Key _ ->
                            Error_InvalidToken (Expecting_Sigil Lexer.Colon)
                                |> ParseResult_Err

                        LastEntryOfRecord_KeyColon _ ->
                            Error_InvalidToken Expecting_Unknown
                                |> ParseResult_Err

                        LastEntryOfRecord_KeyValue key value ->
                            { nesting =
                                NestingLeafType_Function
                                    { firstInput = value
                                    , otherInputs = empty
                                    , output = Nothing
                                    }
                            , parents =
                                NestingParentType_PartialRecord
                                    { firstEntries = firstEntries
                                    , lastEntryName = key
                                    }
                                    :: collapsedLeaf.parents
                            }
                                |> PartialResult_Progress
                                |> newState

        Lexer.Newlines _ 0 ->
            case (autoCollapseNesting CollapseLevel_Function prevExpr).nesting of
                NestingLeafType_Expr expr ->
                    PartialResult_Done expr
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


parserExpressionFromEmpty :
    (ExpressionResult -> ParseResult)
    -> Located LexItem
    -> ParseResult
parserExpressionFromEmpty newState item =
    let
        withCorrectLocation x =
            Located.map (\_ -> x) item
    in
    case Located.unwrap item of
        Lexer.NumericLiteral str ->
            case String.toInt str of
                Just i ->
                    Frontend.Int i
                        |> withCorrectLocation
                        |> ExpressionNestingLeafType_Expr
                        |> PartialResult_Progress
                        |> newState

                Nothing ->
                    Error_InvalidNumericLiteral str
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


parserExpression :
    (ExpressionResult -> ParseResult)
    -> ExpressionNestingLeaf
    -> Located LexItem
    -> ParseResult
parserExpression newState prevExpr item =
    let
        withCorrectLocation x =
            Located.map (\_ -> x) item
    in
    case Located.unwrap item of
        Lexer.Sigil (Lexer.Operator op) ->
            appendOperatorTo prevExpr op
                |> partialExpressionToParseResult newState

        Lexer.NumericLiteral str ->
            case String.toInt str of
                Just i ->
                    Frontend.Int i
                        |> withCorrectLocation
                        |> appendValueExprTo prevExpr
                        |> partialExpressionToParseResult newState

                Nothing ->
                    Error_InvalidNumericLiteral str
                        |> ParseResult_Err

        Lexer.Newlines _ 0 ->
            case prevExpr of
                ExpressionNestingLeaf_Operator partialExpr ->
                    case partialExpr.rhs of
                        Just rhs ->
                            Located.merge (Frontend.Operator partialExpr.op) partialExpr.lhs rhs
                                |> PartialResult_Done
                                |> newState

                        Nothing ->
                            Error_PartwayThroughValueDeclaration
                                |> ParseResult_Err

                ExpressionNestingLeafType_Expr expr ->
                    expr
                        |> PartialResult_Done
                        |> newState

        Lexer.Newlines _ _ ->
            ParseResult_Skip

        Lexer.Whitespace _ ->
            ParseResult_Skip

        _ ->
            Error_InvalidToken Expecting_Unknown
                |> ParseResult_Err



-- HELPERS


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


parentsToLeafWith : PartialTypeExpression -> List NestingParentType -> PartialTypeExpressionLeaf
parentsToLeafWith expr parents =
    case parents of
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

                    NestingParentType_Function { firstInput, otherInputs } ->
                        NestingLeafType_Function
                            { firstInput = firstInput
                            , otherInputs = otherInputs
                            , output = Just expr
                            }
            , parents = grandparents
            }

        [] ->
            { nesting = NestingLeafType_Expr expr
            , parents = []
            }


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


closeBracket :
    Stack PartialTypeExpression
    -> Maybe PartialTypeExpression
    -> List NestingParentType
    -> Result Error PartialTypeExpressionLeaf
closeBracket argStack mLastExpression parents =
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
            parentsToLeafWith expr parents
                |> Ok

        Err e ->
            Err e


closeRecord :
    PartialRecord
    -> List NestingParentType
    -> Result Error PartialTypeExpressionLeaf
closeRecord { firstEntries, lastEntry } parents =
    let
        fromRecord recordEntries =
            let
                record =
                    TypeExpression_Record recordEntries
            in
            parentsToLeafWith record parents
    in
    case lastEntry of
        LastEntryOfRecord_KeyValue key value ->
            ( key, value )
                |> pushOnto firstEntries
                |> toList (\x -> x)
                |> fromRecord
                |> Ok

        LastEntryOfRecord_Empty ->
            if firstEntries == empty then
                []
                    |> fromRecord
                    |> Ok

            else
                Error_InvalidToken Expecting_Unknown
                    |> Err

        _ ->
            Error_InvalidToken Expecting_Unknown
                |> Err



-- Value expression helpers


appendOperatorTo : ExpressionNestingLeaf -> Operator -> Result Error ExpressionNestingLeaf
appendOperatorTo leaf appendingOp =
    case leaf of
        ExpressionNestingLeaf_Operator newParent ->
            case newParent.rhs of
                Nothing ->
                    Error_InvalidToken Expecting_Unknown
                        |> Err

                Just parentRhs ->
                    let
                        parentPrec =
                            Operator.getPrecedence newParent.op

                        appendingPrec =
                            Operator.getPrecedence appendingOp
                    in
                    case Operator.comparePrec { lhs = parentPrec, rhs = appendingPrec } of
                        EQ ->
                            case Operator.getAssociativity parentPrec of
                                Operator.ConflictsWithOthers ->
                                    Debug.todo ""

                                Operator.ConflictsWithSelf ->
                                    Debug.todo ""

                                Operator.RightToLeft ->
                                    ExpressionNestingLeaf_Operator
                                        { op = appendingOp
                                        , lhs = parentRhs
                                        , rhs = Nothing
                                        , parent =
                                            Just
                                                (ExpressionNestingParent_Operator
                                                    { op = newParent.op
                                                    , lhs = newParent.lhs
                                                    , parent = newParent.parent
                                                    }
                                                )
                                        }
                                        |> Ok

                                Operator.LeftToRight ->
                                    ExpressionNestingLeaf_Operator
                                        { op = appendingOp
                                        , lhs = Located.merge (Frontend.Operator newParent.op) newParent.lhs parentRhs
                                        , rhs = Nothing
                                        , parent =
                                            Just
                                                (ExpressionNestingParent_Operator
                                                    { op = newParent.op
                                                    , lhs = newParent.lhs
                                                    , parent = newParent.parent
                                                    }
                                                )
                                        }
                                        |> Ok

                        GT ->
                            Debug.todo ""

                        LT ->
                            Debug.todo ""

        ExpressionNestingLeafType_Expr locatedexpr ->
            ExpressionNestingLeaf_Operator
                { op = appendingOp
                , lhs = locatedexpr
                , parent = Nothing
                , rhs = Nothing
                }
                |> Ok


appendValueExprTo : ExpressionNestingLeaf -> Frontend.LocatedExpr -> Result Error ExpressionNestingLeaf
appendValueExprTo leaf appendingExpr =
    case leaf of
        ExpressionNestingLeaf_Operator newParent ->
            case newParent.rhs of
                Nothing ->
                    ExpressionNestingLeaf_Operator
                        { op = newParent.op
                        , lhs = newParent.lhs
                        , rhs = Just appendingExpr
                        , parent = newParent.parent
                        }
                        |> Ok

                Just parentRhs ->
                    Error_ValueDoesNotTakeArgs parentRhs
                        |> Err

        ExpressionNestingLeafType_Expr locatedExpr ->
            Error_ValueDoesNotTakeArgs locatedExpr
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

        State_BlockTypeAlias (BlockTypeAlias_Named _ _) ->
            Error_PartwayThroughTypeAlias
                |> Err
                |> Just

        State_BlockTypeAlias (BlockTypeAlias_NamedAssigns _ _) ->
            Error_PartwayThroughTypeAlias
                |> Err
                |> Just

        State_BlockTypeAlias (BlockTypeAlias_Completish aliasName typeArgs partialExpr) ->
            case (autoCollapseNesting CollapseLevel_Function partialExpr).nesting of
                NestingLeafType_Expr expr ->
                    partialTypeExpressionToConcreteType expr
                        |> Result.map
                            (\conceteType ->
                                { ty = aliasName
                                , genericArgs = typeArgs
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

        State_BlockCustomType firstItem ->
            Debug.todo "handle incomplete block"

        State_BlockValueDeclaration (BlockValueDeclaration_Named _) ->
            Error_PartwayThroughValueDeclaration
                |> Err
                |> Just

        State_BlockValueDeclaration (BlockValueDeclaration_NamedAssigns _) ->
            Error_PartwayThroughValueDeclaration
                |> Err
                |> Just

        State_BlockValueDeclaration (BlockValueDeclaration_Completish { name, args, partialExpr }) ->
            case partialExpr of
                ExpressionNestingLeaf_Operator _ ->
                    Error_PartwayThroughValueDeclaration
                        |> Err
                        |> Just

                ExpressionNestingLeafType_Expr expr ->
                    { name = name
                    , args = args
                    , valueExpr__ = expr
                    }
                        |> ValueDeclaration
                        |> Ok
                        |> Just



-- helper functions


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
            parentsToLeafWith newTypeExpr pte.parents
                |> autoCollapseNesting collapseLevel

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
                    parentsToLeafWith newTypeExpr pte.parents
                        |> autoCollapseNesting collapseLevel


{-| TODO(harry): We can add things to a tuple too! Rename this function
appropriately.

TODO(harry): We can inline this function.

-}
partialExpressionToParseResult :
    (PartialResult progress neverDone -> ParseResult)
    -> Result Error progress
    -> ParseResult
partialExpressionToParseResult newState rnewPartialType =
    case rnewPartialType of
        Ok newPartialType ->
            PartialResult_Progress newPartialType
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
