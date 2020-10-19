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
import Elm.Data.Type.Concrete as ConcreteType exposing (ConcreteType)
import Elm.Data.VarName exposing (VarName)
import Stage.Parse.Lexer as Lexer exposing (LexItem(..), LexSigil(..))
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
        { name : Located String

        -- TODO(harry): these could be patterns!
        , args : List (Located String)

        -- This key's name is hard coded into parser-tests/Update.elm
        , valueExpr__ : Frontend.LocatedExpr
        }
    | TypeAlias
        { ty : String
        , genericArgs : List String
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
    | BlockTypeAlias_Named String (Stack String)
    | BlockTypeAlias_NamedAssigns String (List String)
    | BlockTypeAlias_Completish String (List String) PartialTypeExpressionLeaf


type BlockCustomType
    = BlockCustomType_Named String (Stack String)
    | BlockCustomType_NamedAssigns String (List String)


type BlockValueDeclaration
    = BlockValueDeclaration_Named
        { name : Located String
        , args : Stack (Located String)
        }
    | BlockValueDeclaration_NamedAssigns
        { name : Located String
        , args : List (Located String)
        }
    | BlockValueDeclaration_Completish
        { name : Located String
        , args : List (Located String)
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
        { op : Located Operator
        , lhs : Frontend.LocatedExpr
        , parent : Maybe ExpressionNestingParent
        }


type ExpressionNestingLeaf
    = ExpressionNestingLeaf_Operator
        { op : Located Operator
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
    | Error_BlockStartsWithQualifiedName
        { qualifiers : List String
        , name : String
        }
      -- Type Expressions --
    | Error_TypeNameStartsWithLowerCase String
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
    | Error_ConflictingOperators (Located Operator) (Located Operator)
      -- Incomplete parsing --
    | Error_PartwayThroughBlock
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


parseAnything : State -> Located Lexer.LexItem -> ParseResult
parseAnything state item =
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

        region =
            Located.getRegion item
    in
    case Located.unwrap item of
        Lexer.Invalid _ ->
            ParseResult_Err (Error_InvalidToken Expecting_Unknown)

        Lexer.Newlines _ 0 ->
            case state of
                State_Error_Recovery ->
                    State_Error_Recovery
                        |> ParseResult_Ok

                State_BlockStart ->
                    ParseResult_Ok State_BlockStart

                State_BlockFirstItem BlockFirstItem_Type ->
                    Error_PartwayThroughBlock
                        |> ParseResult_Err

                State_BlockFirstItem BlockFirstItem_Module ->
                    Error_PartwayThroughBlock
                        |> ParseResult_Err

                State_BlockValueDeclaration (BlockValueDeclaration_Named _) ->
                    Error_PartwayThroughBlock
                        |> ParseResult_Err

                State_BlockValueDeclaration (BlockValueDeclaration_NamedAssigns _) ->
                    Error_PartwayThroughBlock
                        |> ParseResult_Err

                State_BlockValueDeclaration (BlockValueDeclaration_Completish { name, args, partialExpr }) ->
                    case partialExpr of
                        ExpressionNestingLeaf_Operator { rhs, lhs, op, parent } ->
                            case rhs of
                                Just rhs_ ->
                                    collapseOperators { op = op, lhs = lhs, parent = parent } rhs_
                                        |> PartialResult_Done
                                        |> newExpressionState name args

                                Nothing ->
                                    Error_PartwayThroughBlock
                                        |> ParseResult_Err

                        ExpressionNestingLeafType_Expr expr ->
                            expr
                                |> PartialResult_Done
                                |> newExpressionState name args

                State_BlockTypeAlias BlockTypeAlias_Keywords ->
                    Error_PartwayThroughBlock
                        |> ParseResult_Err

                State_BlockTypeAlias (BlockTypeAlias_Named _ _) ->
                    Error_PartwayThroughBlock
                        |> ParseResult_Err

                State_BlockTypeAlias (BlockTypeAlias_NamedAssigns _ _) ->
                    Error_PartwayThroughBlock
                        |> ParseResult_Err

                State_BlockTypeAlias (BlockTypeAlias_Completish name typeArgs exprSoFar) ->
                    case (autoCollapseNesting CollapseLevel_Function exprSoFar).nesting of
                        NestingLeafType_Expr expr ->
                            PartialResult_Done expr
                                |> newTypeAliasState name typeArgs

                        _ ->
                            Error_PartwayThroughBlock
                                |> ParseResult_Err

                State_BlockCustomType (BlockCustomType_Named _ _) ->
                    Error_PartwayThroughBlock
                        |> ParseResult_Err

                State_BlockCustomType (BlockCustomType_NamedAssigns _ _) ->
                    Debug.todo "BlockCustomType_NamedAssigns"

        Lexer.Newlines _ _ ->
            ParseResult_Skip

        Lexer.Ignorable _ ->
            ParseResult_Skip

        Lexer.Token token ->
            case state of
                State_Error_Recovery ->
                    State_Error_Recovery
                        |> ParseResult_Ok

                State_BlockStart ->
                    parseBlockStart region token

                State_BlockFirstItem BlockFirstItem_Type ->
                    parseTypeBlock token

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
                        (Located.located region token)

                State_BlockValueDeclaration (BlockValueDeclaration_NamedAssigns { name, args }) ->
                    parserExpressionFromEmpty (newExpressionState name args) (Located.located region token)

                State_BlockValueDeclaration (BlockValueDeclaration_Completish { name, args, partialExpr }) ->
                    parserExpression
                        (newExpressionState name args)
                        partialExpr
                        (Located.located region token)

                State_BlockTypeAlias BlockTypeAlias_Keywords ->
                    parseTypeAliasName token

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
                        (Located.located region token)

                State_BlockTypeAlias (BlockTypeAlias_NamedAssigns name typeArgs) ->
                    parserTypeExprFromEmpty
                        (newTypeAliasState name typeArgs)
                        (Located.located region token)

                State_BlockTypeAlias (BlockTypeAlias_Completish name typeArgs exprSoFar) ->
                    parserTypeExpr
                        (newTypeAliasState name typeArgs)
                        exprSoFar
                        (Located.located region token)

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
                        (Located.located region token)

                State_BlockCustomType (BlockCustomType_NamedAssigns _ _) ->
                    Debug.todo "BlockCustomType_NamedAssigns"


{-|


### Panics

If the LexItem is a `Newlines` with indentation or is `Whitespace`.

-}
parseBlockStart : Located.Region -> Lexer.LexToken -> ParseResult
parseBlockStart region item =
    let
        withCorrectLocation =
            Located.located region
    in
    case item of
        Lexer.Keyword Token.Type ->
            ParseResult_Ok (State_BlockFirstItem BlockFirstItem_Type)

        Lexer.Keyword Token.Module ->
            ParseResult_Ok (State_BlockFirstItem BlockFirstItem_Module)

        Lexer.Keyword other ->
            ParseResult_Err (Error_MisplacedKeyword other)

        Lexer.Identifier ({ qualifiers, name } as identitfier) ->
            if qualifiers /= [] then
                ParseResult_Err (Error_BlockStartsWithQualifiedName identitfier)

            else
                ParseResult_Ok
                    (State_BlockValueDeclaration
                        (BlockValueDeclaration_Named
                            { name = withCorrectLocation name
                            , args = empty
                            }
                        )
                    )

        _ ->
            ParseResult_Err (Error_InvalidToken Expecting_Block)


parseTypeBlock : Lexer.LexToken -> ParseResult
parseTypeBlock item =
    case item of
        Lexer.Keyword Token.Alias ->
            State_BlockTypeAlias BlockTypeAlias_Keywords
                |> ParseResult_Ok

        Lexer.Keyword other ->
            Error_MisplacedKeyword other
                |> ParseResult_Err

        Lexer.Identifier ({ qualifiers, name } as identifier) ->
            if qualifiers /= [] then
                ParseResult_Err (Error_BlockStartsWithQualifiedName identifier)

            else
                State_BlockCustomType (BlockCustomType_Named name empty)
                    |> ParseResult_Ok

        _ ->
            -- TODO(harry) indicate that we could also be expecting the `alias`
            -- keyword.
            Error_InvalidToken Expecting_TypeName
                |> ParseResult_Err


parseTypeAliasName : Lexer.LexToken -> ParseResult
parseTypeAliasName item =
    case item of
        Lexer.Keyword other ->
            Error_MisplacedKeyword other
                |> ParseResult_Err

        Lexer.Identifier ({ qualifiers, name } as identifier) ->
            if qualifiers /= [] then
                ParseResult_Err (Error_BlockStartsWithQualifiedName identifier)

            else
                State_BlockTypeAlias (BlockTypeAlias_Named name empty)
                    |> ParseResult_Ok

        _ ->
            Error_InvalidToken Expecting_TypeName
                |> ParseResult_Err


parseLowercaseArgsOrAssignment : (Located String -> State) -> State -> Located Lexer.LexToken -> ParseResult
parseLowercaseArgsOrAssignment onTypeArg onAssignment item =
    let
        withCorrectLocation x =
            Located.map (\_ -> x) item
    in
    case Located.unwrap item of
        Lexer.Keyword kw ->
            Error_MisplacedKeyword kw
                |> ParseResult_Err

        Lexer.Identifier ({ qualifiers, name } as identifier) ->
            if qualifiers /= [] then
                ParseResult_Err (Error_BlockStartsWithQualifiedName identifier)

            else
                onTypeArg (withCorrectLocation name)
                    |> ParseResult_Ok

        Lexer.Sigil Lexer.Assign ->
            onAssignment
                |> ParseResult_Ok

        _ ->
            Error_InvalidToken (Expecting_Sigil Lexer.Assign)
                |> ParseResult_Err


parserTypeExprFromEmpty :
    (TypeExpressionResult -> ParseResult)
    -> Located Lexer.LexToken
    -> ParseResult
parserTypeExprFromEmpty newState item =
    case Located.unwrap item of
        Lexer.Identifier { qualifiers, name } ->
            if qualifiers /= [] then
                Debug.todo ""

            else
                { parents = []
                , nesting =
                    NestingLeafType_TypeWithArgs
                        { name = name
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

        _ ->
            Error_InvalidToken Expecting_Unknown
                |> ParseResult_Err


parserTypeExpr :
    (TypeExpressionResult -> ParseResult)
    -> PartialTypeExpressionLeaf
    -> Located Lexer.LexToken
    -> ParseResult
parserTypeExpr newState prevExpr item =
    case Located.unwrap item of
        Lexer.Identifier { qualifiers, name } ->
            if qualifiers /= [] then
                Debug.todo ""

            else
                exprAppend prevExpr name
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
                NestingLeafType_Expr _ ->
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
                NestingLeafType_Expr _ ->
                    Error_UnmatchedBracket Lexer.Curly Lexer.Close
                        |> ParseResult_Err

                NestingLeafType_TypeWithArgs { name, args } ->
                    Debug.todo "Make this state impossible"

                NestingLeafType_Bracket _ _ ->
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

                NestingLeafType_TypeWithArgs _ ->
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

                NestingLeafType_Bracket _ Nothing ->
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

        _ ->
            Error_InvalidToken Expecting_Unknown
                |> ParseResult_Err


parserExpressionFromEmpty :
    (ExpressionResult -> ParseResult)
    -> Located Lexer.LexToken
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

        _ ->
            Error_InvalidToken Expecting_Unknown
                |> ParseResult_Err


parserExpression :
    (ExpressionResult -> ParseResult)
    -> ExpressionNestingLeaf
    -> Located Lexer.LexToken
    -> ParseResult
parserExpression newState prevExpr item =
    let
        withCorrectLocation x =
            Located.map (\_ -> x) item
    in
    case Located.unwrap item of
        Lexer.Sigil (Lexer.Operator op) ->
            appendOperatorTo prevExpr (withCorrectLocation op)
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
        NestingLeafType_Bracket _ mostNested ->
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
                    Error_TypeDoesNotTakeArgs existingRoot newType
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

        NestingLeafType_Function { firstInput, output } ->
            Result.map2
                (\newOutput newParents ->
                    { parents = newParents
                    , nesting = newOutput
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


appendOperatorTo : ExpressionNestingLeaf -> Located Operator -> Result Error ExpressionNestingLeaf
appendOperatorTo leaf appendingOp =
    case leaf of
        ExpressionNestingLeaf_Operator oldLeaf ->
            case oldLeaf.rhs of
                Nothing ->
                    Error_InvalidToken Expecting_Unknown
                        |> Err

                Just parentRhs ->
                    let
                        prevPrec =
                            Operator.getPrecedence (Located.unwrap oldLeaf.op)

                        appendingPrec =
                            Operator.getPrecedence (Located.unwrap appendingOp)
                    in
                    case Operator.comparePrec { lhs = prevPrec, rhs = appendingPrec } of
                        EQ ->
                            case Operator.getAssociativity prevPrec of
                                Operator.ConflictsWithOthers ->
                                    Debug.todo ""

                                Operator.ConflictsWithSelf ->
                                    Error_ConflictingOperators oldLeaf.op appendingOp
                                        |> Err

                                Operator.RightToLeft ->
                                    ExpressionNestingLeaf_Operator
                                        { op = appendingOp
                                        , lhs = parentRhs
                                        , rhs = Nothing
                                        , parent =
                                            Just
                                                (ExpressionNestingParent_Operator
                                                    { op = oldLeaf.op
                                                    , lhs = oldLeaf.lhs
                                                    , parent = oldLeaf.parent
                                                    }
                                                )
                                        }
                                        |> Ok

                                Operator.LeftToRight ->
                                    ExpressionNestingLeaf_Operator
                                        { op = appendingOp
                                        , lhs = Located.merge (Frontend.Operator oldLeaf.op) oldLeaf.lhs parentRhs
                                        , rhs = Nothing
                                        , parent = oldLeaf.parent
                                        }
                                        |> Ok

                        GT ->
                            -- Prev operator has higher precedence than the appending operator. We bundle the previous
                            -- nesting context into the child's lhs.
                            ExpressionNestingLeaf_Operator
                                { op = appendingOp
                                , lhs =
                                    Located.merge
                                        (\op args ->
                                            let
                                                ( lhs, rhs ) =
                                                    Located.unwrap args
                                            in
                                            Frontend.Operator op lhs rhs
                                        )
                                        oldLeaf.op
                                        (Located.merge Tuple.pair oldLeaf.lhs parentRhs)
                                , rhs = Nothing
                                , parent = oldLeaf.parent
                                }
                                |> Ok

                        LT ->
                            -- Prev operator has lower precedence than the appending operator. We steal the previous
                            -- operator's rhs and start a new layer of nesting with the previous rhs as our new lhs.
                            ExpressionNestingLeaf_Operator
                                { op = appendingOp
                                , lhs = parentRhs
                                , rhs = Nothing
                                , parent =
                                    Just
                                        (ExpressionNestingParent_Operator
                                            { op = oldLeaf.op
                                            , lhs = oldLeaf.lhs
                                            , parent = oldLeaf.parent
                                            }
                                        )
                                }
                                |> Ok

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
        ExpressionNestingLeaf_Operator oldLeaf ->
            case oldLeaf.rhs of
                Nothing ->
                    ExpressionNestingLeaf_Operator
                        { op = oldLeaf.op
                        , lhs = oldLeaf.lhs
                        , rhs = Just appendingExpr
                        , parent = oldLeaf.parent
                        }
                        |> Ok

                Just parentRhs ->
                    Error_ValueDoesNotTakeArgs parentRhs
                        |> Err

        ExpressionNestingLeafType_Expr locatedExpr ->
            Error_ValueDoesNotTakeArgs locatedExpr
                |> Err


collapseOperators :
    { op : Located Operator
    , lhs : Frontend.LocatedExpr
    , parent : Maybe ExpressionNestingParent
    }
    -> Frontend.LocatedExpr
    -> Frontend.LocatedExpr
collapseOperators opData rhs =
    let
        expr =
            Located.merge (Frontend.Operator opData.op) opData.lhs rhs
    in
    case opData.parent of
        Just (ExpressionNestingParent_Operator parentData) ->
            collapseOperators parentData expr

        Nothing ->
            expr


blockFromState : State -> Maybe (Result Error Block)
blockFromState state =
    case state of
        State_Error_Recovery ->
            Nothing

        State_BlockStart ->
            Nothing

        State_BlockFirstItem _ ->
            Debug.todo "handle incomplete block"

        State_BlockTypeAlias BlockTypeAlias_Keywords ->
            Error_PartwayThroughBlock
                |> Err
                |> Just

        State_BlockTypeAlias (BlockTypeAlias_Named _ _) ->
            Error_PartwayThroughBlock
                |> Err
                |> Just

        State_BlockTypeAlias (BlockTypeAlias_NamedAssigns _ _) ->
            Error_PartwayThroughBlock
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
                    Error_PartwayThroughBlock
                        |> Err
                        |> Just

        State_BlockCustomType _ ->
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
