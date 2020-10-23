module Stage.Parse.Contextualize exposing (..)

{-| Here we define parsers. A parser is function

    type alias Parser =
        Lexer.LexItem -> Result Error State

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

```text
module Main exposing (..)   -- 0


import Dict                 -- 1

type A = A                  -- 2

a : A                       -- 3
a = A                       -- 4
```

There are five blocks.

-}
type Block
    = Module
        { ty : Elm.Data.Module.ModuleType
        , name : Elm.Data.ModuleName.ModuleName
        , exposingList : Elm.Data.Exposing.Exposing
        }
    | ValueDeclaration
        { name : Located Token.LowerCase

        -- TODO(harry): these could be patterns!
        , args : List (Located Token.LowerCase)

        -- This key's name is hard coded into parser-tests/Update.elm
        , valueExpr__ : Frontend.LocatedExpr
        }
    | TypeAlias
        { ty : Token.UpperCase
        , genericArgs : List Token.LowerCase
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


type ParseSuccess
    = ParseSuccess_NewState State
    | ParseSuccess_Complete Block
    | ParseSuccess_Skip


type alias State_ =
    { previousBlocks : List RunResult
    , state : State
    }


type BlockFirstItem
    = BlockFirstItem_Type
    | BlockFirstItem_Module


type BlockTypeAlias
    = BlockTypeAlias_Keywords
    | BlockTypeAlias_Named Token.UpperCase (Stack Token.LowerCase)
    | BlockTypeAlias_NamedAssigns Token.UpperCase (List Token.LowerCase)
    | BlockTypeAlias_Completish Token.UpperCase (List Token.LowerCase) (TypeExpressionNestingLeaf () ())


type BlockCustomType
    = BlockCustomType_Named Token.UpperCase (Stack Token.LowerCase)
    | BlockCustomType_NamedAssigns Token.UpperCase (List Token.LowerCase)


type BlockValueDeclaration
    = BlockValueDeclaration_Named
        { name : Located Token.LowerCase
        , args : Stack (Located Token.LowerCase)
        }
    | BlockValueDeclaration_NamedAssigns
        { name : Located Token.LowerCase
        , args : List (Located Token.LowerCase)
        }
    | BlockValueDeclaration_Completish
        { name : Located Token.LowerCase
        , args : List (Located Token.LowerCase)
        , partialExpr : ExpressionNestingLeaf
        }



-- TYPE EXPRESSIONS --


{-| Notes:

  - Type names can start with a lower case character as it may be generic. If
    it is generic there should be no args (we do not check this currently
    though :().

-}
type TypeExpression
    = TypeExpression_NamedType
        { qualifiers : List Token.UpperCase
        , name : Token.UpperCase
        , args : Stack TypeExpression
        }
    | TypeExpression_GenericType Token.LowerCase
    | TypeExpression_Unit
    | TypeExpression_Bracketed TypeExpression
    | TypeExpression_Tuple TypeExpression TypeExpression (List TypeExpression)
    | TypeExpression_Record (List ( Token.LowerCase, TypeExpression ))
    | TypeExpression_Function
        { firstInput : TypeExpression
        , otherInputs : List TypeExpression
        , output : TypeExpression
        }


type LastEntryOfRecord
    = LastEntryOfRecord_Empty
    | LastEntryOfRecord_Key Token.LowerCase
    | LastEntryOfRecord_KeyColon Token.LowerCase
    | LastEntryOfRecord_KeyValue Token.LowerCase TypeExpression


type alias PartialRecord =
    { firstEntries : Stack ( Token.LowerCase, TypeExpression )
    , lastEntry : LastEntryOfRecord
    , parent : Maybe TypeExpressionNestingParent
    }


type TypeExpressionNestingParent
    = NestingParentType_Bracket
        { expressions : Stack TypeExpression
        , parent : Maybe TypeExpressionNestingParent
        }
    | NestingParentType_PartialRecord
        { firstEntries : Stack ( Token.LowerCase, TypeExpression )
        , lastEntryName : Token.LowerCase
        , parent : Maybe TypeExpressionNestingParent
        }
    | NestingParentType_TypeWithArgs
        { qualifiers : List Token.UpperCase
        , name : Token.UpperCase
        , args : Stack TypeExpression
        , parent : Maybe TypeExpressionNestingParent
        , phantom : ()
        }
    | NestingParentType_Function
        { firstInput : TypeExpression
        , otherInputs : Stack TypeExpression
        , parent : Maybe TypeExpressionNestingParent
        }


{-| Sometimes we need to limit which types of `TypeExpressionNestingLeaf` are valid. We do that using the phantom type
parameters. For example `TypeExpressionNestingLeaf () Never` cannot be a `TypeExpressionNestingLeaf_TypeWithArgs`.
-}
type TypeExpressionNestingLeaf phantomFunctionWithOutput phantomTypeWithArgs
    = TypeExpressionNestingLeaf_Bracket
        { firstExpressions : Stack TypeExpression
        , trailingExpression : Maybe TypeExpression
        , parent : Maybe TypeExpressionNestingParent
        }
    | TypeExpressionNestingLeaf_PartialRecord PartialRecord
    | TypeExpressionNestingLeaf_TypeWithArgs
        { qualifiers : List Token.UpperCase
        , name : Token.UpperCase
        , args : Stack TypeExpression
        , parent : Maybe TypeExpressionNestingParent
        , phantom : phantomTypeWithArgs
        }
    | TypeExpressionNestingLeaf_Function
        { firstInput : TypeExpression
        , otherInputs : Stack TypeExpression
        , output : Maybe ( TypeExpression, phantomFunctionWithOutput )
        , parent : Maybe TypeExpressionNestingParent
        }
    | TypeExpressionNestingLeaf_Expr TypeExpression



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
    | ExpressionTypeExpressionNestingLeaf_Expr Frontend.LocatedExpr


type Error
    = Error_InvalidToken Expecting
    | Error_MisplacedKeyword Keyword
    | Error_BlockStartsWithUpperCase Token.UpperCase
    | Error_BlockStartsWithQualifiedName
        { qualifiers : List Token.UpperCase
        , name : Token.Token
        }
    | Error_QualifiedArgName
        { qualifiers : List Token.UpperCase
        , name : Token.Token
        }
    | Error_UpperCaseArgName
        { qualifiers : List Token.UpperCase
        , name : Token.UpperCase
        }
    | Error_UpperCaseRecordKey Token.UpperCase
    | Error_QualifiedRecordKey
        { qualifiers : List Token.UpperCase
        , name : Token.Token
        }
    | Error_LowerCasedTypename
        { qualifiers : List Token.UpperCase
        , name : Token.LowerCase
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
    | Error_TypeDoesNotTakeArgs TypeExpression TypeExpression
    | Error_TypeDoesNotTakeArgs2 TypeExpression
    | Error_ValueDoesNotTakeArgs Frontend.LocatedExpr
    | Error_ExtraItemAfterBlock TypeExpression Lexer.LexItem
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
                Ok (ParseSuccess_NewState newState_) ->
                    runHelp
                        rest
                        { previousBlocks = state.previousBlocks
                        , state = newState_
                        }

                Ok (ParseSuccess_Complete block) ->
                    runHelp
                        rest
                        { previousBlocks = Ok block :: state.previousBlocks
                        , state = State_BlockStart
                        }

                Ok ParseSuccess_Skip ->
                    runHelp
                        rest
                        state

                Err error ->
                    runHelp
                        (case error of
                            Error_Panic _ ->
                                [{- An empty list to abort parsing. -}]

                            _ ->
                                rest
                        )
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


parseAnything : State -> Located Lexer.LexItem -> Result Error ParseSuccess
parseAnything state item =
    let
        newTypeAliasState : Token.UpperCase -> List Token.LowerCase -> TypeExpressionNestingLeaf () () -> Result Error State
        newTypeAliasState aliasName typeArgs expr =
            State_BlockTypeAlias (BlockTypeAlias_Completish aliasName typeArgs expr)
                |> Ok

        doneParsingTypeAlias : Token.UpperCase -> List Token.LowerCase -> TypeExpression -> Result Error ParseSuccess
        doneParsingTypeAlias aliasName typeArgs expr =
            case typeExpressionToConcreteType expr of
                Ok concreteType ->
                    { ty = aliasName
                    , genericArgs = typeArgs
                    , expr = concreteType
                    }
                        |> TypeAlias
                        |> ParseSuccess_Complete
                        |> Ok

                Err (ToConcreteTypeError_TooManyTupleArgs a b c d e) ->
                    Error_TooManyTupleArgs a b c d e
                        |> Err

        newExpressionState : Located Token.LowerCase -> List (Located Token.LowerCase) -> ExpressionNestingLeaf -> Result Error State
        newExpressionState name args expr =
            State_BlockValueDeclaration
                (BlockValueDeclaration_Completish
                    { name = name
                    , args = args
                    , partialExpr = expr
                    }
                )
                |> Ok

        doneParsingExpr : Located Token.LowerCase -> List (Located Token.LowerCase) -> LocatedExpr -> ParseSuccess
        doneParsingExpr name args expr =
            { name = name
            , args = args
            , valueExpr__ = expr
            }
                |> ValueDeclaration
                |> ParseSuccess_Complete

        region =
            Located.getRegion item
    in
    case Located.unwrap item of
        Lexer.Invalid _ ->
            Err (Error_InvalidToken Expecting_Unknown)

        Lexer.Newlines _ 0 ->
            case state of
                State_Error_Recovery ->
                    State_BlockStart
                        |> ParseSuccess_NewState
                        |> Ok

                State_BlockStart ->
                    State_BlockStart
                        |> ParseSuccess_NewState
                        |> Ok

                State_BlockFirstItem BlockFirstItem_Type ->
                    Error_PartwayThroughBlock
                        |> Err

                State_BlockFirstItem BlockFirstItem_Module ->
                    Error_PartwayThroughBlock
                        |> Err

                State_BlockValueDeclaration (BlockValueDeclaration_Named _) ->
                    Error_PartwayThroughBlock
                        |> Err

                State_BlockValueDeclaration (BlockValueDeclaration_NamedAssigns _) ->
                    Error_PartwayThroughBlock
                        |> Err

                State_BlockValueDeclaration (BlockValueDeclaration_Completish { name, args, partialExpr }) ->
                    case partialExpr of
                        ExpressionNestingLeaf_Operator { rhs, lhs, op, parent } ->
                            case rhs of
                                Just rhs_ ->
                                    collapseOperators { op = op, lhs = lhs, parent = parent } rhs_
                                        |> doneParsingExpr name args
                                        |> Ok

                                Nothing ->
                                    Error_PartwayThroughBlock
                                        |> Err

                        ExpressionTypeExpressionNestingLeaf_Expr expr ->
                            doneParsingExpr name args expr
                                |> Ok

                State_BlockTypeAlias BlockTypeAlias_Keywords ->
                    Error_PartwayThroughBlock
                        |> Err

                State_BlockTypeAlias (BlockTypeAlias_Named _ _) ->
                    Error_PartwayThroughBlock
                        |> Err

                State_BlockTypeAlias (BlockTypeAlias_NamedAssigns _ _) ->
                    Error_PartwayThroughBlock
                        |> Err

                State_BlockTypeAlias (BlockTypeAlias_Completish name typeArgs exprSoFar) ->
                    case autoCollapseNesting CollapseLevel_Function exprSoFar of
                        TypeExpressionNestingLeaf_Expr expr ->
                            doneParsingTypeAlias name typeArgs expr

                        _ ->
                            Error_PartwayThroughBlock
                                |> Err

                State_BlockCustomType (BlockCustomType_Named _ _) ->
                    Error_PartwayThroughBlock
                        |> Err

                State_BlockCustomType (BlockCustomType_NamedAssigns _ _) ->
                    Debug.todo "BlockCustomType_NamedAssigns"

        Lexer.Newlines _ _ ->
            Ok ParseSuccess_Skip

        Lexer.Ignorable _ ->
            Ok ParseSuccess_Skip

        Lexer.Token token ->
            Result.map
                ParseSuccess_NewState
                (case state of
                    State_Error_Recovery ->
                        State_Error_Recovery
                            |> Ok

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
                )


{-|


### Panics

If the LexItem is a `Newlines` with indentation or is `Whitespace`.

-}
parseBlockStart : Located.Region -> Lexer.LexToken -> Result Error State
parseBlockStart region item =
    let
        withCorrectLocation =
            Located.located region
    in
    case item of
        Lexer.Keyword Token.Type ->
            Ok (State_BlockFirstItem BlockFirstItem_Type)

        Lexer.Keyword Token.Module ->
            Ok (State_BlockFirstItem BlockFirstItem_Module)

        Lexer.Keyword other ->
            Err (Error_MisplacedKeyword other)

        Lexer.Identifier ({ qualifiers, name } as identitfier) ->
            if qualifiers /= [] then
                Err (Error_BlockStartsWithQualifiedName identitfier)

            else
                case name of
                    Token.TokenLowerCase lower ->
                        Ok
                            (State_BlockValueDeclaration
                                (BlockValueDeclaration_Named
                                    { name = withCorrectLocation lower
                                    , args = empty
                                    }
                                )
                            )

                    Token.TokenUpperCase upperCase ->
                        Error_BlockStartsWithUpperCase upperCase
                            |> Err

        _ ->
            Err (Error_InvalidToken Expecting_Block)


parseTypeBlock : Lexer.LexToken -> Result Error State
parseTypeBlock item =
    case item of
        Lexer.Keyword Token.Alias ->
            State_BlockTypeAlias BlockTypeAlias_Keywords
                |> Ok

        Lexer.Keyword other ->
            Error_MisplacedKeyword other
                |> Err

        Lexer.Identifier ({ qualifiers, name } as identifier) ->
            if qualifiers /= [] then
                Err (Error_BlockStartsWithQualifiedName identifier)

            else
                case name of
                    Token.TokenLowerCase lower ->
                        Error_LowerCasedTypename
                            { qualifiers = qualifiers
                            , name = lower
                            }
                            |> Err

                    Token.TokenUpperCase upper ->
                        State_BlockCustomType (BlockCustomType_Named upper empty)
                            |> Ok

        _ ->
            -- TODO(harry) indicate that we could also be expecting the `alias`
            -- keyword.
            Error_InvalidToken Expecting_TypeName
                |> Err


parseTypeAliasName : Lexer.LexToken -> Result Error State
parseTypeAliasName item =
    case item of
        Lexer.Keyword other ->
            Error_MisplacedKeyword other
                |> Err

        Lexer.Identifier ({ qualifiers, name } as identifier) ->
            if qualifiers /= [] then
                Err (Error_BlockStartsWithQualifiedName identifier)

            else
                case name of
                    Token.TokenLowerCase lower ->
                        Error_LowerCasedTypename
                            { qualifiers = qualifiers
                            , name = lower
                            }
                            |> Err

                    Token.TokenUpperCase upper ->
                        State_BlockTypeAlias (BlockTypeAlias_Named upper empty)
                            |> Ok

        _ ->
            Error_InvalidToken Expecting_TypeName
                |> Err


parseLowercaseArgsOrAssignment : (Located Token.LowerCase -> State) -> State -> Located Lexer.LexToken -> Result Error State
parseLowercaseArgsOrAssignment onTypeArg onAssignment item =
    let
        withCorrectLocation x =
            Located.map (\_ -> x) item
    in
    case Located.unwrap item of
        Lexer.Keyword kw ->
            Error_MisplacedKeyword kw
                |> Err

        Lexer.Identifier ({ qualifiers, name } as identifier) ->
            if qualifiers /= [] then
                Err (Error_QualifiedArgName identifier)

            else
                case name of
                    Token.TokenLowerCase lower ->
                        onTypeArg (withCorrectLocation lower)
                            |> Ok

                    Token.TokenUpperCase upper ->
                        Error_UpperCaseArgName { qualifiers = qualifiers, name = upper }
                            |> Err

        Lexer.Sigil Lexer.Assign ->
            onAssignment
                |> Ok

        _ ->
            Error_InvalidToken (Expecting_Sigil Lexer.Assign)
                |> Err


parserTypeExprFromEmpty :
    (TypeExpressionNestingLeaf () () -> Result Error State)
    -> Located Lexer.LexToken
    -> Result Error State
parserTypeExprFromEmpty newState item =
    case Located.unwrap item of
        Lexer.Identifier { qualifiers, name } ->
            case name of
                Token.TokenLowerCase lower ->
                    if qualifiers == [] then
                        TypeExpression_GenericType lower
                            |> TypeExpressionNestingLeaf_Expr
                            |> newState

                    else
                        Error_LowerCasedTypename
                            { qualifiers = qualifiers
                            , name = lower
                            }
                            |> Err

                Token.TokenUpperCase upper ->
                    if qualifiers /= [] then
                        Debug.todo ""

                    else
                        TypeExpressionNestingLeaf_TypeWithArgs
                            { qualifiers = qualifiers
                            , name = upper
                            , args = empty
                            , parent = Nothing
                            , phantom = ()
                            }
                            |> newState

        Lexer.Sigil (Lexer.Bracket Lexer.Round Lexer.Open) ->
            TypeExpressionNestingLeaf_Bracket
                { firstExpressions = empty
                , trailingExpression = Nothing
                , parent = Nothing
                }
                |> newState

        Lexer.Sigil (Lexer.Bracket role Lexer.Close) ->
            Error_UnmatchedBracket role Lexer.Close
                |> Err

        Lexer.Sigil (Lexer.Bracket Lexer.Curly Lexer.Open) ->
            TypeExpressionNestingLeaf_PartialRecord
                { firstEntries = empty
                , lastEntry = LastEntryOfRecord_Empty
                , parent = Nothing
                }
                |> newState

        Lexer.Sigil Lexer.Colon ->
            Error_InvalidToken Expecting_Unknown
                |> Err

        Lexer.Sigil Lexer.Comma ->
            Error_InvalidToken Expecting_Unknown
                |> Err

        Lexer.Sigil Lexer.ThinArrow ->
            Error_InvalidToken Expecting_Unknown
                |> Err

        _ ->
            Error_InvalidToken Expecting_Unknown
                |> Err


parserTypeExpr :
    (TypeExpressionNestingLeaf () () -> Result Error State)
    -> TypeExpressionNestingLeaf () ()
    -> Located Lexer.LexToken
    -> Result Error State
parserTypeExpr newState prevExpr item =
    case Located.unwrap item of
        Lexer.Identifier ident ->
            exprAppend prevExpr ident
                |> Result.andThen newState

        Lexer.Sigil (Lexer.Bracket Lexer.Round Lexer.Open) ->
            leafToParent prevExpr
                |> Result.map
                    (\parent ->
                        TypeExpressionNestingLeaf_Bracket
                            { firstExpressions = empty
                            , trailingExpression = Nothing
                            , parent = Just parent
                            }
                    )
                |> Result.andThen newState

        Lexer.Sigil (Lexer.Bracket Lexer.Round Lexer.Close) ->
            case autoCollapseNesting CollapseLevel_Function prevExpr of
                TypeExpressionNestingLeaf_Expr _ ->
                    Error_UnmatchedBracket Lexer.Round Lexer.Close
                        |> Err

                TypeExpressionNestingLeaf_TypeWithArgs { name, args, phantom } ->
                    never phantom

                TypeExpressionNestingLeaf_Bracket { firstExpressions, trailingExpression, parent } ->
                    closeBracket firstExpressions trailingExpression parent
                        |> Result.andThen newState

                TypeExpressionNestingLeaf_PartialRecord _ ->
                    Error_WrongClosingBracket
                        { expecting = Lexer.Curly
                        , found = Lexer.Round
                        }
                        |> Err

                TypeExpressionNestingLeaf_Function { output } ->
                    case output of
                        Nothing ->
                            Error_MissingFunctionReturnType
                                |> Err

                        Just ( _, phantom ) ->
                            never phantom

        Lexer.Sigil (Lexer.Bracket Lexer.Curly Lexer.Open) ->
            leafToParent prevExpr
                |> Result.map
                    (\newParent ->
                        TypeExpressionNestingLeaf_PartialRecord
                            { firstEntries = empty
                            , lastEntry = LastEntryOfRecord_Empty
                            , parent = Just newParent
                            }
                    )
                |> Result.andThen newState

        Lexer.Sigil Lexer.Colon ->
            appendColonTo prevExpr
                |> Result.andThen newState

        Lexer.Sigil Lexer.Comma ->
            appendCommaTo prevExpr
                |> Result.andThen newState

        Lexer.Sigil (Lexer.Bracket Lexer.Curly Lexer.Close) ->
            case autoCollapseNesting CollapseLevel_Function prevExpr of
                TypeExpressionNestingLeaf_Expr _ ->
                    Error_UnmatchedBracket Lexer.Curly Lexer.Close
                        |> Err

                TypeExpressionNestingLeaf_TypeWithArgs { name, args, phantom } ->
                    never phantom

                TypeExpressionNestingLeaf_Bracket _ ->
                    Error_WrongClosingBracket
                        { expecting = Lexer.Round
                        , found = Lexer.Curly
                        }
                        |> Err

                TypeExpressionNestingLeaf_PartialRecord pr ->
                    closeRecord pr
                        |> Result.andThen newState

                TypeExpressionNestingLeaf_Function { output } ->
                    case output of
                        Nothing ->
                            Error_MissingFunctionReturnType
                                |> Err

                        Just ( _, phantom ) ->
                            never phantom

        Lexer.Sigil Lexer.ThinArrow ->
            case autoCollapseNesting (CollapseLevel_TypeWithArgs ()) prevExpr of
                TypeExpressionNestingLeaf_Expr expr ->
                    TypeExpressionNestingLeaf_Function
                        { firstInput = expr
                        , otherInputs = empty
                        , output = Nothing
                        , parent = Nothing
                        }
                        |> newState

                TypeExpressionNestingLeaf_TypeWithArgs { phantom } ->
                    never phantom

                TypeExpressionNestingLeaf_Function { firstInput, otherInputs, output, parent } ->
                    case output of
                        Nothing ->
                            Error_InvalidToken Expecting_Unknown
                                |> Err

                        Just output_ ->
                            TypeExpressionNestingLeaf_Function
                                { firstInput = firstInput
                                , otherInputs = output_ |> Tuple.first |> pushOnto otherInputs
                                , output = Nothing
                                , parent = parent
                                }
                                |> newState

                TypeExpressionNestingLeaf_Bracket { firstExpressions, trailingExpression, parent } ->
                    case trailingExpression of
                        Just trailingExpression_ ->
                            TypeExpressionNestingLeaf_Function
                                { firstInput = trailingExpression_
                                , otherInputs = empty
                                , output = Nothing
                                , parent =
                                    { expressions = firstExpressions
                                    , parent = parent
                                    }
                                        |> NestingParentType_Bracket
                                        |> Just
                                }
                                |> newState

                        Nothing ->
                            Error_InvalidToken Expecting_Unknown
                                |> Err

                TypeExpressionNestingLeaf_PartialRecord { firstEntries, lastEntry, parent } ->
                    case lastEntry of
                        LastEntryOfRecord_Empty ->
                            Error_InvalidToken Expecting_Identifier
                                |> Err

                        LastEntryOfRecord_Key _ ->
                            Error_InvalidToken (Expecting_Sigil Lexer.Colon)
                                |> Err

                        LastEntryOfRecord_KeyColon _ ->
                            Error_InvalidToken Expecting_Unknown
                                |> Err

                        LastEntryOfRecord_KeyValue key value ->
                            TypeExpressionNestingLeaf_Function
                                { firstInput = value
                                , otherInputs = empty
                                , output = Nothing
                                , parent =
                                    { firstEntries = firstEntries
                                    , lastEntryName = key
                                    , parent = parent
                                    }
                                        |> NestingParentType_PartialRecord
                                        |> Just
                                }
                                |> newState

        _ ->
            Error_InvalidToken Expecting_Unknown
                |> Err


parserExpressionFromEmpty :
    (ExpressionNestingLeaf -> Result Error State)
    -> Located Lexer.LexToken
    -> Result Error State
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
                        |> ExpressionTypeExpressionNestingLeaf_Expr
                        |> newState

                Nothing ->
                    Error_InvalidNumericLiteral str
                        |> Err

        _ ->
            Error_InvalidToken Expecting_Unknown
                |> Err


parserExpression :
    (ExpressionNestingLeaf -> Result Error State)
    -> ExpressionNestingLeaf
    -> Located Lexer.LexToken
    -> Result Error State
parserExpression newState prevExpr item =
    let
        withCorrectLocation x =
            Located.map (\_ -> x) item
    in
    case Located.unwrap item of
        Lexer.Sigil (Lexer.Operator op) ->
            appendOperatorTo prevExpr (withCorrectLocation op)
                |> Result.andThen newState

        Lexer.NumericLiteral str ->
            case String.toInt str of
                Just i ->
                    Frontend.Int i
                        |> withCorrectLocation
                        |> appendValueExprTo prevExpr
                        |> Result.andThen newState

                Nothing ->
                    Error_InvalidNumericLiteral str
                        |> Err

        _ ->
            Error_InvalidToken Expecting_Unknown
                |> Err



-- HELPERS


leafToParent : TypeExpressionNestingLeaf () () -> Result Error TypeExpressionNestingParent
leafToParent leaf =
    case leaf of
        TypeExpressionNestingLeaf_Expr expr ->
            -- Cannot nest unless there is a trailing comma!
            Error_TypeDoesNotTakeArgs2 expr
                |> Err

        TypeExpressionNestingLeaf_Bracket { firstExpressions, trailingExpression, parent } ->
            case trailingExpression of
                Just lastType ->
                    -- Cannot nest unless there is a trailing comma!
                    Error_TypeDoesNotTakeArgs2 lastType
                        |> Err

                Nothing ->
                    { expressions = firstExpressions
                    , parent = parent
                    }
                        |> NestingParentType_Bracket
                        |> Ok

        TypeExpressionNestingLeaf_PartialRecord { firstEntries, lastEntry, parent } ->
            case lastEntry of
                LastEntryOfRecord_Empty ->
                    Error_ExpectedKeyWhilstParsingRecord
                        |> Err

                LastEntryOfRecord_Key _ ->
                    Error_ExpectedColonWhilstParsingRecord
                        |> Err

                LastEntryOfRecord_KeyColon key ->
                    { firstEntries = firstEntries
                    , lastEntryName = key
                    , parent = parent
                    }
                        |> NestingParentType_PartialRecord
                        |> Ok

                LastEntryOfRecord_KeyValue _ lastValueType ->
                    Error_TypeDoesNotTakeArgs2 lastValueType
                        |> Err

        TypeExpressionNestingLeaf_TypeWithArgs details ->
            NestingParentType_TypeWithArgs details
                |> Ok

        TypeExpressionNestingLeaf_Function { firstInput, otherInputs, output, parent } ->
            case output of
                Nothing ->
                    NestingParentType_Function
                        { firstInput = firstInput
                        , otherInputs = otherInputs
                        , parent = parent
                        }
                        |> Ok

                Just ( te, () ) ->
                    Error_TypeDoesNotTakeArgs2 te
                        |> Err


parentsToLeafWith : TypeExpression -> Maybe TypeExpressionNestingParent -> TypeExpressionNestingLeaf () ()
parentsToLeafWith expr toMakeIntoLeaf =
    case toMakeIntoLeaf of
        Just (NestingParentType_PartialRecord { firstEntries, lastEntryName, parent }) ->
            TypeExpressionNestingLeaf_PartialRecord
                { firstEntries = firstEntries
                , lastEntry = LastEntryOfRecord_KeyValue lastEntryName expr
                , parent = parent
                }

        Just (NestingParentType_Bracket { expressions, parent }) ->
            TypeExpressionNestingLeaf_Bracket
                { firstExpressions = expressions
                , trailingExpression = Just expr
                , parent = parent
                }

        Just (NestingParentType_TypeWithArgs { qualifiers, name, args, parent }) ->
            TypeExpressionNestingLeaf_TypeWithArgs
                { qualifiers = qualifiers
                , name = name
                , args = expr |> pushOnto args
                , parent = parent
                , phantom = ()
                }

        Just (NestingParentType_Function { firstInput, otherInputs, parent }) ->
            TypeExpressionNestingLeaf_Function
                { firstInput = firstInput
                , otherInputs = otherInputs
                , output = Just ( expr, () )
                , parent = parent
                }

        Nothing ->
            TypeExpressionNestingLeaf_Expr expr


exprAppend :
    TypeExpressionNestingLeaf () ()
    ->
        { qualifiers : List Token.UpperCase
        , name : Token.Token
        }
    -> Result Error (TypeExpressionNestingLeaf () ())
exprAppend currentLeaf token =
    case token.name of
        Token.TokenLowerCase lower ->
            let
                lowerCaseTypeError =
                    Error_LowerCasedTypename
                        { qualifiers = token.qualifiers
                        , name = lower
                        }
                        |> Err

                genericType =
                    if token.qualifiers == [] then
                        TypeExpression_GenericType lower
                            |> Ok

                    else
                        lowerCaseTypeError

                doesNotTakeArgsError expr =
                    Result.andThen (Error_TypeDoesNotTakeArgs expr >> Err) genericType
            in
            case currentLeaf of
                -- We are within a nested bracket.
                TypeExpressionNestingLeaf_Bracket { firstExpressions, trailingExpression, parent } ->
                    case trailingExpression of
                        Nothing ->
                            genericType
                                |> Result.map
                                    (\ty ->
                                        TypeExpressionNestingLeaf_Bracket
                                            { firstExpressions = firstExpressions
                                            , trailingExpression = Just ty
                                            , parent = parent
                                            }
                                    )

                        Just existingRoot ->
                            doesNotTakeArgsError existingRoot

                TypeExpressionNestingLeaf_Expr expr ->
                    doesNotTakeArgsError expr

                TypeExpressionNestingLeaf_PartialRecord pr ->
                    case pr.lastEntry of
                        LastEntryOfRecord_Empty ->
                            if token.qualifiers == [] then
                                TypeExpressionNestingLeaf_PartialRecord
                                    { firstEntries = pr.firstEntries
                                    , lastEntry = LastEntryOfRecord_Key lower
                                    , parent = pr.parent
                                    }
                                    |> Ok

                            else
                                Error_QualifiedRecordKey
                                    { qualifiers = token.qualifiers
                                    , name = token.name
                                    }
                                    |> Err

                        _ ->
                            Result.map2
                                (\newParent gt -> parentsToLeafWith gt (Just newParent))
                                (leafToParent currentLeaf)
                                genericType

                TypeExpressionNestingLeaf_TypeWithArgs _ ->
                    Result.map2
                        (\newParent gt -> parentsToLeafWith gt (Just newParent))
                        (leafToParent currentLeaf)
                        genericType

                TypeExpressionNestingLeaf_Function _ ->
                    Result.map2
                        (\newParent gt -> parentsToLeafWith gt (Just newParent))
                        (leafToParent currentLeaf)
                        genericType

        Token.TokenUpperCase upper ->
            let
                doesNotTakeArgsError expr =
                    Error_TypeDoesNotTakeArgs expr
                        (TypeExpression_NamedType
                            { qualifiers = token.qualifiers
                            , name = upper
                            , args = empty
                            }
                        )
                        |> Err
            in
            case currentLeaf of
                -- We are within a nested bracket.
                TypeExpressionNestingLeaf_Bracket { firstExpressions, trailingExpression } ->
                    case trailingExpression of
                        Nothing ->
                            leafToParent currentLeaf
                                |> Result.map
                                    (\newParent ->
                                        TypeExpressionNestingLeaf_TypeWithArgs
                                            { qualifiers = token.qualifiers
                                            , name = upper
                                            , args = empty
                                            , parent = Just newParent
                                            , phantom = ()
                                            }
                                    )

                        Just existingRoot ->
                            doesNotTakeArgsError existingRoot

                TypeExpressionNestingLeaf_Expr expr ->
                    doesNotTakeArgsError expr

                TypeExpressionNestingLeaf_PartialRecord pr ->
                    case pr.lastEntry of
                        LastEntryOfRecord_Empty ->
                            if token.qualifiers /= [] then
                                Error_QualifiedRecordKey
                                    { qualifiers = token.qualifiers
                                    , name = token.name
                                    }
                                    |> Err

                            else
                                Error_UpperCaseRecordKey upper
                                    |> Err

                        _ ->
                            leafToParent currentLeaf
                                |> Result.map
                                    (\newParent ->
                                        TypeExpressionNestingLeaf_TypeWithArgs
                                            { qualifiers = token.qualifiers
                                            , name = upper
                                            , args = empty
                                            , parent = Just newParent
                                            , phantom = ()
                                            }
                                    )

                TypeExpressionNestingLeaf_TypeWithArgs { name, args, parent } ->
                    TypeExpressionNestingLeaf_TypeWithArgs
                        { qualifiers = token.qualifiers
                        , name = name
                        , args =
                            { qualifiers = token.qualifiers
                            , name = upper
                            , args = empty
                            }
                                |> TypeExpression_NamedType
                                |> pushOnto args
                        , parent = parent
                        , phantom = ()
                        }
                        |> Ok

                TypeExpressionNestingLeaf_Function { firstInput, output } ->
                    Result.andThen
                        (\newParent ->
                            case output of
                                Just ( outputExpr, () ) ->
                                    doesNotTakeArgsError outputExpr

                                Nothing ->
                                    TypeExpressionNestingLeaf_TypeWithArgs
                                        { qualifiers = token.qualifiers
                                        , name = upper
                                        , args = empty
                                        , parent = Just newParent
                                        , phantom = ()
                                        }
                                        |> Ok
                        )
                        (leafToParent currentLeaf)


appendCommaTo : TypeExpressionNestingLeaf () () -> Result Error (TypeExpressionNestingLeaf () ())
appendCommaTo prevExpr =
    case autoCollapseNesting CollapseLevel_Function prevExpr of
        TypeExpressionNestingLeaf_PartialRecord { firstEntries, lastEntry, parent } ->
            case lastEntry of
                LastEntryOfRecord_KeyValue key value ->
                    TypeExpressionNestingLeaf_PartialRecord
                        { firstEntries = ( key, value ) |> pushOnto firstEntries
                        , lastEntry = LastEntryOfRecord_Empty
                        , parent = parent
                        }
                        |> Ok

                _ ->
                    Error_InvalidToken Expecting_Unknown
                        |> Err

        TypeExpressionNestingLeaf_Bracket { firstExpressions, trailingExpression, parent } ->
            case trailingExpression of
                Just trailingExpression_ ->
                    TypeExpressionNestingLeaf_Bracket
                        { firstExpressions = trailingExpression_ |> pushOnto firstExpressions
                        , trailingExpression = Nothing
                        , parent = parent
                        }
                        |> Ok

                Nothing ->
                    Error_InvalidToken Expecting_Unknown
                        |> Err

        _ ->
            Error_InvalidToken Expecting_Unknown
                |> Err


appendColonTo : TypeExpressionNestingLeaf () () -> Result Error (TypeExpressionNestingLeaf () ())
appendColonTo prevExpr =
    case prevExpr of
        TypeExpressionNestingLeaf_PartialRecord { firstEntries, lastEntry, parent } ->
            case lastEntry of
                LastEntryOfRecord_Key key ->
                    TypeExpressionNestingLeaf_PartialRecord
                        { firstEntries = firstEntries
                        , lastEntry = LastEntryOfRecord_KeyColon key
                        , parent = parent
                        }
                        |> Ok

                _ ->
                    Error_InvalidToken Expecting_Unknown
                        |> Err

        _ ->
            Error_InvalidToken Expecting_Unknown
                |> Err


closeBracket :
    Stack TypeExpression
    -> Maybe TypeExpression
    -> Maybe TypeExpressionNestingParent
    -> Result Error (TypeExpressionNestingLeaf () ())
closeBracket argStack mLastExpression mParent =
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
            parentsToLeafWith expr mParent
                |> Ok

        Err e ->
            Err e


closeRecord :
    PartialRecord
    -> Result Error (TypeExpressionNestingLeaf () ())
closeRecord { firstEntries, lastEntry, parent } =
    let
        fromRecord recordEntries =
            let
                record =
                    TypeExpression_Record recordEntries
            in
            parentsToLeafWith record parent
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

        ExpressionTypeExpressionNestingLeaf_Expr locatedexpr ->
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

        ExpressionTypeExpressionNestingLeaf_Expr locatedExpr ->
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
            case autoCollapseNesting CollapseLevel_Function partialExpr of
                TypeExpressionNestingLeaf_Expr expr ->
                    typeExpressionToConcreteType expr
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

                ExpressionTypeExpressionNestingLeaf_Expr expr ->
                    { name = name
                    , args = args
                    , valueExpr__ = expr
                    }
                        |> ValueDeclaration
                        |> Ok
                        |> Just



-- helper functions


type CollapseLevel phantomFunctionWithOutput
    = CollapseLevel_TypeWithArgs phantomFunctionWithOutput
    | CollapseLevel_Function


autoCollapseNesting : CollapseLevel phantomFunctionWithOutput -> TypeExpressionNestingLeaf () () -> TypeExpressionNestingLeaf phantomFunctionWithOutput Never
autoCollapseNesting collapseLevel pte =
    case pte of
        TypeExpressionNestingLeaf_TypeWithArgs { qualifiers, name, args, parent } ->
            let
                newTypeExpr =
                    TypeExpression_NamedType { qualifiers = qualifiers, name = name, args = args }
            in
            parentsToLeafWith newTypeExpr parent
                |> autoCollapseNesting collapseLevel

        TypeExpressionNestingLeaf_Expr expr ->
            TypeExpressionNestingLeaf_Expr expr

        TypeExpressionNestingLeaf_Bracket b ->
            TypeExpressionNestingLeaf_Bracket b

        TypeExpressionNestingLeaf_PartialRecord pr ->
            TypeExpressionNestingLeaf_PartialRecord pr

        TypeExpressionNestingLeaf_Function { firstInput, otherInputs, output, parent } ->
            case ( collapseLevel, output ) of
                ( CollapseLevel_TypeWithArgs phantom, _ ) ->
                    TypeExpressionNestingLeaf_Function
                        { firstInput = firstInput
                        , otherInputs = otherInputs
                        , output = output |> Maybe.map (\( o, _ ) -> ( o, phantom ))
                        , parent = parent
                        }

                ( CollapseLevel_Function, Nothing ) ->
                    TypeExpressionNestingLeaf_Function
                        { firstInput = firstInput
                        , otherInputs = otherInputs
                        , output = Nothing
                        , parent = parent
                        }

                ( CollapseLevel_Function, Just ( outputExpr, () ) ) ->
                    let
                        newTypeExpr =
                            TypeExpression_Function
                                { firstInput = firstInput
                                , otherInputs = otherInputs |> toList (\x -> x)
                                , output = outputExpr
                                }
                    in
                    parentsToLeafWith newTypeExpr parent
                        |> autoCollapseNesting collapseLevel


type ToConcreteTypeError
    = ToConcreteTypeError_TooManyTupleArgs
        --
        (ConcreteType PossiblyQualified)
        (ConcreteType PossiblyQualified)
        (ConcreteType PossiblyQualified)
        (ConcreteType PossiblyQualified)
        (List (ConcreteType PossiblyQualified))


typeExpressionToConcreteType : TypeExpression -> Result ToConcreteTypeError (ConcreteType PossiblyQualified)
typeExpressionToConcreteType pte =
    case pte of
        TypeExpression_NamedType { qualifiers, name, args } ->
            let
                (Token.UpperCase sName) =
                    name

                mModuleName =
                    if qualifiers == [] then
                        Nothing

                    else
                        qualifiers
                            |> List.map (\(Token.UpperCase s) -> s)
                            |> String.join "."
                            |> Just
            in
            args
                |> toList typeExpressionToConcreteType
                |> collectList (\x -> x)
                |> Result.map
                    (\goodArgs ->
                        { qualifiedness = Qualifiedness.PossiblyQualified mModuleName
                        , name = sName
                        , args = goodArgs
                        }
                            |> ConcreteType.UserDefinedType
                    )

        TypeExpression_GenericType name ->
            let
                (Token.LowerCase sName) =
                    name
            in
            { qualifiedness = Qualifiedness.PossiblyQualified Nothing
            , name = sName
            , args = []
            }
                |> ConcreteType.UserDefinedType
                |> Ok

        TypeExpression_Unit ->
            ConcreteType.Unit
                |> Ok

        TypeExpression_Bracketed ty ->
            typeExpressionToConcreteType ty

        TypeExpression_Tuple first second [] ->
            Result.map2
                ConcreteType.Tuple
                (typeExpressionToConcreteType first)
                (typeExpressionToConcreteType second)

        TypeExpression_Tuple first second (third :: []) ->
            Result.map3
                ConcreteType.Tuple3
                (typeExpressionToConcreteType first)
                (typeExpressionToConcreteType second)
                (typeExpressionToConcreteType third)

        TypeExpression_Tuple first second (third :: fouth :: rest) ->
            Result.map5
                ToConcreteTypeError_TooManyTupleArgs
                (typeExpressionToConcreteType first)
                (typeExpressionToConcreteType second)
                (typeExpressionToConcreteType third)
                (typeExpressionToConcreteType fouth)
                (rest
                    |> collectList typeExpressionToConcreteType
                )
                |> Result.andThen Err

        TypeExpression_Record keyValues ->
            keyValues
                |> collectList
                    (\( Token.LowerCase key, value ) ->
                        typeExpressionToConcreteType value
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
                (typeExpressionToConcreteType functionTypeExpr.firstInput)
                (functionTypeExpr.otherInputs
                    |> List.reverse
                    |> collectList typeExpressionToConcreteType
                )
                (typeExpressionToConcreteType functionTypeExpr.output)


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
