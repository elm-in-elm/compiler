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
import Parser exposing (oneOf)
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
    | TypeExpression_PartialRecord
        { firstEntries : Stack ( String, PartialTypeExpression )
        , lastEntry : LastEntryOfRecord
        }
    | TypeExpression_Record (Stack ( String, PartialTypeExpression ))


type LastEntryOfRecord
    = LastEntryOfRecord_Empty
    | LastEntryOfRecord_Key String
    | LastEntryOfRecord_KeyColon String
    | LastEntryOfRecord_KeyValue String PartialTypeExpression


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
    | Error_ExpectedColonWhilstParsingRecord
    | Error_ExpectedKeyWhilstParsingRecord
    | Error_TypeDoesNotTakeArgs PartialTypeExpression PartialTypeExpression
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



-- parsers


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
                            case partialTypeExpressionToConcreteType expr of
                                Ok concreteType ->
                                    { ty = name
                                    , expr = concreteType
                                    }
                                        |> TypeAlias
                                        |> ParseResult_Complete

                                Err () ->
                                    Error_PartwayThroughTypeAlias
                                        |> ParseResult_Err

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
                            case partialTypeExpressionToConcreteType expr of
                                Ok conceteType ->
                                    { ty = name
                                    , expr = conceteType
                                    }
                                        |> TypeAlias
                                        |> ParseResult_Complete

                                Err () ->
                                    Error_PartwayThroughTypeAlias
                                        |> ParseResult_Err

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
parserTypeExpr newState prevExpr item =
    case item of
        Lexer.Token str ->
            exprAppend prevExpr (addArgumentToType (TokenOrType_Token str))
                |> partialTypeExpressionToParseResult newState

        Lexer.Sigil (Lexer.Bracket Lexer.Round role) ->
            case role of
                Lexer.Open ->
                    { bracketStack =
                        Nothing
                            |> pushOnto prevExpr.bracketStack
                    , root = prevExpr.root
                    }
                        |> TypeExpressionResult_Progress
                        |> newState

                Lexer.Close ->
                    case pop prevExpr.bracketStack of
                        Just ( mexpr, poppedBracketStack ) ->
                            let
                                expr =
                                    case mexpr of
                                        Just expr_ ->
                                            TypeExpression_Bracketed expr_

                                        Nothing ->
                                            TypeExpression_Unit
                            in
                            exprAppend
                                { bracketStack = poppedBracketStack
                                , root = prevExpr.root
                                }
                                (addArgumentToType (TokenOrType_Type expr))
                                |> partialTypeExpressionToParseResult newState

                        _ ->
                            -- TODO(harry): can we add information about the
                            -- bracket we are expecting here?
                            Error_UnmatchedBracket Lexer.Round Lexer.Close
                                |> ParseResult_Err

        Lexer.Sigil (Lexer.Bracket Lexer.Curly Lexer.Open) ->
            let
                newType =
                    TypeExpression_PartialRecord
                        { firstEntries = empty
                        , lastEntry = LastEntryOfRecord_Empty
                        }
            in
            exprAppend prevExpr (addArgumentToType (TokenOrType_Type newType))
                |> partialTypeExpressionToParseResult newState

        Lexer.Sigil Lexer.Colon ->
            addThingToPartialRecord ThingToAddToPartialRecord_Colon prevExpr item newState

        Lexer.Sigil Lexer.Comma ->
            addThingToPartialRecord ThingToAddToPartialRecord_Comma prevExpr item newState

        Lexer.Sigil (Lexer.Bracket Lexer.Curly Lexer.Close) ->
            addThingToPartialRecord ThingToAddToPartialRecord_Close prevExpr item newState

        Lexer.Newlines _ 0 ->
            case ( pop prevExpr.bracketStack, prevExpr.root ) of
                ( Just _, _ ) ->
                    Error_PartwayThroughTypeAlias
                        |> ParseResult_Err

                ( Nothing, Just expr ) ->
                    TypeExpressionResult_Done expr
                        |> newState

                ( Nothing, Nothing ) ->
                    Error_PartwayThroughTypeAlias
                        |> ParseResult_Err

        Lexer.Newlines _ _ ->
            ParseResult_Skip

        Lexer.Whitespace _ ->
            ParseResult_Skip

        _ ->
            Error_InvalidToken item Expecting_Unknown
                |> ParseResult_Err


exprAppend :
    PartialTypeExpression2
    -> (Maybe PartialTypeExpression -> Result Error PartialTypeExpression)
    -> Result Error PartialTypeExpression2
exprAppend { bracketStack, root } append =
    case pop bracketStack of
        -- We are in the top level of the type expression; we have
        -- found a closing bracket to match every opening bracket we
        -- have encountered so far whilst parsing the type expression.
        -- (We may have found no brackets at all so far.)
        Nothing ->
            append root
                |> Result.map
                    (\newRoot ->
                        { bracketStack = empty
                        , root = Just newRoot
                        }
                    )

        -- We are within a nested bracket.
        Just ( mlatestTypeExpr, rest ) ->
            append mlatestTypeExpr
                |> Result.map
                    (\newLatestTypeExpr ->
                        { bracketStack =
                            Just newLatestTypeExpr
                                |> pushOnto rest
                        , root = root
                        }
                    )


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
                    partialTypeExpressionToConcreteType expr
                        |> Result.map
                            (\conceteType ->
                                { ty = name
                                , expr = conceteType
                                }
                                    |> TypeAlias
                            )
                        |> Result.mapError (\() -> Error_PartwayThroughTypeAlias)
                        |> Just

                _ ->
                    Error_PartwayThroughTypeAlias
                        |> Err
                        |> Just

        State_BlockCustomType firstItem ->
            Debug.todo "handle incomplete block"



-- helper functions


type TokenOrType
    = TokenOrType_Token String
    | TokenOrType_Type PartialTypeExpression


addToPartialRecord :
    TokenOrType
    ->
        { firstEntries : Stack ( String, PartialTypeExpression )
        , lastEntry : LastEntryOfRecord
        }
    ->
        Result Error
            { firstEntries : Stack ( String, PartialTypeExpression )
            , lastEntry : LastEntryOfRecord
            }
addToPartialRecord tot { firstEntries, lastEntry } =
    let
        newType =
            case tot of
                TokenOrType_Token str ->
                    TypeExpression_NamedType
                        { name = str
                        , args = empty
                        }

                TokenOrType_Type ty ->
                    ty
    in
    case lastEntry of
        LastEntryOfRecord_Empty ->
            case tot of
                TokenOrType_Token str ->
                    { firstEntries = firstEntries
                    , lastEntry =
                        LastEntryOfRecord_Key str
                    }
                        |> Ok

                TokenOrType_Type _ ->
                    Error_ExpectedKeyWhilstParsingRecord
                        |> Err

        LastEntryOfRecord_Key key ->
            Error_ExpectedColonWhilstParsingRecord
                |> Err

        LastEntryOfRecord_KeyColon key ->
            { firstEntries = firstEntries
            , lastEntry =
                LastEntryOfRecord_KeyValue key newType
            }
                |> Ok

        LastEntryOfRecord_KeyValue key (TypeExpression_NamedType { name, args }) ->
            { firstEntries = firstEntries
            , lastEntry =
                LastEntryOfRecord_KeyValue
                    key
                    (TypeExpression_NamedType
                        { name = name
                        , args =
                            newType
                                |> pushOnto args
                        }
                    )
            }
                |> Ok

        LastEntryOfRecord_KeyValue _ TypeExpression_Unit ->
            Error_TypeDoesNotTakeArgs TypeExpression_Unit newType
                |> Err

        LastEntryOfRecord_KeyValue _ ((TypeExpression_Bracketed _) as ty) ->
            Error_TypeDoesNotTakeArgs ty newType
                |> Err

        LastEntryOfRecord_KeyValue _ ((TypeExpression_Record _) as ty) ->
            Error_TypeDoesNotTakeArgs ty newType
                |> Err

        LastEntryOfRecord_KeyValue key (TypeExpression_PartialRecord innerPartialRecord) ->
            addToPartialRecord tot innerPartialRecord
                |> Result.map
                    (\newInnerPartialRecord ->
                        { firstEntries = firstEntries
                        , lastEntry =
                            LastEntryOfRecord_KeyValue
                                key
                                (TypeExpression_PartialRecord newInnerPartialRecord)
                        }
                    )


addArgumentToType : TokenOrType -> Maybe PartialTypeExpression -> Result Error PartialTypeExpression
addArgumentToType argToAdd mexistingTypeExpr =
    let
        newType =
            case argToAdd of
                TokenOrType_Token str ->
                    TypeExpression_NamedType
                        { name = str
                        , args = empty
                        }

                TokenOrType_Type ty ->
                    ty
    in
    case mexistingTypeExpr of
        Nothing ->
            newType
                |> Ok

        Just (TypeExpression_NamedType { name, args }) ->
            TypeExpression_NamedType
                { name = name
                , args =
                    newType
                        |> pushOnto args
                }
                |> Ok

        Just (TypeExpression_PartialRecord existingPartialRecord) ->
            addToPartialRecord argToAdd existingPartialRecord
                |> Result.map TypeExpression_PartialRecord

        Just ((TypeExpression_Bracketed _) as ty) ->
            Error_TypeDoesNotTakeArgs ty newType
                |> Err

        Just ((TypeExpression_Record _) as ty) ->
            Error_TypeDoesNotTakeArgs ty newType
                |> Err

        Just TypeExpression_Unit ->
            Error_TypeDoesNotTakeArgs TypeExpression_Unit newType
                |> Err


type ThingToAddToPartialRecord
    = ThingToAddToPartialRecord_Colon
    | ThingToAddToPartialRecord_Comma
    | ThingToAddToPartialRecord_Close


addThingToPartialRecord :
    ThingToAddToPartialRecord
    -> PartialTypeExpression2
    -> Lexer.LexItem
    -> (TypeExpressionResult -> ParseResult)
    -> ParseResult
addThingToPartialRecord thing prevExpr item newState =
    exprAppend
        prevExpr
        (\typeExpr ->
            case typeExpr of
                Just (TypeExpression_PartialRecord pr) ->
                    addThingToPartialRecordHelp thing pr
                        |> Result.mapError (\() -> Error_InvalidToken item Expecting_Unknown)

                _ ->
                    Error_InvalidToken item Expecting_Unknown
                        |> Err
        )
        |> partialTypeExpressionToParseResult newState


addThingToPartialRecordHelp :
    ThingToAddToPartialRecord
    ->
        { firstEntries : Stack ( String, PartialTypeExpression )
        , lastEntry : LastEntryOfRecord
        }
    -> Result () PartialTypeExpression
addThingToPartialRecordHelp thing { firstEntries, lastEntry } =
    case ( thing, lastEntry ) of
        ( ThingToAddToPartialRecord_Colon, LastEntryOfRecord_Key key ) ->
            { firstEntries = firstEntries
            , lastEntry = LastEntryOfRecord_KeyColon key
            }
                |> TypeExpression_PartialRecord
                |> Ok

        -- Must come before other `LastEntryOfRecord_KeyValue` cases!
        ( _, LastEntryOfRecord_KeyValue key (TypeExpression_PartialRecord pr) ) ->
            addThingToPartialRecordHelp thing pr
                |> Result.map
                    (\newPr ->
                        { firstEntries = firstEntries
                        , lastEntry =
                            LastEntryOfRecord_KeyValue key newPr
                        }
                            |> TypeExpression_PartialRecord
                    )

        ( ThingToAddToPartialRecord_Comma, LastEntryOfRecord_KeyValue key value ) ->
            { firstEntries = ( key, value ) |> pushOnto firstEntries
            , lastEntry = LastEntryOfRecord_Empty
            }
                |> TypeExpression_PartialRecord
                |> Ok

        ( ThingToAddToPartialRecord_Close, LastEntryOfRecord_KeyValue key value ) ->
            ( key, value )
                |> pushOnto firstEntries
                |> TypeExpression_Record
                |> Ok

        _ ->
            Err ()


partialTypeExpressionToParseResult : (TypeExpressionResult -> ParseResult) -> Result Error PartialTypeExpression2 -> ParseResult
partialTypeExpressionToParseResult newState rnewPartialType =
    case rnewPartialType of
        Ok newPartialType ->
            TypeExpressionResult_Progress newPartialType
                |> newState

        Err e ->
            ParseResult_Err e


{-| TODO(harry): custom error message here
-}
partialTypeExpressionToConcreteType : PartialTypeExpression -> Result () (ConcreteType PossiblyQualified)
partialTypeExpressionToConcreteType pte =
    case pte of
        TypeExpression_NamedType { name, args } ->
            args
                |> toList partialTypeExpressionToConcreteType
                |> collectList
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

        TypeExpression_PartialRecord ty ->
            Err ()

        TypeExpression_Record keyValues ->
            keyValues
                |> toList
                    (\( key, value ) ->
                        partialTypeExpressionToConcreteType value
                            |> Result.map (\concreteValue -> ( key, concreteValue ))
                    )
                |> collectList
                |> Result.map
                    (\goodKeyValues ->
                        Dict.fromList goodKeyValues
                            |> ConcreteType.Record
                    )


recoverErrors : ParseResult -> ParseResult
recoverErrors res =
    case res of
        ParseResult_Err _ ->
            ParseResult_Ok State_Error_Recovery

        _ ->
            res


collectList : List (Result e o) -> Result e (List o)
collectList =
    collectListHelp []


collectListHelp : List o -> List (Result e o) -> Result e (List o)
collectListHelp new old =
    case old of
        (Ok curr) :: rest ->
            collectListHelp (curr :: new) rest

        (Err e) :: rest ->
            Err e

        [] ->
            Ok new


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
