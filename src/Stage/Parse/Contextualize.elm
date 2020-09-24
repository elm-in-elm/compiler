module Stage.Parse.Contextualize exposing (..)

import Elm.AST.Frontend as Frontend exposing (Expr(..), LocatedExpr, LocatedPattern, Pattern(..))
import Elm.Data.Declaration
import Elm.Data.Exposing
import Elm.Data.Located as Located exposing (Located)
import Elm.Data.Module exposing (Module, ModuleType(..))
import Elm.Data.ModuleName exposing (ModuleName)
import Elm.Data.Qualifiedness exposing (PossiblyQualified(..))
import Elm.Data.Type.Concrete as ConcreteType exposing (ConcreteType)
import Elm.Data.TypeAnnotation exposing (TypeAnnotation)
import Elm.Data.VarName exposing (VarName)
import Stage.Parse.Lexer as Lexer exposing (LexItem)
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
    | CustomType
        { ty : VarName
        , constructors : List (Elm.Data.Declaration.Constructor PossiblyQualified)
        }


type State
    = StateBlockStart
    | StateErrorRecovery
    | StateBlockFirstItem BlockFirstItem
    | StateBlockSecondItem BlockSecondItem


type alias State_ =
    { previousBlocks : List (Result ( State, Error ) Block)
    , state : State
    }


type BlockFirstItem
    = BlockFirstItemType
    | BlockFirstItemModule
    | BlockFirstItemName Token.ValueOrFunction


type BlockSecondItem
    = BlockSecondItemTypeAlias
    | BlockSecondItemCustomTypeNamed Token.TypeOrConstructor


type Error
    = ErrorInvalidToken LexItem
    | ErrorMisplacedKeyword Keyword
    | BlockStartsWithTypeOrConstructor Token.TypeOrConstructor
    | TypeNameStartsWithLowerCase Token.ValueOrFunction
    | Panic String


parser : List LexItem -> List (Result ( State, Error ) Block)
parser items =
    parserHelp
        items
        { previousBlocks = []
        , state = StateBlockStart
        }


parserHelp : List LexItem -> State_ -> List (Result ( State, Error ) Block)
parserHelp items state =
    case items of
        item :: rest ->
            let
                newState =
                    case state.state of
                        StateErrorRecovery ->
                            parseBlockStart item
                                |> Result.withDefault StateErrorRecovery
                                |> Ok

                        StateBlockStart ->
                            parseBlockStart item

                        StateBlockFirstItem BlockFirstItemType ->
                            parseTypeBlock item

                        StateBlockFirstItem BlockFirstItemModule ->
                            Debug.todo ""

                        StateBlockFirstItem (BlockFirstItemName name) ->
                            Debug.todo ""

                        StateBlockSecondItem BlockSecondItemTypeAlias ->
                            Debug.todo ""

                        StateBlockSecondItem (BlockSecondItemCustomTypeNamed name) ->
                            Debug.todo ""
            in
            case newState of
                Ok newState_ ->
                    parserHelp rest { previousBlocks = state.previousBlocks, state = newState_ }

                Err error ->
                    parserHelp
                        rest
                        { previousBlocks = Err ( state.state, error ) :: state.previousBlocks
                        , state = StateErrorRecovery
                        }

        [] ->
            List.reverse state.previousBlocks


{-|


### Panics

If the LexItem is `Newlines`.

-}
parseBlockStart : LexItem -> Result Error State
parseBlockStart item =
    case item of
        Lexer.Token str ->
            case Token.classifyToken str of
                Token.TokenKeyword Token.Type ->
                    Ok (StateBlockFirstItem BlockFirstItemType)

                Token.TokenKeyword Token.Module ->
                    Ok (StateBlockFirstItem BlockFirstItemModule)

                Token.TokenKeyword other ->
                    Err (ErrorMisplacedKeyword other)

                Token.TokenValueOrFunction valOrFunc ->
                    Ok (StateBlockFirstItem (BlockFirstItemName valOrFunc))

                Token.TokenTypeOrConstructor typeOrConstructor ->
                    Err (BlockStartsWithTypeOrConstructor typeOrConstructor)

        Lexer.Newlines _ _ ->
            Ok StateBlockStart

        _ ->
            Err (ErrorInvalidToken item)


parseTypeBlock : LexItem -> Result Error State
parseTypeBlock item =
    case item of
        Lexer.Token str ->
            case Token.classifyToken str of
                Token.TokenKeyword Token.Alias ->
                    Ok (StateBlockSecondItem BlockSecondItemTypeAlias)

                Token.TokenKeyword other ->
                    Err (ErrorMisplacedKeyword other)

                Token.TokenTypeOrConstructor typeOrConstructor ->
                    Ok (StateBlockSecondItem (BlockSecondItemCustomTypeNamed typeOrConstructor))

                Token.TokenValueOrFunction valOrFunc ->
                    Err (TypeNameStartsWithLowerCase valOrFunc)

        Lexer.Newlines _ 0 ->
            Ok StateBlockStart

        Lexer.Newlines _ _ ->
            Ok (StateBlockFirstItem BlockFirstItemType)

        _ ->
            Err (ErrorInvalidToken item)


parserTypeAlias : LexItem -> Result Error State
parserTypeAlias item =
    case item of
        Lexer.Token str ->
            case Token.classifyToken str of
                Token.TokenKeyword other ->
                    Err (ErrorMisplacedKeyword other)

                Token.TokenTypeOrConstructor typeOrConstructor ->
                    Ok (StateBlockSecondItem (BlockSecondItemCustomTypeNamed typeOrConstructor))

                Token.TokenValueOrFunction valOrFunc ->
                    Err (TypeNameStartsWithLowerCase valOrFunc)

        Lexer.Newlines _ 0 ->
            Ok StateBlockStart

        Lexer.Newlines _ _ ->
            Ok (StateBlockSecondItem BlockSecondItemTypeAlias)

        _ ->
            Err (ErrorInvalidToken item)


blockFromState : State -> Result Error (Maybe Block)
blockFromState state =
    case state of
        StateErrorRecovery ->
            Ok Nothing

        StateBlockStart ->
            Ok Nothing

        StateBlockFirstItem firstItem ->
            Debug.todo "handle incomplete block"

        StateBlockSecondItem firstItem ->
            Debug.todo "handle incomplete block"
