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
    = State_BlockStart
    | State_Error_Recovery
    | State_BlockFirstItem BlockFirstItem
    | State_BlockSecondItem BlockSecondItem


type alias State_ =
    { previousBlocks : List (Result ( State, Error ) Block)
    , state : State
    }


type BlockFirstItem
    = BlockFirstItem_Type
    | BlockFirstItem_Module
    | BlockFirstItem_Name Token.ValueOrFunction


type BlockSecondItem
    = BlockSecondItem_TypeAlias
    | BlockSecondItem_CustomTypeNamed Token.TypeOrConstructor


type Error
    = Error_InvalidToken LexItem
    | Error_MisplacedKeyword Keyword
    | Error_BlockStartsWithTypeOrConstructor Token.TypeOrConstructor
    | Error_TypeNameStartsWithLowerCase Token.ValueOrFunction
    | Error_Panic String


parser : List LexItem -> List (Result ( State, Error ) Block)
parser items =
    parserHelp
        items
        { previousBlocks = []
        , state = State_BlockStart
        }


parserHelp : List LexItem -> State_ -> List (Result ( State, Error ) Block)
parserHelp items state =
    case items of
        item :: rest ->
            let
                newState =
                    case state.state of
                        State_Error_Recovery ->
                            parseBlockStart item
                                |> Result.withDefault State_Error_Recovery
                                |> Ok

                        State_BlockStart ->
                            parseBlockStart item

                        State_BlockFirstItem BlockFirstItem_Type ->
                            parseTypeBlock item

                        State_BlockFirstItem BlockFirstItem_Module ->
                            Debug.todo ""

                        State_BlockFirstItem (BlockFirstItem_Name name) ->
                            Debug.todo ""

                        State_BlockSecondItem BlockSecondItem_TypeAlias ->
                            Debug.todo ""

                        State_BlockSecondItem (BlockSecondItem_CustomTypeNamed name) ->
                            Debug.todo ""
            in
            case newState of
                Ok newState_ ->
                    parserHelp rest { previousBlocks = state.previousBlocks, state = newState_ }

                Err error ->
                    parserHelp
                        rest
                        { previousBlocks = Err ( state.state, error ) :: state.previousBlocks
                        , state = State_Error_Recovery
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
                    Ok (State_BlockFirstItem BlockFirstItem_Type)

                Token.TokenKeyword Token.Module ->
                    Ok (State_BlockFirstItem BlockFirstItem_Module)

                Token.TokenKeyword other ->
                    Err (Error_MisplacedKeyword other)

                Token.TokenValueOrFunction valOrFunc ->
                    Ok (State_BlockFirstItem (BlockFirstItem_Name valOrFunc))

                Token.TokenTypeOrConstructor typeOrConstructor ->
                    Err (Error_BlockStartsWithTypeOrConstructor typeOrConstructor)

        Lexer.Newlines _ _ ->
            Ok State_BlockStart

        _ ->
            Err (Error_InvalidToken item)


parseTypeBlock : LexItem -> Result Error State
parseTypeBlock item =
    case item of
        Lexer.Token str ->
            case Token.classifyToken str of
                Token.TokenKeyword Token.Alias ->
                    Ok (State_BlockSecondItem BlockSecondItem_TypeAlias)

                Token.TokenKeyword other ->
                    Err (Error_MisplacedKeyword other)

                Token.TokenTypeOrConstructor typeOrConstructor ->
                    Ok (State_BlockSecondItem (BlockSecondItem_CustomTypeNamed typeOrConstructor))

                Token.TokenValueOrFunction valOrFunc ->
                    Err (Error_TypeNameStartsWithLowerCase valOrFunc)

        Lexer.Newlines _ 0 ->
            Ok State_BlockStart

        Lexer.Newlines _ _ ->
            Ok (State_BlockFirstItem BlockFirstItem_Type)

        _ ->
            Err (Error_InvalidToken item)


parserTypeAlias : LexItem -> Result Error State
parserTypeAlias item =
    case item of
        Lexer.Token str ->
            case Token.classifyToken str of
                Token.TokenKeyword other ->
                    Err (Error_MisplacedKeyword other)

                Token.TokenTypeOrConstructor typeOrConstructor ->
                    Ok (State_BlockSecondItem (BlockSecondItem_CustomTypeNamed typeOrConstructor))

                Token.TokenValueOrFunction valOrFunc ->
                    Err (Error_TypeNameStartsWithLowerCase valOrFunc)

        Lexer.Newlines _ 0 ->
            Ok State_BlockStart

        Lexer.Newlines _ _ ->
            Ok (State_BlockSecondItem BlockSecondItem_TypeAlias)

        _ ->
            Err (Error_InvalidToken item)


blockFromState : State -> Result Error (Maybe Block)
blockFromState state =
    case state of
        State_Error_Recovery ->
            Ok Nothing

        State_BlockStart ->
            Ok Nothing

        State_BlockFirstItem firstItem ->
            Debug.todo "handle incomplete block"

        State_BlockSecondItem firstItem ->
            Debug.todo "handle incomplete block"
