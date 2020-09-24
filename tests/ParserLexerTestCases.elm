module ParserLexerTestCases exposing (testCases)

import Elm.Data.Located as Located exposing (Located(..))
import Elm.Data.Qualifiedness exposing (PossiblyQualified(..))
import Elm.Data.Type.Concrete exposing (ConcreteType(..))
import Stage.Parse.Contextualize as Contextualize exposing (..)
import Stage.Parse.Lexer as Lexer exposing (..)
import Stage.Parse.Token exposing (TypeOrConstructor(..))
import Test exposing (Test, describe, test)



-- AUTO GENERATED TEST CASES
--
-- Do not edit below this line or your changes will be overwritten by
-- tests/parser-tests/update.js


testCases :
    List
       { contextualized : Maybe (List (Result error1 Block))
       , lexed : Result error (List (Located LexItem))
       , name : String
       , source : String
       }
testCases =
    [ { name = "type-alias"
      , source = """type alias Model = List Int
"""
      , lexed = Ok [
            Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token "type"),
            Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1),
            Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token "alias"),
            Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Whitespace 1),
            Located { end = { col = 17, row = 1 }, start = { col = 12, row = 1 } } (Token "Model"),
            Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Whitespace 1),
            Located { end = { col = 19, row = 1 }, start = { col = 18, row = 1 } } (Sigil Assign),
            Located { end = { col = 20, row = 1 }, start = { col = 19, row = 1 } } (Whitespace 1),
            Located { end = { col = 24, row = 1 }, start = { col = 20, row = 1 } } (Token "List"),
            Located { end = { col = 25, row = 1 }, start = { col = 24, row = 1 } } (Whitespace 1),
            Located { end = { col = 28, row = 1 }, start = { col = 25, row = 1 } } (Token "Int"),
            Located { end = { col = 1, row = 2 }, start = { col = 28, row = 1 } } (Newlines [] 0)]
      , contextualized =Just [Ok (TypeAlias { expr = UserDefinedType { args = [UserDefinedType { args = [], name = "Int", qualifiedness = PossiblyQualified Nothing 
            }], name = "List", qualifiedness = PossiblyQualified Nothing 
            }, ty = TypeOrConstructor "Model" 
            })]
      }
    ]
    