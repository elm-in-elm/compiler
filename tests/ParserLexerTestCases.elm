module ParserLexerTestCases exposing (shouldNotParseTestCases, shouldParseTestCases)

import Dict
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


shouldParseTestCases :
    List
        { contextualized : Maybe (List (Result never Block))
        , lexed : Result Never (List (Located LexItem))
        , name : String
        , source : String
        }
shouldParseTestCases =
    [ { name = "type-alias"
      , source = """type alias Model = List Int
"""
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token "type")
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token "alias")
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Whitespace 1)
                , Located { end = { col = 17, row = 1 }, start = { col = 12, row = 1 } } (Token "Model")
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Whitespace 1)
                , Located { end = { col = 19, row = 1 }, start = { col = 18, row = 1 } } (Sigil Assign)
                , Located { end = { col = 20, row = 1 }, start = { col = 19, row = 1 } } (Whitespace 1)
                , Located { end = { col = 24, row = 1 }, start = { col = 20, row = 1 } } (Token "List")
                , Located { end = { col = 25, row = 1 }, start = { col = 24, row = 1 } } (Whitespace 1)
                , Located { end = { col = 28, row = 1 }, start = { col = 25, row = 1 } } (Token "Int")
                , Located { end = { col = 1, row = 2 }, start = { col = 28, row = 1 } } (Newlines [] 0)
                ]
      , contextualized =
            Just
                [ Ok
                    (TypeAlias
                        { expr =
                            UserDefinedType
                                { args =
                                    [ UserDefinedType { args = [], name = "Int", qualifiedness = PossiblyQualified Nothing }
                                    ]
                                , name = "List"
                                , qualifiedness = PossiblyQualified Nothing
                                }
                        , ty = TypeOrConstructor "Model"
                        }
                    )
                ]
      }
    , { name = "type-alias-bracket-in-record"
      , source = """type alias Ty = { hi: (Int) }
"""
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token "type")
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token "alias")
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Whitespace 1)
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Token "Ty")
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Whitespace 1)
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Sigil Assign)
                , Located { end = { col = 17, row = 1 }, start = { col = 16, row = 1 } } (Whitespace 1)
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Sigil (Bracket Curly Open))
                , Located { end = { col = 19, row = 1 }, start = { col = 18, row = 1 } } (Whitespace 1)
                , Located { end = { col = 21, row = 1 }, start = { col = 19, row = 1 } } (Token "hi")
                , Located { end = { col = 22, row = 1 }, start = { col = 21, row = 1 } } (Sigil Colon)
                , Located { end = { col = 23, row = 1 }, start = { col = 22, row = 1 } } (Whitespace 1)
                , Located { end = { col = 24, row = 1 }, start = { col = 23, row = 1 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 27, row = 1 }, start = { col = 24, row = 1 } } (Token "Int")
                , Located { end = { col = 28, row = 1 }, start = { col = 27, row = 1 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 29, row = 1 }, start = { col = 28, row = 1 } } (Whitespace 1)
                , Located { end = { col = 30, row = 1 }, start = { col = 29, row = 1 } } (Sigil (Bracket Curly Close))
                , Located { end = { col = 1, row = 2 }, start = { col = 30, row = 1 } } (Newlines [] 0)
                ]
      , contextualized =
            Just
                [ Ok
                    (TypeAlias
                        { expr =
                            Record
                                (Dict.fromList
                                    [ ( "hi", UserDefinedType { args = [], name = "Int", qualifiedness = PossiblyQualified Nothing } )
                                    ]
                                )
                        , ty = TypeOrConstructor "Ty"
                        }
                    )
                ]
      }
    , { name = "type-alias-funky-indentation"
      , source = """type alias
    Model = List Int
"""
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token "type")
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token "alias")
                , Located { end = { col = 5, row = 2 }, start = { col = 11, row = 1 } } (Newlines [] 4)
                , Located { end = { col = 10, row = 2 }, start = { col = 5, row = 2 } } (Token "Model")
                , Located { end = { col = 11, row = 2 }, start = { col = 10, row = 2 } } (Whitespace 1)
                , Located { end = { col = 12, row = 2 }, start = { col = 11, row = 2 } } (Sigil Assign)
                , Located { end = { col = 13, row = 2 }, start = { col = 12, row = 2 } } (Whitespace 1)
                , Located { end = { col = 17, row = 2 }, start = { col = 13, row = 2 } } (Token "List")
                , Located { end = { col = 18, row = 2 }, start = { col = 17, row = 2 } } (Whitespace 1)
                , Located { end = { col = 21, row = 2 }, start = { col = 18, row = 2 } } (Token "Int")
                , Located { end = { col = 1, row = 3 }, start = { col = 21, row = 2 } } (Newlines [] 0)
                ]
      , contextualized =
            Just
                [ Ok
                    (TypeAlias
                        { expr =
                            UserDefinedType
                                { args =
                                    [ UserDefinedType { args = [], name = "Int", qualifiedness = PossiblyQualified Nothing }
                                    ]
                                , name = "List"
                                , qualifiedness = PossiblyQualified Nothing
                                }
                        , ty = TypeOrConstructor "Model"
                        }
                    )
                ]
      }
    , { name = "type-alias-funky-indentation-2"
      , source = """type alias
    Model =
 List Int
"""
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token "type")
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token "alias")
                , Located { end = { col = 5, row = 2 }, start = { col = 11, row = 1 } } (Newlines [] 4)
                , Located { end = { col = 10, row = 2 }, start = { col = 5, row = 2 } } (Token "Model")
                , Located { end = { col = 11, row = 2 }, start = { col = 10, row = 2 } } (Whitespace 1)
                , Located { end = { col = 12, row = 2 }, start = { col = 11, row = 2 } } (Sigil Assign)
                , Located { end = { col = 2, row = 3 }, start = { col = 12, row = 2 } } (Newlines [] 1)
                , Located { end = { col = 6, row = 3 }, start = { col = 2, row = 3 } } (Token "List")
                , Located { end = { col = 7, row = 3 }, start = { col = 6, row = 3 } } (Whitespace 1)
                , Located { end = { col = 10, row = 3 }, start = { col = 7, row = 3 } } (Token "Int")
                , Located { end = { col = 1, row = 4 }, start = { col = 10, row = 3 } } (Newlines [] 0)
                ]
      , contextualized =
            Just
                [ Ok
                    (TypeAlias
                        { expr =
                            UserDefinedType
                                { args =
                                    [ UserDefinedType { args = [], name = "Int", qualifiedness = PossiblyQualified Nothing }
                                    ]
                                , name = "List"
                                , qualifiedness = PossiblyQualified Nothing
                                }
                        , ty = TypeOrConstructor "Model"
                        }
                    )
                ]
      }
    , { name = "type-alias-record-3-entries"
      , source = """type alias Ty = { a: A, b: B, c: C }
"""
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token "type")
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token "alias")
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Whitespace 1)
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Token "Ty")
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Whitespace 1)
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Sigil Assign)
                , Located { end = { col = 17, row = 1 }, start = { col = 16, row = 1 } } (Whitespace 1)
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Sigil (Bracket Curly Open))
                , Located { end = { col = 19, row = 1 }, start = { col = 18, row = 1 } } (Whitespace 1)
                , Located { end = { col = 20, row = 1 }, start = { col = 19, row = 1 } } (Token "a")
                , Located { end = { col = 21, row = 1 }, start = { col = 20, row = 1 } } (Sigil Colon)
                , Located { end = { col = 22, row = 1 }, start = { col = 21, row = 1 } } (Whitespace 1)
                , Located { end = { col = 23, row = 1 }, start = { col = 22, row = 1 } } (Token "A")
                , Located { end = { col = 24, row = 1 }, start = { col = 23, row = 1 } } (Sigil Comma)
                , Located { end = { col = 25, row = 1 }, start = { col = 24, row = 1 } } (Whitespace 1)
                , Located { end = { col = 26, row = 1 }, start = { col = 25, row = 1 } } (Token "b")
                , Located { end = { col = 27, row = 1 }, start = { col = 26, row = 1 } } (Sigil Colon)
                , Located { end = { col = 28, row = 1 }, start = { col = 27, row = 1 } } (Whitespace 1)
                , Located { end = { col = 29, row = 1 }, start = { col = 28, row = 1 } } (Token "B")
                , Located { end = { col = 30, row = 1 }, start = { col = 29, row = 1 } } (Sigil Comma)
                , Located { end = { col = 31, row = 1 }, start = { col = 30, row = 1 } } (Whitespace 1)
                , Located { end = { col = 32, row = 1 }, start = { col = 31, row = 1 } } (Token "c")
                , Located { end = { col = 33, row = 1 }, start = { col = 32, row = 1 } } (Sigil Colon)
                , Located { end = { col = 34, row = 1 }, start = { col = 33, row = 1 } } (Whitespace 1)
                , Located { end = { col = 35, row = 1 }, start = { col = 34, row = 1 } } (Token "C")
                , Located { end = { col = 36, row = 1 }, start = { col = 35, row = 1 } } (Whitespace 1)
                , Located { end = { col = 37, row = 1 }, start = { col = 36, row = 1 } } (Sigil (Bracket Curly Close))
                , Located { end = { col = 1, row = 2 }, start = { col = 37, row = 1 } } (Newlines [] 0)
                ]
      , contextualized =
            Just
                [ Ok
                    (TypeAlias
                        { expr =
                            Record
                                (Dict.fromList
                                    [ ( "a", UserDefinedType { args = [], name = "A", qualifiedness = PossiblyQualified Nothing } )
                                    , ( "b", UserDefinedType { args = [], name = "B", qualifiedness = PossiblyQualified Nothing } )
                                    , ( "c", UserDefinedType { args = [], name = "C", qualifiedness = PossiblyQualified Nothing } )
                                    ]
                                )
                        , ty = TypeOrConstructor "Ty"
                        }
                    )
                ]
      }
    , { name = "type-alias-record-empty"
      , source = """type alias Ty = {}
"""
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token "type")
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token "alias")
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Whitespace 1)
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Token "Ty")
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Whitespace 1)
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Sigil Assign)
                , Located { end = { col = 17, row = 1 }, start = { col = 16, row = 1 } } (Whitespace 1)
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Sigil (Bracket Curly Open))
                , Located { end = { col = 19, row = 1 }, start = { col = 18, row = 1 } } (Sigil (Bracket Curly Close))
                , Located { end = { col = 1, row = 2 }, start = { col = 19, row = 1 } } (Newlines [] 0)
                ]
      , contextualized =
            Just
                [ Ok (TypeAlias { expr = Record (Dict.fromList []), ty = TypeOrConstructor "Ty" })
                ]
      }
    , { name = "type-alias-record-empty-multiline"
      , source = """type alias Ty = {


    }
"""
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token "type")
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token "alias")
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Whitespace 1)
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Token "Ty")
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Whitespace 1)
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Sigil Assign)
                , Located { end = { col = 17, row = 1 }, start = { col = 16, row = 1 } } (Whitespace 1)
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Sigil (Bracket Curly Open))
                , Located { end = { col = 5, row = 4 }, start = { col = 18, row = 1 } }
                    (Newlines
                        [ 0
                        , 0
                        ]
                        4
                    )
                , Located { end = { col = 6, row = 4 }, start = { col = 5, row = 4 } } (Sigil (Bracket Curly Close))
                , Located { end = { col = 1, row = 5 }, start = { col = 6, row = 4 } } (Newlines [] 0)
                ]
      , contextualized =
            Just
                [ Ok (TypeAlias { expr = Record (Dict.fromList []), ty = TypeOrConstructor "Ty" })
                ]
      }
    , { name = "type-alias-record-in-bracket"
      , source = """type alias Ty = ({ hi: 6 })
"""
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token "type")
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token "alias")
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Whitespace 1)
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Token "Ty")
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Whitespace 1)
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Sigil Assign)
                , Located { end = { col = 17, row = 1 }, start = { col = 16, row = 1 } } (Whitespace 1)
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 19, row = 1 }, start = { col = 18, row = 1 } } (Sigil (Bracket Curly Open))
                , Located { end = { col = 20, row = 1 }, start = { col = 19, row = 1 } } (Whitespace 1)
                , Located { end = { col = 22, row = 1 }, start = { col = 20, row = 1 } } (Token "hi")
                , Located { end = { col = 23, row = 1 }, start = { col = 22, row = 1 } } (Sigil Colon)
                , Located { end = { col = 24, row = 1 }, start = { col = 23, row = 1 } } (Whitespace 1)
                , Located { end = { col = 25, row = 1 }, start = { col = 24, row = 1 } } (Token "6")
                , Located { end = { col = 26, row = 1 }, start = { col = 25, row = 1 } } (Whitespace 1)
                , Located { end = { col = 27, row = 1 }, start = { col = 26, row = 1 } } (Sigil (Bracket Curly Close))
                , Located { end = { col = 28, row = 1 }, start = { col = 27, row = 1 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 1, row = 2 }, start = { col = 28, row = 1 } } (Newlines [] 0)
                ]
      , contextualized =
            Just
                [ Ok
                    (TypeAlias
                        { expr =
                            Record
                                (Dict.fromList
                                    [ ( "hi", UserDefinedType { args = [], name = "6", qualifiedness = PossiblyQualified Nothing } )
                                    ]
                                )
                        , ty = TypeOrConstructor "Ty"
                        }
                    )
                ]
      }
    , { name = "type-alias-record-nested"
      , source = """type alias Ty =
    { hi:  { a: 7, b: List String }
    , ih: CustomType A B C (D E)
    }
"""
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token "type")
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token "alias")
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Whitespace 1)
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Token "Ty")
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Whitespace 1)
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Sigil Assign)
                , Located { end = { col = 5, row = 2 }, start = { col = 16, row = 1 } } (Newlines [] 4)
                , Located { end = { col = 6, row = 2 }, start = { col = 5, row = 2 } } (Sigil (Bracket Curly Open))
                , Located { end = { col = 7, row = 2 }, start = { col = 6, row = 2 } } (Whitespace 1)
                , Located { end = { col = 9, row = 2 }, start = { col = 7, row = 2 } } (Token "hi")
                , Located { end = { col = 10, row = 2 }, start = { col = 9, row = 2 } } (Sigil Colon)
                , Located { end = { col = 12, row = 2 }, start = { col = 10, row = 2 } } (Whitespace 2)
                , Located { end = { col = 13, row = 2 }, start = { col = 12, row = 2 } } (Sigil (Bracket Curly Open))
                , Located { end = { col = 14, row = 2 }, start = { col = 13, row = 2 } } (Whitespace 1)
                , Located { end = { col = 15, row = 2 }, start = { col = 14, row = 2 } } (Token "a")
                , Located { end = { col = 16, row = 2 }, start = { col = 15, row = 2 } } (Sigil Colon)
                , Located { end = { col = 17, row = 2 }, start = { col = 16, row = 2 } } (Whitespace 1)
                , Located { end = { col = 18, row = 2 }, start = { col = 17, row = 2 } } (Token "7")
                , Located { end = { col = 19, row = 2 }, start = { col = 18, row = 2 } } (Sigil Comma)
                , Located { end = { col = 20, row = 2 }, start = { col = 19, row = 2 } } (Whitespace 1)
                , Located { end = { col = 21, row = 2 }, start = { col = 20, row = 2 } } (Token "b")
                , Located { end = { col = 22, row = 2 }, start = { col = 21, row = 2 } } (Sigil Colon)
                , Located { end = { col = 23, row = 2 }, start = { col = 22, row = 2 } } (Whitespace 1)
                , Located { end = { col = 27, row = 2 }, start = { col = 23, row = 2 } } (Token "List")
                , Located { end = { col = 28, row = 2 }, start = { col = 27, row = 2 } } (Whitespace 1)
                , Located { end = { col = 34, row = 2 }, start = { col = 28, row = 2 } } (Token "String")
                , Located { end = { col = 35, row = 2 }, start = { col = 34, row = 2 } } (Whitespace 1)
                , Located { end = { col = 36, row = 2 }, start = { col = 35, row = 2 } } (Sigil (Bracket Curly Close))
                , Located { end = { col = 5, row = 3 }, start = { col = 36, row = 2 } } (Newlines [] 4)
                , Located { end = { col = 6, row = 3 }, start = { col = 5, row = 3 } } (Sigil Comma)
                , Located { end = { col = 7, row = 3 }, start = { col = 6, row = 3 } } (Whitespace 1)
                , Located { end = { col = 9, row = 3 }, start = { col = 7, row = 3 } } (Token "ih")
                , Located { end = { col = 10, row = 3 }, start = { col = 9, row = 3 } } (Sigil Colon)
                , Located { end = { col = 11, row = 3 }, start = { col = 10, row = 3 } } (Whitespace 1)
                , Located { end = { col = 21, row = 3 }, start = { col = 11, row = 3 } } (Token "CustomType")
                , Located { end = { col = 22, row = 3 }, start = { col = 21, row = 3 } } (Whitespace 1)
                , Located { end = { col = 23, row = 3 }, start = { col = 22, row = 3 } } (Token "A")
                , Located { end = { col = 24, row = 3 }, start = { col = 23, row = 3 } } (Whitespace 1)
                , Located { end = { col = 25, row = 3 }, start = { col = 24, row = 3 } } (Token "B")
                , Located { end = { col = 26, row = 3 }, start = { col = 25, row = 3 } } (Whitespace 1)
                , Located { end = { col = 27, row = 3 }, start = { col = 26, row = 3 } } (Token "C")
                , Located { end = { col = 28, row = 3 }, start = { col = 27, row = 3 } } (Whitespace 1)
                , Located { end = { col = 29, row = 3 }, start = { col = 28, row = 3 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 30, row = 3 }, start = { col = 29, row = 3 } } (Token "D")
                , Located { end = { col = 31, row = 3 }, start = { col = 30, row = 3 } } (Whitespace 1)
                , Located { end = { col = 32, row = 3 }, start = { col = 31, row = 3 } } (Token "E")
                , Located { end = { col = 33, row = 3 }, start = { col = 32, row = 3 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 5, row = 4 }, start = { col = 33, row = 3 } } (Newlines [] 4)
                , Located { end = { col = 6, row = 4 }, start = { col = 5, row = 4 } } (Sigil (Bracket Curly Close))
                , Located { end = { col = 1, row = 5 }, start = { col = 6, row = 4 } } (Newlines [] 0)
                ]
      , contextualized =
            Just
                [ Ok
                    (TypeAlias
                        { expr =
                            Record
                                (Dict.fromList
                                    [ ( "hi"
                                      , Record
                                            (Dict.fromList
                                                [ ( "a", UserDefinedType { args = [], name = "7", qualifiedness = PossiblyQualified Nothing } )
                                                , ( "b"
                                                  , UserDefinedType
                                                        { args =
                                                            [ UserDefinedType { args = [], name = "String", qualifiedness = PossiblyQualified Nothing }
                                                            ]
                                                        , name = "List"
                                                        , qualifiedness = PossiblyQualified Nothing
                                                        }
                                                  )
                                                ]
                                            )
                                      )
                                    , ( "ih"
                                      , UserDefinedType
                                            { args =
                                                [ UserDefinedType { args = [], name = "A", qualifiedness = PossiblyQualified Nothing }
                                                , UserDefinedType { args = [], name = "B", qualifiedness = PossiblyQualified Nothing }
                                                , UserDefinedType { args = [], name = "C", qualifiedness = PossiblyQualified Nothing }
                                                , UserDefinedType
                                                    { args =
                                                        [ UserDefinedType { args = [], name = "E", qualifiedness = PossiblyQualified Nothing }
                                                        ]
                                                    , name = "D"
                                                    , qualifiedness = PossiblyQualified Nothing
                                                    }
                                                ]
                                            , name = "CustomType"
                                            , qualifiedness = PossiblyQualified Nothing
                                            }
                                      )
                                    ]
                                )
                        , ty = TypeOrConstructor "Ty"
                        }
                    )
                ]
      }
    , { name = "type-alias-record-simple"
      , source = """type alias Ty = { hi: 6 }
"""
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token "type")
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token "alias")
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Whitespace 1)
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Token "Ty")
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Whitespace 1)
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Sigil Assign)
                , Located { end = { col = 17, row = 1 }, start = { col = 16, row = 1 } } (Whitespace 1)
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Sigil (Bracket Curly Open))
                , Located { end = { col = 19, row = 1 }, start = { col = 18, row = 1 } } (Whitespace 1)
                , Located { end = { col = 21, row = 1 }, start = { col = 19, row = 1 } } (Token "hi")
                , Located { end = { col = 22, row = 1 }, start = { col = 21, row = 1 } } (Sigil Colon)
                , Located { end = { col = 23, row = 1 }, start = { col = 22, row = 1 } } (Whitespace 1)
                , Located { end = { col = 24, row = 1 }, start = { col = 23, row = 1 } } (Token "6")
                , Located { end = { col = 25, row = 1 }, start = { col = 24, row = 1 } } (Whitespace 1)
                , Located { end = { col = 26, row = 1 }, start = { col = 25, row = 1 } } (Sigil (Bracket Curly Close))
                , Located { end = { col = 1, row = 2 }, start = { col = 26, row = 1 } } (Newlines [] 0)
                ]
      , contextualized =
            Just
                [ Ok
                    (TypeAlias
                        { expr =
                            Record
                                (Dict.fromList
                                    [ ( "hi", UserDefinedType { args = [], name = "6", qualifiedness = PossiblyQualified Nothing } )
                                    ]
                                )
                        , ty = TypeOrConstructor "Ty"
                        }
                    )
                ]
      }
    , { name = "type-alias-record-two-entries"
      , source = """type alias Ty = { hi: 6, buy: 8 }
"""
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token "type")
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token "alias")
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Whitespace 1)
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Token "Ty")
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Whitespace 1)
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Sigil Assign)
                , Located { end = { col = 17, row = 1 }, start = { col = 16, row = 1 } } (Whitespace 1)
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Sigil (Bracket Curly Open))
                , Located { end = { col = 19, row = 1 }, start = { col = 18, row = 1 } } (Whitespace 1)
                , Located { end = { col = 21, row = 1 }, start = { col = 19, row = 1 } } (Token "hi")
                , Located { end = { col = 22, row = 1 }, start = { col = 21, row = 1 } } (Sigil Colon)
                , Located { end = { col = 23, row = 1 }, start = { col = 22, row = 1 } } (Whitespace 1)
                , Located { end = { col = 24, row = 1 }, start = { col = 23, row = 1 } } (Token "6")
                , Located { end = { col = 25, row = 1 }, start = { col = 24, row = 1 } } (Sigil Comma)
                , Located { end = { col = 26, row = 1 }, start = { col = 25, row = 1 } } (Whitespace 1)
                , Located { end = { col = 29, row = 1 }, start = { col = 26, row = 1 } } (Token "buy")
                , Located { end = { col = 30, row = 1 }, start = { col = 29, row = 1 } } (Sigil Colon)
                , Located { end = { col = 31, row = 1 }, start = { col = 30, row = 1 } } (Whitespace 1)
                , Located { end = { col = 32, row = 1 }, start = { col = 31, row = 1 } } (Token "8")
                , Located { end = { col = 33, row = 1 }, start = { col = 32, row = 1 } } (Whitespace 1)
                , Located { end = { col = 34, row = 1 }, start = { col = 33, row = 1 } } (Sigil (Bracket Curly Close))
                , Located { end = { col = 1, row = 2 }, start = { col = 34, row = 1 } } (Newlines [] 0)
                ]
      , contextualized =
            Just
                [ Ok
                    (TypeAlias
                        { expr =
                            Record
                                (Dict.fromList
                                    [ ( "buy", UserDefinedType { args = [], name = "8", qualifiedness = PossiblyQualified Nothing } )
                                    , ( "hi", UserDefinedType { args = [], name = "6", qualifiedness = PossiblyQualified Nothing } )
                                    ]
                                )
                        , ty = TypeOrConstructor "Ty"
                        }
                    )
                ]
      }
    , { name = "type-alias-unit"
      , source = """type alias Hi = ()
"""
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token "type")
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token "alias")
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Whitespace 1)
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Token "Hi")
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Whitespace 1)
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Sigil Assign)
                , Located { end = { col = 17, row = 1 }, start = { col = 16, row = 1 } } (Whitespace 1)
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 19, row = 1 }, start = { col = 18, row = 1 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 1, row = 2 }, start = { col = 19, row = 1 } } (Newlines [] 0)
                ]
      , contextualized =
            Just
                [ Ok (TypeAlias { expr = Unit, ty = TypeOrConstructor "Hi" })
                ]
      }
    , { name = "type-alias-with-bracket"
      , source = """type alias Hi = (Int)
"""
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token "type")
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token "alias")
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Whitespace 1)
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Token "Hi")
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Whitespace 1)
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Sigil Assign)
                , Located { end = { col = 17, row = 1 }, start = { col = 16, row = 1 } } (Whitespace 1)
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 21, row = 1 }, start = { col = 18, row = 1 } } (Token "Int")
                , Located { end = { col = 22, row = 1 }, start = { col = 21, row = 1 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 1, row = 2 }, start = { col = 22, row = 1 } } (Newlines [] 0)
                ]
      , contextualized =
            Just
                [ Ok (TypeAlias { expr = UserDefinedType { args = [], name = "Int", qualifiedness = PossiblyQualified Nothing }, ty = TypeOrConstructor "Hi" })
                ]
      }
    , { name = "type-alias-with-pair"
      , source = """type alias Hi = (Int, List String)
type alias Hi = (Int)
"""
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token "type")
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token "alias")
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Whitespace 1)
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Token "Hi")
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Whitespace 1)
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Sigil Assign)
                , Located { end = { col = 17, row = 1 }, start = { col = 16, row = 1 } } (Whitespace 1)
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 21, row = 1 }, start = { col = 18, row = 1 } } (Token "Int")
                , Located { end = { col = 22, row = 1 }, start = { col = 21, row = 1 } } (Sigil Comma)
                , Located { end = { col = 23, row = 1 }, start = { col = 22, row = 1 } } (Whitespace 1)
                , Located { end = { col = 27, row = 1 }, start = { col = 23, row = 1 } } (Token "List")
                , Located { end = { col = 28, row = 1 }, start = { col = 27, row = 1 } } (Whitespace 1)
                , Located { end = { col = 34, row = 1 }, start = { col = 28, row = 1 } } (Token "String")
                , Located { end = { col = 35, row = 1 }, start = { col = 34, row = 1 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 1, row = 2 }, start = { col = 35, row = 1 } } (Newlines [] 0)
                , Located { end = { col = 5, row = 2 }, start = { col = 1, row = 2 } } (Token "type")
                , Located { end = { col = 6, row = 2 }, start = { col = 5, row = 2 } } (Whitespace 1)
                , Located { end = { col = 11, row = 2 }, start = { col = 6, row = 2 } } (Token "alias")
                , Located { end = { col = 12, row = 2 }, start = { col = 11, row = 2 } } (Whitespace 1)
                , Located { end = { col = 14, row = 2 }, start = { col = 12, row = 2 } } (Token "Hi")
                , Located { end = { col = 15, row = 2 }, start = { col = 14, row = 2 } } (Whitespace 1)
                , Located { end = { col = 16, row = 2 }, start = { col = 15, row = 2 } } (Sigil Assign)
                , Located { end = { col = 17, row = 2 }, start = { col = 16, row = 2 } } (Whitespace 1)
                , Located { end = { col = 18, row = 2 }, start = { col = 17, row = 2 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 21, row = 2 }, start = { col = 18, row = 2 } } (Token "Int")
                , Located { end = { col = 22, row = 2 }, start = { col = 21, row = 2 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 1, row = 3 }, start = { col = 22, row = 2 } } (Newlines [] 0)
                ]
      , contextualized =
            Just
                [ Ok
                    (TypeAlias
                        { expr =
                            Tuple (UserDefinedType { args = [], name = "Int", qualifiedness = PossiblyQualified Nothing })
                                (UserDefinedType
                                    { args =
                                        [ UserDefinedType { args = [], name = "String", qualifiedness = PossiblyQualified Nothing }
                                        ]
                                    , name = "List"
                                    , qualifiedness = PossiblyQualified Nothing
                                    }
                                )
                        , ty = TypeOrConstructor "Hi"
                        }
                    )
                , Ok (TypeAlias { expr = UserDefinedType { args = [], name = "Int", qualifiedness = PossiblyQualified Nothing }, ty = TypeOrConstructor "Hi" })
                ]
      }
    , { name = "type-alias-with-tripple"
      , source = """type alias Hi = (Int, Two, Three)
type alias Hi = ((), (), ())
"""
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token "type")
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token "alias")
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Whitespace 1)
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Token "Hi")
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Whitespace 1)
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Sigil Assign)
                , Located { end = { col = 17, row = 1 }, start = { col = 16, row = 1 } } (Whitespace 1)
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 21, row = 1 }, start = { col = 18, row = 1 } } (Token "Int")
                , Located { end = { col = 22, row = 1 }, start = { col = 21, row = 1 } } (Sigil Comma)
                , Located { end = { col = 23, row = 1 }, start = { col = 22, row = 1 } } (Whitespace 1)
                , Located { end = { col = 26, row = 1 }, start = { col = 23, row = 1 } } (Token "Two")
                , Located { end = { col = 27, row = 1 }, start = { col = 26, row = 1 } } (Sigil Comma)
                , Located { end = { col = 28, row = 1 }, start = { col = 27, row = 1 } } (Whitespace 1)
                , Located { end = { col = 33, row = 1 }, start = { col = 28, row = 1 } } (Token "Three")
                , Located { end = { col = 34, row = 1 }, start = { col = 33, row = 1 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 1, row = 2 }, start = { col = 34, row = 1 } } (Newlines [] 0)
                , Located { end = { col = 5, row = 2 }, start = { col = 1, row = 2 } } (Token "type")
                , Located { end = { col = 6, row = 2 }, start = { col = 5, row = 2 } } (Whitespace 1)
                , Located { end = { col = 11, row = 2 }, start = { col = 6, row = 2 } } (Token "alias")
                , Located { end = { col = 12, row = 2 }, start = { col = 11, row = 2 } } (Whitespace 1)
                , Located { end = { col = 14, row = 2 }, start = { col = 12, row = 2 } } (Token "Hi")
                , Located { end = { col = 15, row = 2 }, start = { col = 14, row = 2 } } (Whitespace 1)
                , Located { end = { col = 16, row = 2 }, start = { col = 15, row = 2 } } (Sigil Assign)
                , Located { end = { col = 17, row = 2 }, start = { col = 16, row = 2 } } (Whitespace 1)
                , Located { end = { col = 18, row = 2 }, start = { col = 17, row = 2 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 19, row = 2 }, start = { col = 18, row = 2 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 20, row = 2 }, start = { col = 19, row = 2 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 21, row = 2 }, start = { col = 20, row = 2 } } (Sigil Comma)
                , Located { end = { col = 22, row = 2 }, start = { col = 21, row = 2 } } (Whitespace 1)
                , Located { end = { col = 23, row = 2 }, start = { col = 22, row = 2 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 24, row = 2 }, start = { col = 23, row = 2 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 25, row = 2 }, start = { col = 24, row = 2 } } (Sigil Comma)
                , Located { end = { col = 26, row = 2 }, start = { col = 25, row = 2 } } (Whitespace 1)
                , Located { end = { col = 27, row = 2 }, start = { col = 26, row = 2 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 28, row = 2 }, start = { col = 27, row = 2 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 29, row = 2 }, start = { col = 28, row = 2 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 1, row = 3 }, start = { col = 29, row = 2 } } (Newlines [] 0)
                ]
      , contextualized =
            Just
                [ Ok (TypeAlias { expr = Tuple3 (UserDefinedType { args = [], name = "Int", qualifiedness = PossiblyQualified Nothing }) (UserDefinedType { args = [], name = "Two", qualifiedness = PossiblyQualified Nothing }) (UserDefinedType { args = [], name = "Three", qualifiedness = PossiblyQualified Nothing }), ty = TypeOrConstructor "Hi" })
                , Ok (TypeAlias { expr = Tuple3 Unit Unit Unit, ty = TypeOrConstructor "Hi" })
                ]
      }
    , { name = "type-alias-with-tripple-in-record"
      , source = """type alias Hi =
    { a: (Int, Int, Int)
    , b: ({ good_bye: () })
    }
"""
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token "type")
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token "alias")
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Whitespace 1)
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Token "Hi")
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Whitespace 1)
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Sigil Assign)
                , Located { end = { col = 5, row = 2 }, start = { col = 16, row = 1 } } (Newlines [] 4)
                , Located { end = { col = 6, row = 2 }, start = { col = 5, row = 2 } } (Sigil (Bracket Curly Open))
                , Located { end = { col = 7, row = 2 }, start = { col = 6, row = 2 } } (Whitespace 1)
                , Located { end = { col = 8, row = 2 }, start = { col = 7, row = 2 } } (Token "a")
                , Located { end = { col = 9, row = 2 }, start = { col = 8, row = 2 } } (Sigil Colon)
                , Located { end = { col = 10, row = 2 }, start = { col = 9, row = 2 } } (Whitespace 1)
                , Located { end = { col = 11, row = 2 }, start = { col = 10, row = 2 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 14, row = 2 }, start = { col = 11, row = 2 } } (Token "Int")
                , Located { end = { col = 15, row = 2 }, start = { col = 14, row = 2 } } (Sigil Comma)
                , Located { end = { col = 16, row = 2 }, start = { col = 15, row = 2 } } (Whitespace 1)
                , Located { end = { col = 19, row = 2 }, start = { col = 16, row = 2 } } (Token "Int")
                , Located { end = { col = 20, row = 2 }, start = { col = 19, row = 2 } } (Sigil Comma)
                , Located { end = { col = 21, row = 2 }, start = { col = 20, row = 2 } } (Whitespace 1)
                , Located { end = { col = 24, row = 2 }, start = { col = 21, row = 2 } } (Token "Int")
                , Located { end = { col = 25, row = 2 }, start = { col = 24, row = 2 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 5, row = 3 }, start = { col = 25, row = 2 } } (Newlines [] 4)
                , Located { end = { col = 6, row = 3 }, start = { col = 5, row = 3 } } (Sigil Comma)
                , Located { end = { col = 7, row = 3 }, start = { col = 6, row = 3 } } (Whitespace 1)
                , Located { end = { col = 8, row = 3 }, start = { col = 7, row = 3 } } (Token "b")
                , Located { end = { col = 9, row = 3 }, start = { col = 8, row = 3 } } (Sigil Colon)
                , Located { end = { col = 10, row = 3 }, start = { col = 9, row = 3 } } (Whitespace 1)
                , Located { end = { col = 11, row = 3 }, start = { col = 10, row = 3 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 12, row = 3 }, start = { col = 11, row = 3 } } (Sigil (Bracket Curly Open))
                , Located { end = { col = 13, row = 3 }, start = { col = 12, row = 3 } } (Whitespace 1)
                , Located { end = { col = 21, row = 3 }, start = { col = 13, row = 3 } } (Token "good_bye")
                , Located { end = { col = 22, row = 3 }, start = { col = 21, row = 3 } } (Sigil Colon)
                , Located { end = { col = 23, row = 3 }, start = { col = 22, row = 3 } } (Whitespace 1)
                , Located { end = { col = 24, row = 3 }, start = { col = 23, row = 3 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 25, row = 3 }, start = { col = 24, row = 3 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 26, row = 3 }, start = { col = 25, row = 3 } } (Whitespace 1)
                , Located { end = { col = 27, row = 3 }, start = { col = 26, row = 3 } } (Sigil (Bracket Curly Close))
                , Located { end = { col = 28, row = 3 }, start = { col = 27, row = 3 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 5, row = 4 }, start = { col = 28, row = 3 } } (Newlines [] 4)
                , Located { end = { col = 6, row = 4 }, start = { col = 5, row = 4 } } (Sigil (Bracket Curly Close))
                , Located { end = { col = 1, row = 5 }, start = { col = 6, row = 4 } } (Newlines [] 0)
                ]
      , contextualized =
            Just
                [ Ok
                    (TypeAlias
                        { expr =
                            Record
                                (Dict.fromList
                                    [ ( "a", Tuple3 (UserDefinedType { args = [], name = "Int", qualifiedness = PossiblyQualified Nothing }) (UserDefinedType { args = [], name = "Int", qualifiedness = PossiblyQualified Nothing }) (UserDefinedType { args = [], name = "Int", qualifiedness = PossiblyQualified Nothing }) )
                                    , ( "b"
                                      , Record
                                            (Dict.fromList
                                                [ ( "good_bye", Unit )
                                                ]
                                            )
                                      )
                                    ]
                                )
                        , ty = TypeOrConstructor "Hi"
                        }
                    )
                ]
      }
    ]


shouldNotParseTestCases :
    List
        { contextualized : Maybe (List (Result ( State, Error ) never))
        , lexed : Result Never (List (Located LexItem))
        , name : String
        , source : String
        }
shouldNotParseTestCases =
    [ { name = "type-alias-invalid-multiple-brackets"
      , source = """type alias Hi = (Int) ()
"""
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token "type")
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token "alias")
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Whitespace 1)
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Token "Hi")
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Whitespace 1)
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Sigil Assign)
                , Located { end = { col = 17, row = 1 }, start = { col = 16, row = 1 } } (Whitespace 1)
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 21, row = 1 }, start = { col = 18, row = 1 } } (Token "Int")
                , Located { end = { col = 22, row = 1 }, start = { col = 21, row = 1 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 23, row = 1 }, start = { col = 22, row = 1 } } (Whitespace 1)
                , Located { end = { col = 24, row = 1 }, start = { col = 23, row = 1 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 25, row = 1 }, start = { col = 24, row = 1 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 1, row = 2 }, start = { col = 25, row = 1 } } (Newlines [] 0)
                ]
      , contextualized =
            Just
                [ Err
                    ( State_BlockTypeAlias
                        (BlockTypeAlias_Completish (TypeOrConstructor "Hi")
                            { nestingStack =
                                Stack
                                    [ NestingType_Bracket ( Stack [], Nothing )
                                    ]
                            , root = Just (TypeExpression_Bracketed (TypeExpression_NamedType { args = Stack [], name = "Int" }))
                            }
                        )
                    , Error_TypeDoesNotTakeArgs (TypeExpression_Bracketed (TypeExpression_NamedType { args = Stack [], name = "Int" })) TypeExpression_Unit
                    )
                ]
      }
    , { name = "type-alias-invalid-multiple-brackets-2"
      , source = """type alias Hi = () ()
"""
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token "type")
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token "alias")
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Whitespace 1)
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Token "Hi")
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Whitespace 1)
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Sigil Assign)
                , Located { end = { col = 17, row = 1 }, start = { col = 16, row = 1 } } (Whitespace 1)
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 19, row = 1 }, start = { col = 18, row = 1 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 20, row = 1 }, start = { col = 19, row = 1 } } (Whitespace 1)
                , Located { end = { col = 21, row = 1 }, start = { col = 20, row = 1 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 22, row = 1 }, start = { col = 21, row = 1 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 1, row = 2 }, start = { col = 22, row = 1 } } (Newlines [] 0)
                ]
      , contextualized =
            Just
                [ Err
                    ( State_BlockTypeAlias
                        (BlockTypeAlias_Completish (TypeOrConstructor "Hi")
                            { nestingStack =
                                Stack
                                    [ NestingType_Bracket ( Stack [], Nothing )
                                    ]
                            , root = Just TypeExpression_Unit
                            }
                        )
                    , Error_TypeDoesNotTakeArgs TypeExpression_Unit TypeExpression_Unit
                    )
                ]
      }
    , { name = "type-alias-invalid-multiple-brackets-3"
      , source = """type alias Hi = () (Int)
"""
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token "type")
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token "alias")
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Whitespace 1)
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Token "Hi")
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Whitespace 1)
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Sigil Assign)
                , Located { end = { col = 17, row = 1 }, start = { col = 16, row = 1 } } (Whitespace 1)
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 19, row = 1 }, start = { col = 18, row = 1 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 20, row = 1 }, start = { col = 19, row = 1 } } (Whitespace 1)
                , Located { end = { col = 21, row = 1 }, start = { col = 20, row = 1 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 24, row = 1 }, start = { col = 21, row = 1 } } (Token "Int")
                , Located { end = { col = 25, row = 1 }, start = { col = 24, row = 1 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 1, row = 2 }, start = { col = 25, row = 1 } } (Newlines [] 0)
                ]
      , contextualized =
            Just
                [ Err
                    ( State_BlockTypeAlias
                        (BlockTypeAlias_Completish (TypeOrConstructor "Hi")
                            { nestingStack =
                                Stack
                                    [ NestingType_Bracket ( Stack [], Just (TypeExpression_NamedType { args = Stack [], name = "Int" }) )
                                    ]
                            , root = Just TypeExpression_Unit
                            }
                        )
                    , Error_TypeDoesNotTakeArgs TypeExpression_Unit (TypeExpression_Bracketed (TypeExpression_NamedType { args = Stack [], name = "Int" }))
                    )
                ]
      }
    , { name = "type-alias-multiline-missing-indentation"
      , source = """type alias Model =
List Int
"""
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token "type")
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token "alias")
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Whitespace 1)
                , Located { end = { col = 17, row = 1 }, start = { col = 12, row = 1 } } (Token "Model")
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Whitespace 1)
                , Located { end = { col = 19, row = 1 }, start = { col = 18, row = 1 } } (Sigil Assign)
                , Located { end = { col = 1, row = 2 }, start = { col = 19, row = 1 } } (Newlines [] 0)
                , Located { end = { col = 5, row = 2 }, start = { col = 1, row = 2 } } (Token "List")
                , Located { end = { col = 6, row = 2 }, start = { col = 5, row = 2 } } (Whitespace 1)
                , Located { end = { col = 9, row = 2 }, start = { col = 6, row = 2 } } (Token "Int")
                , Located { end = { col = 1, row = 3 }, start = { col = 9, row = 2 } } (Newlines [] 0)
                ]
      , contextualized =
            Just
                [ Err ( State_BlockTypeAlias (BlockTypeAlias_NamedAssigns (TypeOrConstructor "Model")), Error_PartwayThroughTypeAlias )
                , Err ( State_BlockStart, Error_BlockStartsWithTypeOrConstructor (TypeOrConstructor "List") )
                ]
      }
    , { name = "type-alias-partial"
      , source = """type alias
"""
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token "type")
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token "alias")
                , Located { end = { col = 1, row = 2 }, start = { col = 11, row = 1 } } (Newlines [] 0)
                ]
      , contextualized =
            Just
                [ Err ( State_BlockTypeAlias BlockTypeAlias_Keywords, Error_PartwayThroughTypeAlias )
                ]
      }
    , { name = "type-alias-partial-2"
      , source = """type alias Hi
"""
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token "type")
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token "alias")
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Whitespace 1)
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Token "Hi")
                , Located { end = { col = 1, row = 2 }, start = { col = 14, row = 1 } } (Newlines [] 0)
                ]
      , contextualized =
            Just
                [ Err ( State_BlockTypeAlias (BlockTypeAlias_Named (TypeOrConstructor "Hi")), Error_PartwayThroughTypeAlias )
                ]
      }
    , { name = "type-alias-partial-3"
      , source = """type alias Hi =
"""
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token "type")
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token "alias")
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Whitespace 1)
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Token "Hi")
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Whitespace 1)
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Sigil Assign)
                , Located { end = { col = 1, row = 2 }, start = { col = 16, row = 1 } } (Newlines [] 0)
                ]
      , contextualized =
            Just
                [ Err ( State_BlockTypeAlias (BlockTypeAlias_NamedAssigns (TypeOrConstructor "Hi")), Error_PartwayThroughTypeAlias )
                ]
      }
    , { name = "type-alias-partial-with-bracket"
      , source = """type alias Hi = (
"""
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token "type")
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token "alias")
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Whitespace 1)
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Token "Hi")
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Whitespace 1)
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Sigil Assign)
                , Located { end = { col = 17, row = 1 }, start = { col = 16, row = 1 } } (Whitespace 1)
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 1, row = 2 }, start = { col = 18, row = 1 } } (Newlines [] 0)
                ]
      , contextualized =
            Just
                [ Err
                    ( State_BlockTypeAlias
                        (BlockTypeAlias_Completish (TypeOrConstructor "Hi")
                            { nestingStack =
                                Stack
                                    [ NestingType_Bracket ( Stack [], Nothing )
                                    ]
                            , root = Nothing
                            }
                        )
                    , Error_PartwayThroughTypeAlias
                    )
                ]
      }
    , { name = "type-alias-partial-with-bracket-2"
      , source = """type alias Hi = (
        Int
"""
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token "type")
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token "alias")
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Whitespace 1)
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Token "Hi")
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Whitespace 1)
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Sigil Assign)
                , Located { end = { col = 17, row = 1 }, start = { col = 16, row = 1 } } (Whitespace 1)
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 9, row = 2 }, start = { col = 18, row = 1 } } (Newlines [] 8)
                , Located { end = { col = 12, row = 2 }, start = { col = 9, row = 2 } } (Token "Int")
                , Located { end = { col = 1, row = 3 }, start = { col = 12, row = 2 } } (Newlines [] 0)
                ]
      , contextualized =
            Just
                [ Err
                    ( State_BlockTypeAlias
                        (BlockTypeAlias_Completish (TypeOrConstructor "Hi")
                            { nestingStack =
                                Stack
                                    [ NestingType_Bracket ( Stack [], Just (TypeExpression_NamedType { args = Stack [], name = "Int" }) )
                                    ]
                            , root = Nothing
                            }
                        )
                    , Error_PartwayThroughTypeAlias
                    )
                ]
      }
    , { name = "type-alias-record-half-empty"
      , source = """type alias Ty = {
"""
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token "type")
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token "alias")
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Whitespace 1)
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Token "Ty")
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Whitespace 1)
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Sigil Assign)
                , Located { end = { col = 17, row = 1 }, start = { col = 16, row = 1 } } (Whitespace 1)
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Sigil (Bracket Curly Open))
                , Located { end = { col = 1, row = 2 }, start = { col = 18, row = 1 } } (Newlines [] 0)
                ]
      , contextualized =
            Just
                [ Err
                    ( State_BlockTypeAlias
                        (BlockTypeAlias_Completish (TypeOrConstructor "Ty")
                            { nestingStack =
                                Stack
                                    [ NestingType_PartialRecord { firstEntries = Stack [], lastEntry = LastEntryOfRecord_Empty }
                                    ]
                            , root = Nothing
                            }
                        )
                    , Error_PartwayThroughTypeAlias
                    )
                ]
      }
    , { name = "type-alias-record-missing-colon"
      , source = """type alias Ty = { hi j7 }
"""
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token "type")
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token "alias")
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Whitespace 1)
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Token "Ty")
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Whitespace 1)
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Sigil Assign)
                , Located { end = { col = 17, row = 1 }, start = { col = 16, row = 1 } } (Whitespace 1)
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Sigil (Bracket Curly Open))
                , Located { end = { col = 19, row = 1 }, start = { col = 18, row = 1 } } (Whitespace 1)
                , Located { end = { col = 21, row = 1 }, start = { col = 19, row = 1 } } (Token "hi")
                , Located { end = { col = 22, row = 1 }, start = { col = 21, row = 1 } } (Whitespace 1)
                , Located { end = { col = 24, row = 1 }, start = { col = 22, row = 1 } } (Token "j7")
                , Located { end = { col = 25, row = 1 }, start = { col = 24, row = 1 } } (Whitespace 1)
                , Located { end = { col = 26, row = 1 }, start = { col = 25, row = 1 } } (Sigil (Bracket Curly Close))
                , Located { end = { col = 1, row = 2 }, start = { col = 26, row = 1 } } (Newlines [] 0)
                ]
      , contextualized =
            Just
                [ Err
                    ( State_BlockTypeAlias
                        (BlockTypeAlias_Completish (TypeOrConstructor "Ty")
                            { nestingStack =
                                Stack
                                    [ NestingType_PartialRecord { firstEntries = Stack [], lastEntry = LastEntryOfRecord_Key "hi" }
                                    ]
                            , root = Nothing
                            }
                        )
                    , Error_ExpectedColonWhilstParsingRecord
                    )
                ]
      }
    , { name = "type-alias-with-quadruple"
      , source = """type alias Hi = (Int, A, B, C, D)
type alias Hi = (A Int, C D E F, H I (J K), L M () O P)
"""
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token "type")
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token "alias")
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Whitespace 1)
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Token "Hi")
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Whitespace 1)
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Sigil Assign)
                , Located { end = { col = 17, row = 1 }, start = { col = 16, row = 1 } } (Whitespace 1)
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 21, row = 1 }, start = { col = 18, row = 1 } } (Token "Int")
                , Located { end = { col = 22, row = 1 }, start = { col = 21, row = 1 } } (Sigil Comma)
                , Located { end = { col = 23, row = 1 }, start = { col = 22, row = 1 } } (Whitespace 1)
                , Located { end = { col = 24, row = 1 }, start = { col = 23, row = 1 } } (Token "A")
                , Located { end = { col = 25, row = 1 }, start = { col = 24, row = 1 } } (Sigil Comma)
                , Located { end = { col = 26, row = 1 }, start = { col = 25, row = 1 } } (Whitespace 1)
                , Located { end = { col = 27, row = 1 }, start = { col = 26, row = 1 } } (Token "B")
                , Located { end = { col = 28, row = 1 }, start = { col = 27, row = 1 } } (Sigil Comma)
                , Located { end = { col = 29, row = 1 }, start = { col = 28, row = 1 } } (Whitespace 1)
                , Located { end = { col = 30, row = 1 }, start = { col = 29, row = 1 } } (Token "C")
                , Located { end = { col = 31, row = 1 }, start = { col = 30, row = 1 } } (Sigil Comma)
                , Located { end = { col = 32, row = 1 }, start = { col = 31, row = 1 } } (Whitespace 1)
                , Located { end = { col = 33, row = 1 }, start = { col = 32, row = 1 } } (Token "D")
                , Located { end = { col = 34, row = 1 }, start = { col = 33, row = 1 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 1, row = 2 }, start = { col = 34, row = 1 } } (Newlines [] 0)
                , Located { end = { col = 5, row = 2 }, start = { col = 1, row = 2 } } (Token "type")
                , Located { end = { col = 6, row = 2 }, start = { col = 5, row = 2 } } (Whitespace 1)
                , Located { end = { col = 11, row = 2 }, start = { col = 6, row = 2 } } (Token "alias")
                , Located { end = { col = 12, row = 2 }, start = { col = 11, row = 2 } } (Whitespace 1)
                , Located { end = { col = 14, row = 2 }, start = { col = 12, row = 2 } } (Token "Hi")
                , Located { end = { col = 15, row = 2 }, start = { col = 14, row = 2 } } (Whitespace 1)
                , Located { end = { col = 16, row = 2 }, start = { col = 15, row = 2 } } (Sigil Assign)
                , Located { end = { col = 17, row = 2 }, start = { col = 16, row = 2 } } (Whitespace 1)
                , Located { end = { col = 18, row = 2 }, start = { col = 17, row = 2 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 19, row = 2 }, start = { col = 18, row = 2 } } (Token "A")
                , Located { end = { col = 20, row = 2 }, start = { col = 19, row = 2 } } (Whitespace 1)
                , Located { end = { col = 23, row = 2 }, start = { col = 20, row = 2 } } (Token "Int")
                , Located { end = { col = 24, row = 2 }, start = { col = 23, row = 2 } } (Sigil Comma)
                , Located { end = { col = 25, row = 2 }, start = { col = 24, row = 2 } } (Whitespace 1)
                , Located { end = { col = 26, row = 2 }, start = { col = 25, row = 2 } } (Token "C")
                , Located { end = { col = 27, row = 2 }, start = { col = 26, row = 2 } } (Whitespace 1)
                , Located { end = { col = 28, row = 2 }, start = { col = 27, row = 2 } } (Token "D")
                , Located { end = { col = 29, row = 2 }, start = { col = 28, row = 2 } } (Whitespace 1)
                , Located { end = { col = 30, row = 2 }, start = { col = 29, row = 2 } } (Token "E")
                , Located { end = { col = 31, row = 2 }, start = { col = 30, row = 2 } } (Whitespace 1)
                , Located { end = { col = 32, row = 2 }, start = { col = 31, row = 2 } } (Token "F")
                , Located { end = { col = 33, row = 2 }, start = { col = 32, row = 2 } } (Sigil Comma)
                , Located { end = { col = 34, row = 2 }, start = { col = 33, row = 2 } } (Whitespace 1)
                , Located { end = { col = 35, row = 2 }, start = { col = 34, row = 2 } } (Token "H")
                , Located { end = { col = 36, row = 2 }, start = { col = 35, row = 2 } } (Whitespace 1)
                , Located { end = { col = 37, row = 2 }, start = { col = 36, row = 2 } } (Token "I")
                , Located { end = { col = 38, row = 2 }, start = { col = 37, row = 2 } } (Whitespace 1)
                , Located { end = { col = 39, row = 2 }, start = { col = 38, row = 2 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 40, row = 2 }, start = { col = 39, row = 2 } } (Token "J")
                , Located { end = { col = 41, row = 2 }, start = { col = 40, row = 2 } } (Whitespace 1)
                , Located { end = { col = 42, row = 2 }, start = { col = 41, row = 2 } } (Token "K")
                , Located { end = { col = 43, row = 2 }, start = { col = 42, row = 2 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 44, row = 2 }, start = { col = 43, row = 2 } } (Sigil Comma)
                , Located { end = { col = 45, row = 2 }, start = { col = 44, row = 2 } } (Whitespace 1)
                , Located { end = { col = 46, row = 2 }, start = { col = 45, row = 2 } } (Token "L")
                , Located { end = { col = 47, row = 2 }, start = { col = 46, row = 2 } } (Whitespace 1)
                , Located { end = { col = 48, row = 2 }, start = { col = 47, row = 2 } } (Token "M")
                , Located { end = { col = 49, row = 2 }, start = { col = 48, row = 2 } } (Whitespace 1)
                , Located { end = { col = 50, row = 2 }, start = { col = 49, row = 2 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 51, row = 2 }, start = { col = 50, row = 2 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 52, row = 2 }, start = { col = 51, row = 2 } } (Whitespace 1)
                , Located { end = { col = 53, row = 2 }, start = { col = 52, row = 2 } } (Token "O")
                , Located { end = { col = 54, row = 2 }, start = { col = 53, row = 2 } } (Whitespace 1)
                , Located { end = { col = 55, row = 2 }, start = { col = 54, row = 2 } } (Token "P")
                , Located { end = { col = 56, row = 2 }, start = { col = 55, row = 2 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 1, row = 3 }, start = { col = 56, row = 2 } } (Newlines [] 0)
                ]
      , contextualized =
            Just
                [ Err
                    ( State_BlockTypeAlias
                        (BlockTypeAlias_Completish (TypeOrConstructor "Hi")
                            { nestingStack = Stack []
                            , root =
                                Just
                                    (TypeExpression_Tuple (TypeExpression_NamedType { args = Stack [], name = "Int" })
                                        (TypeExpression_NamedType { args = Stack [], name = "A" })
                                        [ TypeExpression_NamedType { args = Stack [], name = "B" }
                                        , TypeExpression_NamedType { args = Stack [], name = "C" }
                                        , TypeExpression_NamedType { args = Stack [], name = "D" }
                                        ]
                                    )
                            }
                        )
                    , Error_TooManyTupleArgs (UserDefinedType { args = [], name = "Int", qualifiedness = PossiblyQualified Nothing })
                        (UserDefinedType { args = [], name = "A", qualifiedness = PossiblyQualified Nothing })
                        (UserDefinedType { args = [], name = "B", qualifiedness = PossiblyQualified Nothing })
                        (UserDefinedType { args = [], name = "C", qualifiedness = PossiblyQualified Nothing })
                        [ UserDefinedType { args = [], name = "D", qualifiedness = PossiblyQualified Nothing }
                        ]
                    )
                , Err
                    ( State_BlockTypeAlias
                        (BlockTypeAlias_Completish (TypeOrConstructor "Hi")
                            { nestingStack = Stack []
                            , root =
                                Just
                                    (TypeExpression_Tuple
                                        (TypeExpression_NamedType
                                            { args =
                                                Stack
                                                    [ TypeExpression_NamedType { args = Stack [], name = "Int" }
                                                    ]
                                            , name = "A"
                                            }
                                        )
                                        (TypeExpression_NamedType
                                            { args =
                                                Stack
                                                    [ TypeExpression_NamedType { args = Stack [], name = "F" }
                                                    , TypeExpression_NamedType { args = Stack [], name = "E" }
                                                    , TypeExpression_NamedType { args = Stack [], name = "D" }
                                                    ]
                                            , name = "C"
                                            }
                                        )
                                        [ TypeExpression_NamedType
                                            { args =
                                                Stack
                                                    [ TypeExpression_Bracketed
                                                        (TypeExpression_NamedType
                                                            { args =
                                                                Stack
                                                                    [ TypeExpression_NamedType { args = Stack [], name = "K" }
                                                                    ]
                                                            , name = "J"
                                                            }
                                                        )
                                                    , TypeExpression_NamedType { args = Stack [], name = "I" }
                                                    ]
                                            , name = "H"
                                            }
                                        , TypeExpression_NamedType
                                            { args =
                                                Stack
                                                    [ TypeExpression_NamedType { args = Stack [], name = "P" }
                                                    , TypeExpression_NamedType { args = Stack [], name = "O" }
                                                    , TypeExpression_Unit
                                                    , TypeExpression_NamedType { args = Stack [], name = "M" }
                                                    ]
                                            , name = "L"
                                            }
                                        ]
                                    )
                            }
                        )
                    , Error_TooManyTupleArgs
                        (UserDefinedType
                            { args =
                                [ UserDefinedType { args = [], name = "Int", qualifiedness = PossiblyQualified Nothing }
                                ]
                            , name = "A"
                            , qualifiedness = PossiblyQualified Nothing
                            }
                        )
                        (UserDefinedType
                            { args =
                                [ UserDefinedType { args = [], name = "D", qualifiedness = PossiblyQualified Nothing }
                                , UserDefinedType { args = [], name = "E", qualifiedness = PossiblyQualified Nothing }
                                , UserDefinedType { args = [], name = "F", qualifiedness = PossiblyQualified Nothing }
                                ]
                            , name = "C"
                            , qualifiedness = PossiblyQualified Nothing
                            }
                        )
                        (UserDefinedType
                            { args =
                                [ UserDefinedType { args = [], name = "I", qualifiedness = PossiblyQualified Nothing }
                                , UserDefinedType
                                    { args =
                                        [ UserDefinedType { args = [], name = "K", qualifiedness = PossiblyQualified Nothing }
                                        ]
                                    , name = "J"
                                    , qualifiedness = PossiblyQualified Nothing
                                    }
                                ]
                            , name = "H"
                            , qualifiedness = PossiblyQualified Nothing
                            }
                        )
                        (UserDefinedType
                            { args =
                                [ UserDefinedType { args = [], name = "M", qualifiedness = PossiblyQualified Nothing }
                                , Unit
                                , UserDefinedType { args = [], name = "O", qualifiedness = PossiblyQualified Nothing }
                                , UserDefinedType { args = [], name = "P", qualifiedness = PossiblyQualified Nothing }
                                ]
                            , name = "L"
                            , qualifiedness = PossiblyQualified Nothing
                            }
                        )
                        []
                    )
                ]
      }
    , { name = "type-alias-with-tuple-double-comma"
      , source = """type alias Hi = (Int, , A)
"""
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token "type")
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token "alias")
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Whitespace 1)
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Token "Hi")
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Whitespace 1)
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Sigil Assign)
                , Located { end = { col = 17, row = 1 }, start = { col = 16, row = 1 } } (Whitespace 1)
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 21, row = 1 }, start = { col = 18, row = 1 } } (Token "Int")
                , Located { end = { col = 22, row = 1 }, start = { col = 21, row = 1 } } (Sigil Comma)
                , Located { end = { col = 23, row = 1 }, start = { col = 22, row = 1 } } (Whitespace 1)
                , Located { end = { col = 24, row = 1 }, start = { col = 23, row = 1 } } (Sigil Comma)
                , Located { end = { col = 25, row = 1 }, start = { col = 24, row = 1 } } (Whitespace 1)
                , Located { end = { col = 26, row = 1 }, start = { col = 25, row = 1 } } (Token "A")
                , Located { end = { col = 27, row = 1 }, start = { col = 26, row = 1 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 1, row = 2 }, start = { col = 27, row = 1 } } (Newlines [] 0)
                ]
      , contextualized =
            Just
                [ Err
                    ( State_BlockTypeAlias
                        (BlockTypeAlias_Completish (TypeOrConstructor "Hi")
                            { nestingStack =
                                Stack
                                    [ NestingType_Bracket
                                        ( Stack
                                            [ TypeExpression_NamedType { args = Stack [], name = "Int" }
                                            ]
                                        , Nothing
                                        )
                                    ]
                            , root = Nothing
                            }
                        )
                    , Error_InvalidToken (Sigil Comma) Expecting_Unknown
                    )
                ]
      }
    , { name = "type-alias-with-tuple-not-closed"
      , source = """type alias Hi = (Int,



"""
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token "type")
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token "alias")
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Whitespace 1)
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Token "Hi")
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Whitespace 1)
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Sigil Assign)
                , Located { end = { col = 17, row = 1 }, start = { col = 16, row = 1 } } (Whitespace 1)
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 21, row = 1 }, start = { col = 18, row = 1 } } (Token "Int")
                , Located { end = { col = 22, row = 1 }, start = { col = 21, row = 1 } } (Sigil Comma)
                , Located { end = { col = 1, row = 5 }, start = { col = 22, row = 1 } }
                    (Newlines
                        [ 0
                        , 0
                        , 0
                        ]
                        0
                    )
                ]
      , contextualized =
            Just
                [ Err
                    ( State_BlockTypeAlias
                        (BlockTypeAlias_Completish (TypeOrConstructor "Hi")
                            { nestingStack =
                                Stack
                                    [ NestingType_Bracket
                                        ( Stack
                                            [ TypeExpression_NamedType { args = Stack [], name = "Int" }
                                            ]
                                        , Nothing
                                        )
                                    ]
                            , root = Nothing
                            }
                        )
                    , Error_PartwayThroughTypeAlias
                    )
                ]
      }
    , { name = "type-partial"
      , source = """type
"""
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token "type")
                , Located { end = { col = 1, row = 2 }, start = { col = 5, row = 1 } } (Newlines [] 0)
                ]
      , contextualized =
            Just
                [ Err ( State_BlockFirstItem BlockFirstItem_Type, Error_PartwayThroughTypeAlias )
                ]
      }
    ]
