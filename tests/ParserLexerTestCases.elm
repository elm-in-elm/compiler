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
        { contextualized : Maybe (List (Result ( State, Error ) Block))
        , lexed : Result error (List (Located LexItem))
        , name : String
        , source : String
        }
testCases =
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
    , { name = "type-alias-empty-record"
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
                [ Err ( State_BlockTypeAlias (BlockTypeAlias_NamedAssigns (TypeOrConstructor "Ty")), Error_InvalidToken (Sigil (Bracket Curly Open)) (Expecting_Sigil Assign) )
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
    , { name = "type-alias-invalid-multiple-brackets"
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
                            { bracketStack =
                                Stack
                                    [ Nothing
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
                            { bracketStack =
                                Stack
                                    [ Nothing
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
                            { bracketStack =
                                Stack
                                    [ Just (TypeExpression_NamedType { args = Stack [], name = "Int" })
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
                            { bracketStack =
                                Stack
                                    [ Nothing
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
                            { bracketStack =
                                Stack
                                    [ Just (TypeExpression_NamedType { args = Stack [], name = "Int" })
                                    ]
                            , root = Nothing
                            }
                        )
                    , Error_PartwayThroughTypeAlias
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
