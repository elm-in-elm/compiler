module ParserLexerTestCases exposing (shouldNotParseTestCases, shouldParseTestCases)

import Dict
import Elm.AST.Frontend as Frontend
import Elm.Data.Located exposing (Located(..))
import Elm.Data.Operator exposing (Operator(..))
import Elm.Data.Qualifiedness exposing (PossiblyQualified(..))
import Elm.Data.Type.Concrete exposing (ConcreteType(..))
import Stage.Parse.Contextualize as Contextualize exposing (..)
import Stage.Parse.Lexer exposing (..)
import Stage.Parse.Token exposing (Keyword(..), LowerCase(..), Token(..), UpperCase(..))



-- AUTO GENERATED TEST CASES
--
-- Do not edit below this line or your changes will be overwritten by
-- tests/parser-tests/update.js


shouldParseTestCases :
    List
        { contextualized : Maybe (List Contextualize.RunResult)
        , pretty : String
        , lexed : Result Never (List (Located LexItem))
        , name : String
        , source : String
        }
shouldParseTestCases =
    [ { name = "expression-int"
      , source = """a = 5

b = 78
"""
      , pretty = """
        ( ( Ok
          , ( ValueDeclaration
            , ( name, a )
            , ( args, () )
            , ( valueExpr__
              , ( Int, 5 )
              )
            )
          )
        , ( Ok
          , ( ValueDeclaration
            , ( name, b )
            , ( args, () )
            , ( valueExpr__
              , ( Int, 78 )
              )
            )
          )
        )
"""
      , contextualized =
            Just
                [ Ok (ValueDeclaration { args = [], name = Located { end = { col = 2, row = 1 }, start = { col = 1, row = 1 } } (LowerCase "a"), valueExpr__ = Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Frontend.Int 5) })
                , Ok (ValueDeclaration { args = [], name = Located { end = { col = 2, row = 3 }, start = { col = 1, row = 3 } } (LowerCase "b"), valueExpr__ = Located { end = { col = 7, row = 3 }, start = { col = 5, row = 3 } } (Frontend.Int 78) })
                ]
      , lexed =
            Ok
                [ Located { end = { col = 2, row = 1 }, start = { col = 1, row = 1 } } (Token (Identifier { name = TokenLowerCase (LowerCase "a"), qualifiers = [] }))
                , Located { end = { col = 3, row = 1 }, start = { col = 2, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 4, row = 1 }, start = { col = 3, row = 1 } } (Token (Sigil Assign))
                , Located { end = { col = 5, row = 1 }, start = { col = 4, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Token (NumericLiteral "5"))
                , Located { end = { col = 1, row = 3 }, start = { col = 6, row = 1 } }
                    (Newlines
                        [ 0
                        ]
                        0
                    )
                , Located { end = { col = 2, row = 3 }, start = { col = 1, row = 3 } } (Token (Identifier { name = TokenLowerCase (LowerCase "b"), qualifiers = [] }))
                , Located { end = { col = 3, row = 3 }, start = { col = 2, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 4, row = 3 }, start = { col = 3, row = 3 } } (Token (Sigil Assign))
                , Located { end = { col = 5, row = 3 }, start = { col = 4, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 7, row = 3 }, start = { col = 5, row = 3 } } (Token (NumericLiteral "78"))
                , Located { end = { col = 1, row = 4 }, start = { col = 7, row = 3 } } (Newlines [] 0)
                ]
      }
    , { name = "expression-int-add"
      , source = """a = 5 + 5

b = 78 + 5 + 2+ 4
"""
      , pretty = """
        ( ( Ok
          , ( ValueDeclaration
            , ( name, a )
            , ( args, () )
            , ( valueExpr__
              , ( Operator
                , ( ( op, + )
                  , ( lhs
                    , ( Int, 5 )
                    )
                  , ( rhs
                    , ( Int, 5 )
                    )
                  )
                )
              )
            )
          )
        , ( Ok
          , ( ValueDeclaration
            , ( name, b )
            , ( args, () )
            , ( valueExpr__
              , ( Operator
                , ( ( op, + )
                  , ( lhs
                    , ( Operator
                      , ( ( op, + )
                        , ( lhs
                          , ( Operator
                            , ( ( op, + )
                              , ( lhs
                                , ( Int, 78 )
                                )
                              , ( rhs
                                , ( Int, 5 )
                                )
                              )
                            )
                          )
                        , ( rhs
                          , ( Int, 2 )
                          )
                        )
                      )
                    )
                  , ( rhs
                    , ( Int, 4 )
                    )
                  )
                )
              )
            )
          )
        )
"""
      , contextualized =
            Just
                [ Ok (ValueDeclaration { args = [], name = Located { end = { col = 2, row = 1 }, start = { col = 1, row = 1 } } (LowerCase "a"), valueExpr__ = Located { end = { col = 10, row = 1 }, start = { col = 5, row = 1 } } (Frontend.Operator (Located { end = { col = 8, row = 1 }, start = { col = 7, row = 1 } } Add) (Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Frontend.Int 5)) (Located { end = { col = 10, row = 1 }, start = { col = 9, row = 1 } } (Frontend.Int 5))) })
                , Ok (ValueDeclaration { args = [], name = Located { end = { col = 2, row = 3 }, start = { col = 1, row = 3 } } (LowerCase "b"), valueExpr__ = Located { end = { col = 18, row = 3 }, start = { col = 5, row = 3 } } (Frontend.Operator (Located { end = { col = 16, row = 3 }, start = { col = 15, row = 3 } } Add) (Located { end = { col = 15, row = 3 }, start = { col = 5, row = 3 } } (Frontend.Operator (Located { end = { col = 13, row = 3 }, start = { col = 12, row = 3 } } Add) (Located { end = { col = 11, row = 3 }, start = { col = 5, row = 3 } } (Frontend.Operator (Located { end = { col = 9, row = 3 }, start = { col = 8, row = 3 } } Add) (Located { end = { col = 7, row = 3 }, start = { col = 5, row = 3 } } (Frontend.Int 78)) (Located { end = { col = 11, row = 3 }, start = { col = 10, row = 3 } } (Frontend.Int 5)))) (Located { end = { col = 15, row = 3 }, start = { col = 14, row = 3 } } (Frontend.Int 2)))) (Located { end = { col = 18, row = 3 }, start = { col = 17, row = 3 } } (Frontend.Int 4))) })
                ]
      , lexed =
            Ok
                [ Located { end = { col = 2, row = 1 }, start = { col = 1, row = 1 } } (Token (Identifier { name = TokenLowerCase (LowerCase "a"), qualifiers = [] }))
                , Located { end = { col = 3, row = 1 }, start = { col = 2, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 4, row = 1 }, start = { col = 3, row = 1 } } (Token (Sigil Assign))
                , Located { end = { col = 5, row = 1 }, start = { col = 4, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Token (NumericLiteral "5"))
                , Located { end = { col = 7, row = 1 }, start = { col = 6, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 8, row = 1 }, start = { col = 7, row = 1 } } (Token (Sigil (Operator Add)))
                , Located { end = { col = 9, row = 1 }, start = { col = 8, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 10, row = 1 }, start = { col = 9, row = 1 } } (Token (NumericLiteral "5"))
                , Located { end = { col = 1, row = 3 }, start = { col = 10, row = 1 } }
                    (Newlines
                        [ 0
                        ]
                        0
                    )
                , Located { end = { col = 2, row = 3 }, start = { col = 1, row = 3 } } (Token (Identifier { name = TokenLowerCase (LowerCase "b"), qualifiers = [] }))
                , Located { end = { col = 3, row = 3 }, start = { col = 2, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 4, row = 3 }, start = { col = 3, row = 3 } } (Token (Sigil Assign))
                , Located { end = { col = 5, row = 3 }, start = { col = 4, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 7, row = 3 }, start = { col = 5, row = 3 } } (Token (NumericLiteral "78"))
                , Located { end = { col = 8, row = 3 }, start = { col = 7, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 9, row = 3 }, start = { col = 8, row = 3 } } (Token (Sigil (Operator Add)))
                , Located { end = { col = 10, row = 3 }, start = { col = 9, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 11, row = 3 }, start = { col = 10, row = 3 } } (Token (NumericLiteral "5"))
                , Located { end = { col = 12, row = 3 }, start = { col = 11, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 13, row = 3 }, start = { col = 12, row = 3 } } (Token (Sigil (Operator Add)))
                , Located { end = { col = 14, row = 3 }, start = { col = 13, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 15, row = 3 }, start = { col = 14, row = 3 } } (Token (NumericLiteral "2"))
                , Located { end = { col = 16, row = 3 }, start = { col = 15, row = 3 } } (Token (Sigil (Operator Add)))
                , Located { end = { col = 17, row = 3 }, start = { col = 16, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 18, row = 3 }, start = { col = 17, row = 3 } } (Token (NumericLiteral "4"))
                , Located { end = { col = 1, row = 4 }, start = { col = 18, row = 3 } } (Newlines [] 0)
                ]
      }
    , { name = "expression-int-multiply"
      , source = """a = 5 * 5

b = 78 * 5 * 2 / 4 * 5
"""
      , pretty = """
        ( ( Ok
          , ( ValueDeclaration
            , ( name, a )
            , ( args, () )
            , ( valueExpr__
              , ( Operator
                , ( ( op, * )
                  , ( lhs
                    , ( Int, 5 )
                    )
                  , ( rhs
                    , ( Int, 5 )
                    )
                  )
                )
              )
            )
          )
        , ( Ok
          , ( ValueDeclaration
            , ( name, b )
            , ( args, () )
            , ( valueExpr__
              , ( Operator
                , ( ( op, * )
                  , ( lhs
                    , ( Operator
                      , ( ( op, / )
                        , ( lhs
                          , ( Operator
                            , ( ( op, * )
                              , ( lhs
                                , ( Operator
                                  , ( ( op, * )
                                    , ( lhs
                                      , ( Int, 78 )
                                      )
                                    , ( rhs
                                      , ( Int, 5 )
                                      )
                                    )
                                  )
                                )
                              , ( rhs
                                , ( Int, 2 )
                                )
                              )
                            )
                          )
                        , ( rhs
                          , ( Int, 4 )
                          )
                        )
                      )
                    )
                  , ( rhs
                    , ( Int, 5 )
                    )
                  )
                )
              )
            )
          )
        )
"""
      , contextualized =
            Just
                [ Ok (ValueDeclaration { args = [], name = Located { end = { col = 2, row = 1 }, start = { col = 1, row = 1 } } (LowerCase "a"), valueExpr__ = Located { end = { col = 10, row = 1 }, start = { col = 5, row = 1 } } (Frontend.Operator (Located { end = { col = 8, row = 1 }, start = { col = 7, row = 1 } } Multiply) (Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Frontend.Int 5)) (Located { end = { col = 10, row = 1 }, start = { col = 9, row = 1 } } (Frontend.Int 5))) })
                , Ok (ValueDeclaration { args = [], name = Located { end = { col = 2, row = 3 }, start = { col = 1, row = 3 } } (LowerCase "b"), valueExpr__ = Located { end = { col = 23, row = 3 }, start = { col = 5, row = 3 } } (Frontend.Operator (Located { end = { col = 21, row = 3 }, start = { col = 20, row = 3 } } Multiply) (Located { end = { col = 19, row = 3 }, start = { col = 5, row = 3 } } (Frontend.Operator (Located { end = { col = 17, row = 3 }, start = { col = 16, row = 3 } } Divide) (Located { end = { col = 15, row = 3 }, start = { col = 5, row = 3 } } (Frontend.Operator (Located { end = { col = 13, row = 3 }, start = { col = 12, row = 3 } } Multiply) (Located { end = { col = 11, row = 3 }, start = { col = 5, row = 3 } } (Frontend.Operator (Located { end = { col = 9, row = 3 }, start = { col = 8, row = 3 } } Multiply) (Located { end = { col = 7, row = 3 }, start = { col = 5, row = 3 } } (Frontend.Int 78)) (Located { end = { col = 11, row = 3 }, start = { col = 10, row = 3 } } (Frontend.Int 5)))) (Located { end = { col = 15, row = 3 }, start = { col = 14, row = 3 } } (Frontend.Int 2)))) (Located { end = { col = 19, row = 3 }, start = { col = 18, row = 3 } } (Frontend.Int 4)))) (Located { end = { col = 23, row = 3 }, start = { col = 22, row = 3 } } (Frontend.Int 5))) })
                ]
      , lexed =
            Ok
                [ Located { end = { col = 2, row = 1 }, start = { col = 1, row = 1 } } (Token (Identifier { name = TokenLowerCase (LowerCase "a"), qualifiers = [] }))
                , Located { end = { col = 3, row = 1 }, start = { col = 2, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 4, row = 1 }, start = { col = 3, row = 1 } } (Token (Sigil Assign))
                , Located { end = { col = 5, row = 1 }, start = { col = 4, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Token (NumericLiteral "5"))
                , Located { end = { col = 7, row = 1 }, start = { col = 6, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 8, row = 1 }, start = { col = 7, row = 1 } } (Token (Sigil (Operator Multiply)))
                , Located { end = { col = 9, row = 1 }, start = { col = 8, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 10, row = 1 }, start = { col = 9, row = 1 } } (Token (NumericLiteral "5"))
                , Located { end = { col = 1, row = 3 }, start = { col = 10, row = 1 } }
                    (Newlines
                        [ 0
                        ]
                        0
                    )
                , Located { end = { col = 2, row = 3 }, start = { col = 1, row = 3 } } (Token (Identifier { name = TokenLowerCase (LowerCase "b"), qualifiers = [] }))
                , Located { end = { col = 3, row = 3 }, start = { col = 2, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 4, row = 3 }, start = { col = 3, row = 3 } } (Token (Sigil Assign))
                , Located { end = { col = 5, row = 3 }, start = { col = 4, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 7, row = 3 }, start = { col = 5, row = 3 } } (Token (NumericLiteral "78"))
                , Located { end = { col = 8, row = 3 }, start = { col = 7, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 9, row = 3 }, start = { col = 8, row = 3 } } (Token (Sigil (Operator Multiply)))
                , Located { end = { col = 10, row = 3 }, start = { col = 9, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 11, row = 3 }, start = { col = 10, row = 3 } } (Token (NumericLiteral "5"))
                , Located { end = { col = 12, row = 3 }, start = { col = 11, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 13, row = 3 }, start = { col = 12, row = 3 } } (Token (Sigil (Operator Multiply)))
                , Located { end = { col = 14, row = 3 }, start = { col = 13, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 15, row = 3 }, start = { col = 14, row = 3 } } (Token (NumericLiteral "2"))
                , Located { end = { col = 16, row = 3 }, start = { col = 15, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 17, row = 3 }, start = { col = 16, row = 3 } } (Token (Sigil (Operator Divide)))
                , Located { end = { col = 18, row = 3 }, start = { col = 17, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 19, row = 3 }, start = { col = 18, row = 3 } } (Token (NumericLiteral "4"))
                , Located { end = { col = 20, row = 3 }, start = { col = 19, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 21, row = 3 }, start = { col = 20, row = 3 } } (Token (Sigil (Operator Multiply)))
                , Located { end = { col = 22, row = 3 }, start = { col = 21, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 23, row = 3 }, start = { col = 22, row = 3 } } (Token (NumericLiteral "5"))
                , Located { end = { col = 1, row = 4 }, start = { col = 23, row = 3 } } (Newlines [] 0)
                ]
      }
    , { name = "expression-int-multiply-and-add"
      , source = """a = 5 * 5 + 6
a1 =  7 + 5 * 5
a11 =  7 + 5 * 5 + 6
a2 = 100  + 5 * 5
a3 = 345 * 2234 + 2342 * 1010


b = 78 + 5 * 2 / 4 * 5
b1 = 78 * 5 * 2 / 4 - 5
b2 = 78 * 5 - 2 / 4 * 5
b3 = 78 - 5 + 2 / 4 * 5
b4 = 78 / 5 / 2 / 4 + 5

"""
      , pretty = """
        ( ( Ok
          , ( ValueDeclaration
            , ( name, a )
            , ( args, () )
            , ( valueExpr__
              , ( Operator
                , ( ( op, + )
                  , ( lhs
                    , ( Operator
                      , ( ( op, * )
                        , ( lhs
                          , ( Int, 5 )
                          )
                        , ( rhs
                          , ( Int, 5 )
                          )
                        )
                      )
                    )
                  , ( rhs
                    , ( Int, 6 )
                    )
                  )
                )
              )
            )
          )
        , ( Ok
          , ( ValueDeclaration
            , ( name, a1 )
            , ( args, () )
            , ( valueExpr__
              , ( Operator
                , ( ( op, + )
                  , ( lhs
                    , ( Int, 7 )
                    )
                  , ( rhs
                    , ( Operator
                      , ( ( op, * )
                        , ( lhs
                          , ( Int, 5 )
                          )
                        , ( rhs
                          , ( Int, 5 )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        , ( Ok
          , ( ValueDeclaration
            , ( name, a11 )
            , ( args, () )
            , ( valueExpr__
              , ( Operator
                , ( ( op, + )
                  , ( lhs
                    , ( Int, 7 )
                    )
                  , ( rhs
                    , ( Operator
                      , ( ( op, + )
                        , ( lhs
                          , ( Operator
                            , ( ( op, * )
                              , ( lhs
                                , ( Int, 5 )
                                )
                              , ( rhs
                                , ( Int, 5 )
                                )
                              )
                            )
                          )
                        , ( rhs
                          , ( Int, 6 )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        , ( Ok
          , ( ValueDeclaration
            , ( name, a2 )
            , ( args, () )
            , ( valueExpr__
              , ( Operator
                , ( ( op, + )
                  , ( lhs
                    , ( Int, 100 )
                    )
                  , ( rhs
                    , ( Operator
                      , ( ( op, * )
                        , ( lhs
                          , ( Int, 5 )
                          )
                        , ( rhs
                          , ( Int, 5 )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        , ( Ok
          , ( ValueDeclaration
            , ( name, a3 )
            , ( args, () )
            , ( valueExpr__
              , ( Operator
                , ( ( op, + )
                  , ( lhs
                    , ( Operator
                      , ( ( op, * )
                        , ( lhs
                          , ( Int, 345 )
                          )
                        , ( rhs
                          , ( Int, 2234 )
                          )
                        )
                      )
                    )
                  , ( rhs
                    , ( Operator
                      , ( ( op, * )
                        , ( lhs
                          , ( Int, 2342 )
                          )
                        , ( rhs
                          , ( Int, 1010 )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        , ( Ok
          , ( ValueDeclaration
            , ( name, b )
            , ( args, () )
            , ( valueExpr__
              , ( Operator
                , ( ( op, + )
                  , ( lhs
                    , ( Int, 78 )
                    )
                  , ( rhs
                    , ( Operator
                      , ( ( op, * )
                        , ( lhs
                          , ( Operator
                            , ( ( op, / )
                              , ( lhs
                                , ( Operator
                                  , ( ( op, * )
                                    , ( lhs
                                      , ( Int, 5 )
                                      )
                                    , ( rhs
                                      , ( Int, 2 )
                                      )
                                    )
                                  )
                                )
                              , ( rhs
                                , ( Int, 4 )
                                )
                              )
                            )
                          )
                        , ( rhs
                          , ( Int, 5 )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        , ( Ok
          , ( ValueDeclaration
            , ( name, b1 )
            , ( args, () )
            , ( valueExpr__
              , ( Operator
                , ( ( op, - )
                  , ( lhs
                    , ( Operator
                      , ( ( op, / )
                        , ( lhs
                          , ( Operator
                            , ( ( op, * )
                              , ( lhs
                                , ( Operator
                                  , ( ( op, * )
                                    , ( lhs
                                      , ( Int, 78 )
                                      )
                                    , ( rhs
                                      , ( Int, 5 )
                                      )
                                    )
                                  )
                                )
                              , ( rhs
                                , ( Int, 2 )
                                )
                              )
                            )
                          )
                        , ( rhs
                          , ( Int, 4 )
                          )
                        )
                      )
                    )
                  , ( rhs
                    , ( Int, 5 )
                    )
                  )
                )
              )
            )
          )
        , ( Ok
          , ( ValueDeclaration
            , ( name, b2 )
            , ( args, () )
            , ( valueExpr__
              , ( Operator
                , ( ( op, - )
                  , ( lhs
                    , ( Operator
                      , ( ( op, * )
                        , ( lhs
                          , ( Int, 78 )
                          )
                        , ( rhs
                          , ( Int, 5 )
                          )
                        )
                      )
                    )
                  , ( rhs
                    , ( Operator
                      , ( ( op, * )
                        , ( lhs
                          , ( Operator
                            , ( ( op, / )
                              , ( lhs
                                , ( Int, 2 )
                                )
                              , ( rhs
                                , ( Int, 4 )
                                )
                              )
                            )
                          )
                        , ( rhs
                          , ( Int, 5 )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        , ( Ok
          , ( ValueDeclaration
            , ( name, b3 )
            , ( args, () )
            , ( valueExpr__
              , ( Operator
                , ( ( op, + )
                  , ( lhs
                    , ( Operator
                      , ( ( op, - )
                        , ( lhs
                          , ( Int, 78 )
                          )
                        , ( rhs
                          , ( Int, 5 )
                          )
                        )
                      )
                    )
                  , ( rhs
                    , ( Operator
                      , ( ( op, * )
                        , ( lhs
                          , ( Operator
                            , ( ( op, / )
                              , ( lhs
                                , ( Int, 2 )
                                )
                              , ( rhs
                                , ( Int, 4 )
                                )
                              )
                            )
                          )
                        , ( rhs
                          , ( Int, 5 )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        , ( Ok
          , ( ValueDeclaration
            , ( name, b4 )
            , ( args, () )
            , ( valueExpr__
              , ( Operator
                , ( ( op, + )
                  , ( lhs
                    , ( Operator
                      , ( ( op, / )
                        , ( lhs
                          , ( Operator
                            , ( ( op, / )
                              , ( lhs
                                , ( Operator
                                  , ( ( op, / )
                                    , ( lhs
                                      , ( Int, 78 )
                                      )
                                    , ( rhs
                                      , ( Int, 5 )
                                      )
                                    )
                                  )
                                )
                              , ( rhs
                                , ( Int, 2 )
                                )
                              )
                            )
                          )
                        , ( rhs
                          , ( Int, 4 )
                          )
                        )
                      )
                    )
                  , ( rhs
                    , ( Int, 5 )
                    )
                  )
                )
              )
            )
          )
        )
"""
      , contextualized =
            Just
                [ Ok (ValueDeclaration { args = [], name = Located { end = { col = 2, row = 1 }, start = { col = 1, row = 1 } } (LowerCase "a"), valueExpr__ = Located { end = { col = 14, row = 1 }, start = { col = 5, row = 1 } } (Frontend.Operator (Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } Add) (Located { end = { col = 10, row = 1 }, start = { col = 5, row = 1 } } (Frontend.Operator (Located { end = { col = 8, row = 1 }, start = { col = 7, row = 1 } } Multiply) (Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Frontend.Int 5)) (Located { end = { col = 10, row = 1 }, start = { col = 9, row = 1 } } (Frontend.Int 5)))) (Located { end = { col = 14, row = 1 }, start = { col = 13, row = 1 } } (Frontend.Int 6))) })
                , Ok (ValueDeclaration { args = [], name = Located { end = { col = 3, row = 2 }, start = { col = 1, row = 2 } } (LowerCase "a1"), valueExpr__ = Located { end = { col = 16, row = 2 }, start = { col = 7, row = 2 } } (Frontend.Operator (Located { end = { col = 10, row = 2 }, start = { col = 9, row = 2 } } Add) (Located { end = { col = 8, row = 2 }, start = { col = 7, row = 2 } } (Frontend.Int 7)) (Located { end = { col = 16, row = 2 }, start = { col = 11, row = 2 } } (Frontend.Operator (Located { end = { col = 14, row = 2 }, start = { col = 13, row = 2 } } Multiply) (Located { end = { col = 12, row = 2 }, start = { col = 11, row = 2 } } (Frontend.Int 5)) (Located { end = { col = 16, row = 2 }, start = { col = 15, row = 2 } } (Frontend.Int 5))))) })
                , Ok (ValueDeclaration { args = [], name = Located { end = { col = 4, row = 3 }, start = { col = 1, row = 3 } } (LowerCase "a11"), valueExpr__ = Located { end = { col = 21, row = 3 }, start = { col = 8, row = 3 } } (Frontend.Operator (Located { end = { col = 11, row = 3 }, start = { col = 10, row = 3 } } Add) (Located { end = { col = 9, row = 3 }, start = { col = 8, row = 3 } } (Frontend.Int 7)) (Located { end = { col = 21, row = 3 }, start = { col = 12, row = 3 } } (Frontend.Operator (Located { end = { col = 19, row = 3 }, start = { col = 18, row = 3 } } Add) (Located { end = { col = 17, row = 3 }, start = { col = 12, row = 3 } } (Frontend.Operator (Located { end = { col = 15, row = 3 }, start = { col = 14, row = 3 } } Multiply) (Located { end = { col = 13, row = 3 }, start = { col = 12, row = 3 } } (Frontend.Int 5)) (Located { end = { col = 17, row = 3 }, start = { col = 16, row = 3 } } (Frontend.Int 5)))) (Located { end = { col = 21, row = 3 }, start = { col = 20, row = 3 } } (Frontend.Int 6))))) })
                , Ok (ValueDeclaration { args = [], name = Located { end = { col = 3, row = 4 }, start = { col = 1, row = 4 } } (LowerCase "a2"), valueExpr__ = Located { end = { col = 18, row = 4 }, start = { col = 6, row = 4 } } (Frontend.Operator (Located { end = { col = 12, row = 4 }, start = { col = 11, row = 4 } } Add) (Located { end = { col = 9, row = 4 }, start = { col = 6, row = 4 } } (Frontend.Int 100)) (Located { end = { col = 18, row = 4 }, start = { col = 13, row = 4 } } (Frontend.Operator (Located { end = { col = 16, row = 4 }, start = { col = 15, row = 4 } } Multiply) (Located { end = { col = 14, row = 4 }, start = { col = 13, row = 4 } } (Frontend.Int 5)) (Located { end = { col = 18, row = 4 }, start = { col = 17, row = 4 } } (Frontend.Int 5))))) })
                , Ok (ValueDeclaration { args = [], name = Located { end = { col = 3, row = 5 }, start = { col = 1, row = 5 } } (LowerCase "a3"), valueExpr__ = Located { end = { col = 30, row = 5 }, start = { col = 6, row = 5 } } (Frontend.Operator (Located { end = { col = 18, row = 5 }, start = { col = 17, row = 5 } } Add) (Located { end = { col = 16, row = 5 }, start = { col = 6, row = 5 } } (Frontend.Operator (Located { end = { col = 11, row = 5 }, start = { col = 10, row = 5 } } Multiply) (Located { end = { col = 9, row = 5 }, start = { col = 6, row = 5 } } (Frontend.Int 345)) (Located { end = { col = 16, row = 5 }, start = { col = 12, row = 5 } } (Frontend.Int 2234)))) (Located { end = { col = 30, row = 5 }, start = { col = 19, row = 5 } } (Frontend.Operator (Located { end = { col = 25, row = 5 }, start = { col = 24, row = 5 } } Multiply) (Located { end = { col = 23, row = 5 }, start = { col = 19, row = 5 } } (Frontend.Int 2342)) (Located { end = { col = 30, row = 5 }, start = { col = 26, row = 5 } } (Frontend.Int 1010))))) })
                , Ok (ValueDeclaration { args = [], name = Located { end = { col = 2, row = 8 }, start = { col = 1, row = 8 } } (LowerCase "b"), valueExpr__ = Located { end = { col = 23, row = 8 }, start = { col = 5, row = 8 } } (Frontend.Operator (Located { end = { col = 9, row = 8 }, start = { col = 8, row = 8 } } Add) (Located { end = { col = 7, row = 8 }, start = { col = 5, row = 8 } } (Frontend.Int 78)) (Located { end = { col = 23, row = 8 }, start = { col = 10, row = 8 } } (Frontend.Operator (Located { end = { col = 21, row = 8 }, start = { col = 20, row = 8 } } Multiply) (Located { end = { col = 19, row = 8 }, start = { col = 10, row = 8 } } (Frontend.Operator (Located { end = { col = 17, row = 8 }, start = { col = 16, row = 8 } } Divide) (Located { end = { col = 15, row = 8 }, start = { col = 10, row = 8 } } (Frontend.Operator (Located { end = { col = 13, row = 8 }, start = { col = 12, row = 8 } } Multiply) (Located { end = { col = 11, row = 8 }, start = { col = 10, row = 8 } } (Frontend.Int 5)) (Located { end = { col = 15, row = 8 }, start = { col = 14, row = 8 } } (Frontend.Int 2)))) (Located { end = { col = 19, row = 8 }, start = { col = 18, row = 8 } } (Frontend.Int 4)))) (Located { end = { col = 23, row = 8 }, start = { col = 22, row = 8 } } (Frontend.Int 5))))) })
                , Ok (ValueDeclaration { args = [], name = Located { end = { col = 3, row = 9 }, start = { col = 1, row = 9 } } (LowerCase "b1"), valueExpr__ = Located { end = { col = 24, row = 9 }, start = { col = 6, row = 9 } } (Frontend.Operator (Located { end = { col = 22, row = 9 }, start = { col = 21, row = 9 } } Subtract) (Located { end = { col = 20, row = 9 }, start = { col = 6, row = 9 } } (Frontend.Operator (Located { end = { col = 18, row = 9 }, start = { col = 17, row = 9 } } Divide) (Located { end = { col = 16, row = 9 }, start = { col = 6, row = 9 } } (Frontend.Operator (Located { end = { col = 14, row = 9 }, start = { col = 13, row = 9 } } Multiply) (Located { end = { col = 12, row = 9 }, start = { col = 6, row = 9 } } (Frontend.Operator (Located { end = { col = 10, row = 9 }, start = { col = 9, row = 9 } } Multiply) (Located { end = { col = 8, row = 9 }, start = { col = 6, row = 9 } } (Frontend.Int 78)) (Located { end = { col = 12, row = 9 }, start = { col = 11, row = 9 } } (Frontend.Int 5)))) (Located { end = { col = 16, row = 9 }, start = { col = 15, row = 9 } } (Frontend.Int 2)))) (Located { end = { col = 20, row = 9 }, start = { col = 19, row = 9 } } (Frontend.Int 4)))) (Located { end = { col = 24, row = 9 }, start = { col = 23, row = 9 } } (Frontend.Int 5))) })
                , Ok (ValueDeclaration { args = [], name = Located { end = { col = 3, row = 10 }, start = { col = 1, row = 10 } } (LowerCase "b2"), valueExpr__ = Located { end = { col = 24, row = 10 }, start = { col = 6, row = 10 } } (Frontend.Operator (Located { end = { col = 14, row = 10 }, start = { col = 13, row = 10 } } Subtract) (Located { end = { col = 12, row = 10 }, start = { col = 6, row = 10 } } (Frontend.Operator (Located { end = { col = 10, row = 10 }, start = { col = 9, row = 10 } } Multiply) (Located { end = { col = 8, row = 10 }, start = { col = 6, row = 10 } } (Frontend.Int 78)) (Located { end = { col = 12, row = 10 }, start = { col = 11, row = 10 } } (Frontend.Int 5)))) (Located { end = { col = 24, row = 10 }, start = { col = 15, row = 10 } } (Frontend.Operator (Located { end = { col = 22, row = 10 }, start = { col = 21, row = 10 } } Multiply) (Located { end = { col = 20, row = 10 }, start = { col = 15, row = 10 } } (Frontend.Operator (Located { end = { col = 18, row = 10 }, start = { col = 17, row = 10 } } Divide) (Located { end = { col = 16, row = 10 }, start = { col = 15, row = 10 } } (Frontend.Int 2)) (Located { end = { col = 20, row = 10 }, start = { col = 19, row = 10 } } (Frontend.Int 4)))) (Located { end = { col = 24, row = 10 }, start = { col = 23, row = 10 } } (Frontend.Int 5))))) })
                , Ok (ValueDeclaration { args = [], name = Located { end = { col = 3, row = 11 }, start = { col = 1, row = 11 } } (LowerCase "b3"), valueExpr__ = Located { end = { col = 24, row = 11 }, start = { col = 6, row = 11 } } (Frontend.Operator (Located { end = { col = 14, row = 11 }, start = { col = 13, row = 11 } } Add) (Located { end = { col = 12, row = 11 }, start = { col = 6, row = 11 } } (Frontend.Operator (Located { end = { col = 10, row = 11 }, start = { col = 9, row = 11 } } Subtract) (Located { end = { col = 8, row = 11 }, start = { col = 6, row = 11 } } (Frontend.Int 78)) (Located { end = { col = 12, row = 11 }, start = { col = 11, row = 11 } } (Frontend.Int 5)))) (Located { end = { col = 24, row = 11 }, start = { col = 15, row = 11 } } (Frontend.Operator (Located { end = { col = 22, row = 11 }, start = { col = 21, row = 11 } } Multiply) (Located { end = { col = 20, row = 11 }, start = { col = 15, row = 11 } } (Frontend.Operator (Located { end = { col = 18, row = 11 }, start = { col = 17, row = 11 } } Divide) (Located { end = { col = 16, row = 11 }, start = { col = 15, row = 11 } } (Frontend.Int 2)) (Located { end = { col = 20, row = 11 }, start = { col = 19, row = 11 } } (Frontend.Int 4)))) (Located { end = { col = 24, row = 11 }, start = { col = 23, row = 11 } } (Frontend.Int 5))))) })
                , Ok (ValueDeclaration { args = [], name = Located { end = { col = 3, row = 12 }, start = { col = 1, row = 12 } } (LowerCase "b4"), valueExpr__ = Located { end = { col = 24, row = 12 }, start = { col = 6, row = 12 } } (Frontend.Operator (Located { end = { col = 22, row = 12 }, start = { col = 21, row = 12 } } Add) (Located { end = { col = 20, row = 12 }, start = { col = 6, row = 12 } } (Frontend.Operator (Located { end = { col = 18, row = 12 }, start = { col = 17, row = 12 } } Divide) (Located { end = { col = 16, row = 12 }, start = { col = 6, row = 12 } } (Frontend.Operator (Located { end = { col = 14, row = 12 }, start = { col = 13, row = 12 } } Divide) (Located { end = { col = 12, row = 12 }, start = { col = 6, row = 12 } } (Frontend.Operator (Located { end = { col = 10, row = 12 }, start = { col = 9, row = 12 } } Divide) (Located { end = { col = 8, row = 12 }, start = { col = 6, row = 12 } } (Frontend.Int 78)) (Located { end = { col = 12, row = 12 }, start = { col = 11, row = 12 } } (Frontend.Int 5)))) (Located { end = { col = 16, row = 12 }, start = { col = 15, row = 12 } } (Frontend.Int 2)))) (Located { end = { col = 20, row = 12 }, start = { col = 19, row = 12 } } (Frontend.Int 4)))) (Located { end = { col = 24, row = 12 }, start = { col = 23, row = 12 } } (Frontend.Int 5))) })
                ]
      , lexed =
            Ok
                [ Located { end = { col = 2, row = 1 }, start = { col = 1, row = 1 } } (Token (Identifier { name = TokenLowerCase (LowerCase "a"), qualifiers = [] }))
                , Located { end = { col = 3, row = 1 }, start = { col = 2, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 4, row = 1 }, start = { col = 3, row = 1 } } (Token (Sigil Assign))
                , Located { end = { col = 5, row = 1 }, start = { col = 4, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Token (NumericLiteral "5"))
                , Located { end = { col = 7, row = 1 }, start = { col = 6, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 8, row = 1 }, start = { col = 7, row = 1 } } (Token (Sigil (Operator Multiply)))
                , Located { end = { col = 9, row = 1 }, start = { col = 8, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 10, row = 1 }, start = { col = 9, row = 1 } } (Token (NumericLiteral "5"))
                , Located { end = { col = 11, row = 1 }, start = { col = 10, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Token (Sigil (Operator Add)))
                , Located { end = { col = 13, row = 1 }, start = { col = 12, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 14, row = 1 }, start = { col = 13, row = 1 } } (Token (NumericLiteral "6"))
                , Located { end = { col = 1, row = 2 }, start = { col = 14, row = 1 } } (Newlines [] 0)
                , Located { end = { col = 3, row = 2 }, start = { col = 1, row = 2 } } (Token (Identifier { name = TokenLowerCase (LowerCase "a1"), qualifiers = [] }))
                , Located { end = { col = 4, row = 2 }, start = { col = 3, row = 2 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 5, row = 2 }, start = { col = 4, row = 2 } } (Token (Sigil Assign))
                , Located { end = { col = 7, row = 2 }, start = { col = 5, row = 2 } } (Ignorable (Whitespace 2))
                , Located { end = { col = 8, row = 2 }, start = { col = 7, row = 2 } } (Token (NumericLiteral "7"))
                , Located { end = { col = 9, row = 2 }, start = { col = 8, row = 2 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 10, row = 2 }, start = { col = 9, row = 2 } } (Token (Sigil (Operator Add)))
                , Located { end = { col = 11, row = 2 }, start = { col = 10, row = 2 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 12, row = 2 }, start = { col = 11, row = 2 } } (Token (NumericLiteral "5"))
                , Located { end = { col = 13, row = 2 }, start = { col = 12, row = 2 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 14, row = 2 }, start = { col = 13, row = 2 } } (Token (Sigil (Operator Multiply)))
                , Located { end = { col = 15, row = 2 }, start = { col = 14, row = 2 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 16, row = 2 }, start = { col = 15, row = 2 } } (Token (NumericLiteral "5"))
                , Located { end = { col = 1, row = 3 }, start = { col = 16, row = 2 } } (Newlines [] 0)
                , Located { end = { col = 4, row = 3 }, start = { col = 1, row = 3 } } (Token (Identifier { name = TokenLowerCase (LowerCase "a11"), qualifiers = [] }))
                , Located { end = { col = 5, row = 3 }, start = { col = 4, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 6, row = 3 }, start = { col = 5, row = 3 } } (Token (Sigil Assign))
                , Located { end = { col = 8, row = 3 }, start = { col = 6, row = 3 } } (Ignorable (Whitespace 2))
                , Located { end = { col = 9, row = 3 }, start = { col = 8, row = 3 } } (Token (NumericLiteral "7"))
                , Located { end = { col = 10, row = 3 }, start = { col = 9, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 11, row = 3 }, start = { col = 10, row = 3 } } (Token (Sigil (Operator Add)))
                , Located { end = { col = 12, row = 3 }, start = { col = 11, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 13, row = 3 }, start = { col = 12, row = 3 } } (Token (NumericLiteral "5"))
                , Located { end = { col = 14, row = 3 }, start = { col = 13, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 15, row = 3 }, start = { col = 14, row = 3 } } (Token (Sigil (Operator Multiply)))
                , Located { end = { col = 16, row = 3 }, start = { col = 15, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 17, row = 3 }, start = { col = 16, row = 3 } } (Token (NumericLiteral "5"))
                , Located { end = { col = 18, row = 3 }, start = { col = 17, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 19, row = 3 }, start = { col = 18, row = 3 } } (Token (Sigil (Operator Add)))
                , Located { end = { col = 20, row = 3 }, start = { col = 19, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 21, row = 3 }, start = { col = 20, row = 3 } } (Token (NumericLiteral "6"))
                , Located { end = { col = 1, row = 4 }, start = { col = 21, row = 3 } } (Newlines [] 0)
                , Located { end = { col = 3, row = 4 }, start = { col = 1, row = 4 } } (Token (Identifier { name = TokenLowerCase (LowerCase "a2"), qualifiers = [] }))
                , Located { end = { col = 4, row = 4 }, start = { col = 3, row = 4 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 5, row = 4 }, start = { col = 4, row = 4 } } (Token (Sigil Assign))
                , Located { end = { col = 6, row = 4 }, start = { col = 5, row = 4 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 9, row = 4 }, start = { col = 6, row = 4 } } (Token (NumericLiteral "100"))
                , Located { end = { col = 11, row = 4 }, start = { col = 9, row = 4 } } (Ignorable (Whitespace 2))
                , Located { end = { col = 12, row = 4 }, start = { col = 11, row = 4 } } (Token (Sigil (Operator Add)))
                , Located { end = { col = 13, row = 4 }, start = { col = 12, row = 4 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 14, row = 4 }, start = { col = 13, row = 4 } } (Token (NumericLiteral "5"))
                , Located { end = { col = 15, row = 4 }, start = { col = 14, row = 4 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 16, row = 4 }, start = { col = 15, row = 4 } } (Token (Sigil (Operator Multiply)))
                , Located { end = { col = 17, row = 4 }, start = { col = 16, row = 4 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 18, row = 4 }, start = { col = 17, row = 4 } } (Token (NumericLiteral "5"))
                , Located { end = { col = 1, row = 5 }, start = { col = 18, row = 4 } } (Newlines [] 0)
                , Located { end = { col = 3, row = 5 }, start = { col = 1, row = 5 } } (Token (Identifier { name = TokenLowerCase (LowerCase "a3"), qualifiers = [] }))
                , Located { end = { col = 4, row = 5 }, start = { col = 3, row = 5 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 5, row = 5 }, start = { col = 4, row = 5 } } (Token (Sigil Assign))
                , Located { end = { col = 6, row = 5 }, start = { col = 5, row = 5 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 9, row = 5 }, start = { col = 6, row = 5 } } (Token (NumericLiteral "345"))
                , Located { end = { col = 10, row = 5 }, start = { col = 9, row = 5 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 11, row = 5 }, start = { col = 10, row = 5 } } (Token (Sigil (Operator Multiply)))
                , Located { end = { col = 12, row = 5 }, start = { col = 11, row = 5 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 16, row = 5 }, start = { col = 12, row = 5 } } (Token (NumericLiteral "2234"))
                , Located { end = { col = 17, row = 5 }, start = { col = 16, row = 5 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 18, row = 5 }, start = { col = 17, row = 5 } } (Token (Sigil (Operator Add)))
                , Located { end = { col = 19, row = 5 }, start = { col = 18, row = 5 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 23, row = 5 }, start = { col = 19, row = 5 } } (Token (NumericLiteral "2342"))
                , Located { end = { col = 24, row = 5 }, start = { col = 23, row = 5 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 25, row = 5 }, start = { col = 24, row = 5 } } (Token (Sigil (Operator Multiply)))
                , Located { end = { col = 26, row = 5 }, start = { col = 25, row = 5 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 30, row = 5 }, start = { col = 26, row = 5 } } (Token (NumericLiteral "1010"))
                , Located { end = { col = 1, row = 8 }, start = { col = 30, row = 5 } }
                    (Newlines
                        [ 0
                        , 0
                        ]
                        0
                    )
                , Located { end = { col = 2, row = 8 }, start = { col = 1, row = 8 } } (Token (Identifier { name = TokenLowerCase (LowerCase "b"), qualifiers = [] }))
                , Located { end = { col = 3, row = 8 }, start = { col = 2, row = 8 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 4, row = 8 }, start = { col = 3, row = 8 } } (Token (Sigil Assign))
                , Located { end = { col = 5, row = 8 }, start = { col = 4, row = 8 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 7, row = 8 }, start = { col = 5, row = 8 } } (Token (NumericLiteral "78"))
                , Located { end = { col = 8, row = 8 }, start = { col = 7, row = 8 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 9, row = 8 }, start = { col = 8, row = 8 } } (Token (Sigil (Operator Add)))
                , Located { end = { col = 10, row = 8 }, start = { col = 9, row = 8 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 11, row = 8 }, start = { col = 10, row = 8 } } (Token (NumericLiteral "5"))
                , Located { end = { col = 12, row = 8 }, start = { col = 11, row = 8 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 13, row = 8 }, start = { col = 12, row = 8 } } (Token (Sigil (Operator Multiply)))
                , Located { end = { col = 14, row = 8 }, start = { col = 13, row = 8 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 15, row = 8 }, start = { col = 14, row = 8 } } (Token (NumericLiteral "2"))
                , Located { end = { col = 16, row = 8 }, start = { col = 15, row = 8 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 17, row = 8 }, start = { col = 16, row = 8 } } (Token (Sigil (Operator Divide)))
                , Located { end = { col = 18, row = 8 }, start = { col = 17, row = 8 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 19, row = 8 }, start = { col = 18, row = 8 } } (Token (NumericLiteral "4"))
                , Located { end = { col = 20, row = 8 }, start = { col = 19, row = 8 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 21, row = 8 }, start = { col = 20, row = 8 } } (Token (Sigil (Operator Multiply)))
                , Located { end = { col = 22, row = 8 }, start = { col = 21, row = 8 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 23, row = 8 }, start = { col = 22, row = 8 } } (Token (NumericLiteral "5"))
                , Located { end = { col = 1, row = 9 }, start = { col = 23, row = 8 } } (Newlines [] 0)
                , Located { end = { col = 3, row = 9 }, start = { col = 1, row = 9 } } (Token (Identifier { name = TokenLowerCase (LowerCase "b1"), qualifiers = [] }))
                , Located { end = { col = 4, row = 9 }, start = { col = 3, row = 9 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 5, row = 9 }, start = { col = 4, row = 9 } } (Token (Sigil Assign))
                , Located { end = { col = 6, row = 9 }, start = { col = 5, row = 9 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 8, row = 9 }, start = { col = 6, row = 9 } } (Token (NumericLiteral "78"))
                , Located { end = { col = 9, row = 9 }, start = { col = 8, row = 9 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 10, row = 9 }, start = { col = 9, row = 9 } } (Token (Sigil (Operator Multiply)))
                , Located { end = { col = 11, row = 9 }, start = { col = 10, row = 9 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 12, row = 9 }, start = { col = 11, row = 9 } } (Token (NumericLiteral "5"))
                , Located { end = { col = 13, row = 9 }, start = { col = 12, row = 9 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 14, row = 9 }, start = { col = 13, row = 9 } } (Token (Sigil (Operator Multiply)))
                , Located { end = { col = 15, row = 9 }, start = { col = 14, row = 9 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 16, row = 9 }, start = { col = 15, row = 9 } } (Token (NumericLiteral "2"))
                , Located { end = { col = 17, row = 9 }, start = { col = 16, row = 9 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 18, row = 9 }, start = { col = 17, row = 9 } } (Token (Sigil (Operator Divide)))
                , Located { end = { col = 19, row = 9 }, start = { col = 18, row = 9 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 20, row = 9 }, start = { col = 19, row = 9 } } (Token (NumericLiteral "4"))
                , Located { end = { col = 21, row = 9 }, start = { col = 20, row = 9 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 22, row = 9 }, start = { col = 21, row = 9 } } (Token (Sigil (Operator Subtract)))
                , Located { end = { col = 23, row = 9 }, start = { col = 22, row = 9 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 24, row = 9 }, start = { col = 23, row = 9 } } (Token (NumericLiteral "5"))
                , Located { end = { col = 1, row = 10 }, start = { col = 24, row = 9 } } (Newlines [] 0)
                , Located { end = { col = 3, row = 10 }, start = { col = 1, row = 10 } } (Token (Identifier { name = TokenLowerCase (LowerCase "b2"), qualifiers = [] }))
                , Located { end = { col = 4, row = 10 }, start = { col = 3, row = 10 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 5, row = 10 }, start = { col = 4, row = 10 } } (Token (Sigil Assign))
                , Located { end = { col = 6, row = 10 }, start = { col = 5, row = 10 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 8, row = 10 }, start = { col = 6, row = 10 } } (Token (NumericLiteral "78"))
                , Located { end = { col = 9, row = 10 }, start = { col = 8, row = 10 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 10, row = 10 }, start = { col = 9, row = 10 } } (Token (Sigil (Operator Multiply)))
                , Located { end = { col = 11, row = 10 }, start = { col = 10, row = 10 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 12, row = 10 }, start = { col = 11, row = 10 } } (Token (NumericLiteral "5"))
                , Located { end = { col = 13, row = 10 }, start = { col = 12, row = 10 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 14, row = 10 }, start = { col = 13, row = 10 } } (Token (Sigil (Operator Subtract)))
                , Located { end = { col = 15, row = 10 }, start = { col = 14, row = 10 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 16, row = 10 }, start = { col = 15, row = 10 } } (Token (NumericLiteral "2"))
                , Located { end = { col = 17, row = 10 }, start = { col = 16, row = 10 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 18, row = 10 }, start = { col = 17, row = 10 } } (Token (Sigil (Operator Divide)))
                , Located { end = { col = 19, row = 10 }, start = { col = 18, row = 10 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 20, row = 10 }, start = { col = 19, row = 10 } } (Token (NumericLiteral "4"))
                , Located { end = { col = 21, row = 10 }, start = { col = 20, row = 10 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 22, row = 10 }, start = { col = 21, row = 10 } } (Token (Sigil (Operator Multiply)))
                , Located { end = { col = 23, row = 10 }, start = { col = 22, row = 10 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 24, row = 10 }, start = { col = 23, row = 10 } } (Token (NumericLiteral "5"))
                , Located { end = { col = 1, row = 11 }, start = { col = 24, row = 10 } } (Newlines [] 0)
                , Located { end = { col = 3, row = 11 }, start = { col = 1, row = 11 } } (Token (Identifier { name = TokenLowerCase (LowerCase "b3"), qualifiers = [] }))
                , Located { end = { col = 4, row = 11 }, start = { col = 3, row = 11 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 5, row = 11 }, start = { col = 4, row = 11 } } (Token (Sigil Assign))
                , Located { end = { col = 6, row = 11 }, start = { col = 5, row = 11 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 8, row = 11 }, start = { col = 6, row = 11 } } (Token (NumericLiteral "78"))
                , Located { end = { col = 9, row = 11 }, start = { col = 8, row = 11 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 10, row = 11 }, start = { col = 9, row = 11 } } (Token (Sigil (Operator Subtract)))
                , Located { end = { col = 11, row = 11 }, start = { col = 10, row = 11 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 12, row = 11 }, start = { col = 11, row = 11 } } (Token (NumericLiteral "5"))
                , Located { end = { col = 13, row = 11 }, start = { col = 12, row = 11 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 14, row = 11 }, start = { col = 13, row = 11 } } (Token (Sigil (Operator Add)))
                , Located { end = { col = 15, row = 11 }, start = { col = 14, row = 11 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 16, row = 11 }, start = { col = 15, row = 11 } } (Token (NumericLiteral "2"))
                , Located { end = { col = 17, row = 11 }, start = { col = 16, row = 11 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 18, row = 11 }, start = { col = 17, row = 11 } } (Token (Sigil (Operator Divide)))
                , Located { end = { col = 19, row = 11 }, start = { col = 18, row = 11 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 20, row = 11 }, start = { col = 19, row = 11 } } (Token (NumericLiteral "4"))
                , Located { end = { col = 21, row = 11 }, start = { col = 20, row = 11 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 22, row = 11 }, start = { col = 21, row = 11 } } (Token (Sigil (Operator Multiply)))
                , Located { end = { col = 23, row = 11 }, start = { col = 22, row = 11 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 24, row = 11 }, start = { col = 23, row = 11 } } (Token (NumericLiteral "5"))
                , Located { end = { col = 1, row = 12 }, start = { col = 24, row = 11 } } (Newlines [] 0)
                , Located { end = { col = 3, row = 12 }, start = { col = 1, row = 12 } } (Token (Identifier { name = TokenLowerCase (LowerCase "b4"), qualifiers = [] }))
                , Located { end = { col = 4, row = 12 }, start = { col = 3, row = 12 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 5, row = 12 }, start = { col = 4, row = 12 } } (Token (Sigil Assign))
                , Located { end = { col = 6, row = 12 }, start = { col = 5, row = 12 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 8, row = 12 }, start = { col = 6, row = 12 } } (Token (NumericLiteral "78"))
                , Located { end = { col = 9, row = 12 }, start = { col = 8, row = 12 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 10, row = 12 }, start = { col = 9, row = 12 } } (Token (Sigil (Operator Divide)))
                , Located { end = { col = 11, row = 12 }, start = { col = 10, row = 12 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 12, row = 12 }, start = { col = 11, row = 12 } } (Token (NumericLiteral "5"))
                , Located { end = { col = 13, row = 12 }, start = { col = 12, row = 12 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 14, row = 12 }, start = { col = 13, row = 12 } } (Token (Sigil (Operator Divide)))
                , Located { end = { col = 15, row = 12 }, start = { col = 14, row = 12 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 16, row = 12 }, start = { col = 15, row = 12 } } (Token (NumericLiteral "2"))
                , Located { end = { col = 17, row = 12 }, start = { col = 16, row = 12 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 18, row = 12 }, start = { col = 17, row = 12 } } (Token (Sigil (Operator Divide)))
                , Located { end = { col = 19, row = 12 }, start = { col = 18, row = 12 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 20, row = 12 }, start = { col = 19, row = 12 } } (Token (NumericLiteral "4"))
                , Located { end = { col = 21, row = 12 }, start = { col = 20, row = 12 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 22, row = 12 }, start = { col = 21, row = 12 } } (Token (Sigil (Operator Add)))
                , Located { end = { col = 23, row = 12 }, start = { col = 22, row = 12 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 24, row = 12 }, start = { col = 23, row = 12 } } (Token (NumericLiteral "5"))
                , Located { end = { col = 1, row = 14 }, start = { col = 24, row = 12 } }
                    (Newlines
                        [ 0
                        ]
                        0
                    )
                ]
      }
    , { name = "expression-int-subtract"
      , source = """a = 5 - 5

b = 78 + 5 + 2 - 4 + 5
"""
      , pretty = """
        ( ( Ok
          , ( ValueDeclaration
            , ( name, a )
            , ( args, () )
            , ( valueExpr__
              , ( Operator
                , ( ( op, - )
                  , ( lhs
                    , ( Int, 5 )
                    )
                  , ( rhs
                    , ( Int, 5 )
                    )
                  )
                )
              )
            )
          )
        , ( Ok
          , ( ValueDeclaration
            , ( name, b )
            , ( args, () )
            , ( valueExpr__
              , ( Operator
                , ( ( op, + )
                  , ( lhs
                    , ( Operator
                      , ( ( op, - )
                        , ( lhs
                          , ( Operator
                            , ( ( op, + )
                              , ( lhs
                                , ( Operator
                                  , ( ( op, + )
                                    , ( lhs
                                      , ( Int, 78 )
                                      )
                                    , ( rhs
                                      , ( Int, 5 )
                                      )
                                    )
                                  )
                                )
                              , ( rhs
                                , ( Int, 2 )
                                )
                              )
                            )
                          )
                        , ( rhs
                          , ( Int, 4 )
                          )
                        )
                      )
                    )
                  , ( rhs
                    , ( Int, 5 )
                    )
                  )
                )
              )
            )
          )
        )
"""
      , contextualized =
            Just
                [ Ok (ValueDeclaration { args = [], name = Located { end = { col = 2, row = 1 }, start = { col = 1, row = 1 } } (LowerCase "a"), valueExpr__ = Located { end = { col = 10, row = 1 }, start = { col = 5, row = 1 } } (Frontend.Operator (Located { end = { col = 8, row = 1 }, start = { col = 7, row = 1 } } Subtract) (Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Frontend.Int 5)) (Located { end = { col = 10, row = 1 }, start = { col = 9, row = 1 } } (Frontend.Int 5))) })
                , Ok (ValueDeclaration { args = [], name = Located { end = { col = 2, row = 3 }, start = { col = 1, row = 3 } } (LowerCase "b"), valueExpr__ = Located { end = { col = 23, row = 3 }, start = { col = 5, row = 3 } } (Frontend.Operator (Located { end = { col = 21, row = 3 }, start = { col = 20, row = 3 } } Add) (Located { end = { col = 19, row = 3 }, start = { col = 5, row = 3 } } (Frontend.Operator (Located { end = { col = 17, row = 3 }, start = { col = 16, row = 3 } } Subtract) (Located { end = { col = 15, row = 3 }, start = { col = 5, row = 3 } } (Frontend.Operator (Located { end = { col = 13, row = 3 }, start = { col = 12, row = 3 } } Add) (Located { end = { col = 11, row = 3 }, start = { col = 5, row = 3 } } (Frontend.Operator (Located { end = { col = 9, row = 3 }, start = { col = 8, row = 3 } } Add) (Located { end = { col = 7, row = 3 }, start = { col = 5, row = 3 } } (Frontend.Int 78)) (Located { end = { col = 11, row = 3 }, start = { col = 10, row = 3 } } (Frontend.Int 5)))) (Located { end = { col = 15, row = 3 }, start = { col = 14, row = 3 } } (Frontend.Int 2)))) (Located { end = { col = 19, row = 3 }, start = { col = 18, row = 3 } } (Frontend.Int 4)))) (Located { end = { col = 23, row = 3 }, start = { col = 22, row = 3 } } (Frontend.Int 5))) })
                ]
      , lexed =
            Ok
                [ Located { end = { col = 2, row = 1 }, start = { col = 1, row = 1 } } (Token (Identifier { name = TokenLowerCase (LowerCase "a"), qualifiers = [] }))
                , Located { end = { col = 3, row = 1 }, start = { col = 2, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 4, row = 1 }, start = { col = 3, row = 1 } } (Token (Sigil Assign))
                , Located { end = { col = 5, row = 1 }, start = { col = 4, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Token (NumericLiteral "5"))
                , Located { end = { col = 7, row = 1 }, start = { col = 6, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 8, row = 1 }, start = { col = 7, row = 1 } } (Token (Sigil (Operator Subtract)))
                , Located { end = { col = 9, row = 1 }, start = { col = 8, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 10, row = 1 }, start = { col = 9, row = 1 } } (Token (NumericLiteral "5"))
                , Located { end = { col = 1, row = 3 }, start = { col = 10, row = 1 } }
                    (Newlines
                        [ 0
                        ]
                        0
                    )
                , Located { end = { col = 2, row = 3 }, start = { col = 1, row = 3 } } (Token (Identifier { name = TokenLowerCase (LowerCase "b"), qualifiers = [] }))
                , Located { end = { col = 3, row = 3 }, start = { col = 2, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 4, row = 3 }, start = { col = 3, row = 3 } } (Token (Sigil Assign))
                , Located { end = { col = 5, row = 3 }, start = { col = 4, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 7, row = 3 }, start = { col = 5, row = 3 } } (Token (NumericLiteral "78"))
                , Located { end = { col = 8, row = 3 }, start = { col = 7, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 9, row = 3 }, start = { col = 8, row = 3 } } (Token (Sigil (Operator Add)))
                , Located { end = { col = 10, row = 3 }, start = { col = 9, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 11, row = 3 }, start = { col = 10, row = 3 } } (Token (NumericLiteral "5"))
                , Located { end = { col = 12, row = 3 }, start = { col = 11, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 13, row = 3 }, start = { col = 12, row = 3 } } (Token (Sigil (Operator Add)))
                , Located { end = { col = 14, row = 3 }, start = { col = 13, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 15, row = 3 }, start = { col = 14, row = 3 } } (Token (NumericLiteral "2"))
                , Located { end = { col = 16, row = 3 }, start = { col = 15, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 17, row = 3 }, start = { col = 16, row = 3 } } (Token (Sigil (Operator Subtract)))
                , Located { end = { col = 18, row = 3 }, start = { col = 17, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 19, row = 3 }, start = { col = 18, row = 3 } } (Token (NumericLiteral "4"))
                , Located { end = { col = 20, row = 3 }, start = { col = 19, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 21, row = 3 }, start = { col = 20, row = 3 } } (Token (Sigil (Operator Add)))
                , Located { end = { col = 22, row = 3 }, start = { col = 21, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 23, row = 3 }, start = { col = 22, row = 3 } } (Token (NumericLiteral "5"))
                , Located { end = { col = 1, row = 4 }, start = { col = 23, row = 3 } } (Newlines [] 0)
                ]
      }
    , { name = "type-alias"
      , source = """type alias Model = List Int
"""
      , pretty = """
        ( ( Ok
          , ( TypeAlias
            , ( ty, Model )
            , ( genericArgs, () )
            , ( expr
              , ( UserDefinedType
                , ( ( qualifiedness
                    , ( PossiblyQualified, Nothing )
                    )
                  , ( name, List )
                  , ( args
                    , ( ( UserDefinedType
                        , ( ( qualifiedness
                            , ( PossiblyQualified, Nothing )
                            )
                          , ( name, Int )
                          , ( args, () )
                          )
                        ) )
                    )
                  )
                )
              )
            )
          ) )
"""
      , contextualized =
            Just
                [ Ok
                    (TypeAlias
                        { expr =
                            UserDefinedType
                                { args =
                                    [ UserDefinedType
                                        { args = []
                                        , name = "Int"
                                        , qualifiedness = PossiblyQualified Nothing
                                        }
                                    ]
                                , name = "List"
                                , qualifiedness = PossiblyQualified Nothing
                                }
                        , genericArgs = []
                        , ty = UpperCase "Model"
                        }
                    )
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token (Keyword Type))
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token (Keyword Alias))
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 17, row = 1 }, start = { col = 12, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Model"), qualifiers = [] }))
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 19, row = 1 }, start = { col = 18, row = 1 } } (Token (Sigil Assign))
                , Located { end = { col = 20, row = 1 }, start = { col = 19, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 24, row = 1 }, start = { col = 20, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "List"), qualifiers = [] }))
                , Located { end = { col = 25, row = 1 }, start = { col = 24, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 28, row = 1 }, start = { col = 25, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Int"), qualifiers = [] }))
                , Located { end = { col = 1, row = 2 }, start = { col = 28, row = 1 } } (Newlines [] 0)
                ]
      }
    , { name = "type-alias-and-expression"
      , source = """type alias Model = List Int

expr hi = 77
"""
      , pretty = """
        ( ( Ok
          , ( TypeAlias
            , ( ty, Model )
            , ( genericArgs, () )
            , ( expr
              , ( UserDefinedType
                , ( ( qualifiedness
                    , ( PossiblyQualified, Nothing )
                    )
                  , ( name, List )
                  , ( args
                    , ( ( UserDefinedType
                        , ( ( qualifiedness
                            , ( PossiblyQualified, Nothing )
                            )
                          , ( name, Int )
                          , ( args, () )
                          )
                        ) )
                    )
                  )
                )
              )
            )
          )
        , ( Ok
          , ( ValueDeclaration
            , ( name, expr )
            , ( args
              , ( hi )
              )
            , ( valueExpr__
              , ( Int, 77 )
              )
            )
          )
        )
"""
      , contextualized =
            Just
                [ Ok
                    (TypeAlias
                        { expr =
                            UserDefinedType
                                { args =
                                    [ UserDefinedType
                                        { args = []
                                        , name = "Int"
                                        , qualifiedness = PossiblyQualified Nothing
                                        }
                                    ]
                                , name = "List"
                                , qualifiedness = PossiblyQualified Nothing
                                }
                        , genericArgs = []
                        , ty = UpperCase "Model"
                        }
                    )
                , Ok
                    (ValueDeclaration
                        { args =
                            [ Located { end = { col = 8, row = 3 }, start = { col = 6, row = 3 } } (LowerCase "hi")
                            ]
                        , name = Located { end = { col = 5, row = 3 }, start = { col = 1, row = 3 } } (LowerCase "expr")
                        , valueExpr__ = Located { end = { col = 13, row = 3 }, start = { col = 11, row = 3 } } (Frontend.Int 77)
                        }
                    )
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token (Keyword Type))
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token (Keyword Alias))
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 17, row = 1 }, start = { col = 12, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Model"), qualifiers = [] }))
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 19, row = 1 }, start = { col = 18, row = 1 } } (Token (Sigil Assign))
                , Located { end = { col = 20, row = 1 }, start = { col = 19, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 24, row = 1 }, start = { col = 20, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "List"), qualifiers = [] }))
                , Located { end = { col = 25, row = 1 }, start = { col = 24, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 28, row = 1 }, start = { col = 25, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Int"), qualifiers = [] }))
                , Located { end = { col = 1, row = 3 }, start = { col = 28, row = 1 } }
                    (Newlines
                        [ 0
                        ]
                        0
                    )
                , Located { end = { col = 5, row = 3 }, start = { col = 1, row = 3 } } (Token (Identifier { name = TokenLowerCase (LowerCase "expr"), qualifiers = [] }))
                , Located { end = { col = 6, row = 3 }, start = { col = 5, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 8, row = 3 }, start = { col = 6, row = 3 } } (Token (Identifier { name = TokenLowerCase (LowerCase "hi"), qualifiers = [] }))
                , Located { end = { col = 9, row = 3 }, start = { col = 8, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 10, row = 3 }, start = { col = 9, row = 3 } } (Token (Sigil Assign))
                , Located { end = { col = 11, row = 3 }, start = { col = 10, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 13, row = 3 }, start = { col = 11, row = 3 } } (Token (NumericLiteral "77"))
                , Located { end = { col = 1, row = 4 }, start = { col = 13, row = 3 } } (Newlines [] 0)
                ]
      }
    , { name = "type-alias-bracket-in-record"
      , source = """type alias Ty = { hi: (Int) }
"""
      , pretty = """
        ( ( Ok
          , ( TypeAlias
            , ( ty, Ty )
            , ( genericArgs, () )
            , ( expr
              , ( Record
                , ( ( hi
                    , ( UserDefinedType
                      , ( ( qualifiedness
                          , ( PossiblyQualified, Nothing )
                          )
                        , ( name, Int )
                        , ( args, () )
                        )
                      )
                    ) )
                )
              )
            )
          ) )
"""
      , contextualized =
            Just
                [ Ok
                    (TypeAlias
                        { expr =
                            Record
                                (Dict.fromList
                                    [ ( "hi"
                                      , UserDefinedType
                                            { args = []
                                            , name = "Int"
                                            , qualifiedness = PossiblyQualified Nothing
                                            }
                                      )
                                    ]
                                )
                        , genericArgs = []
                        , ty = UpperCase "Ty"
                        }
                    )
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token (Keyword Type))
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token (Keyword Alias))
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Ty"), qualifiers = [] }))
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Token (Sigil Assign))
                , Located { end = { col = 17, row = 1 }, start = { col = 16, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Token (Sigil (Bracket Curly Open)))
                , Located { end = { col = 19, row = 1 }, start = { col = 18, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 21, row = 1 }, start = { col = 19, row = 1 } } (Token (Identifier { name = TokenLowerCase (LowerCase "hi"), qualifiers = [] }))
                , Located { end = { col = 22, row = 1 }, start = { col = 21, row = 1 } } (Token (Sigil Colon))
                , Located { end = { col = 23, row = 1 }, start = { col = 22, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 24, row = 1 }, start = { col = 23, row = 1 } } (Token (Sigil (Bracket Round Open)))
                , Located { end = { col = 27, row = 1 }, start = { col = 24, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Int"), qualifiers = [] }))
                , Located { end = { col = 28, row = 1 }, start = { col = 27, row = 1 } } (Token (Sigil (Bracket Round Close)))
                , Located { end = { col = 29, row = 1 }, start = { col = 28, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 30, row = 1 }, start = { col = 29, row = 1 } } (Token (Sigil (Bracket Curly Close)))
                , Located { end = { col = 1, row = 2 }, start = { col = 30, row = 1 } } (Newlines [] 0)
                ]
      }
    , { name = "type-alias-function"
      , source = """type alias Function = List Int -> List (List Int)
"""
      , pretty = """
        ( ( Ok
          , ( TypeAlias
            , ( ty, Function )
            , ( genericArgs, () )
            , ( expr
              , ( Function
                , ( ( from
                    , ( UserDefinedType
                      , ( ( qualifiedness
                          , ( PossiblyQualified, Nothing )
                          )
                        , ( name, List )
                        , ( args
                          , ( ( UserDefinedType
                              , ( ( qualifiedness
                                  , ( PossiblyQualified, Nothing )
                                  )
                                , ( name, Int )
                                , ( args, () )
                                )
                              ) )
                          )
                        )
                      )
                    )
                  , ( to
                    , ( UserDefinedType
                      , ( ( qualifiedness
                          , ( PossiblyQualified, Nothing )
                          )
                        , ( name, List )
                        , ( args
                          , ( ( UserDefinedType
                              , ( ( qualifiedness
                                  , ( PossiblyQualified, Nothing )
                                  )
                                , ( name, List )
                                , ( args
                                  , ( ( UserDefinedType
                                      , ( ( qualifiedness
                                          , ( PossiblyQualified, Nothing )
                                          )
                                        , ( name, Int )
                                        , ( args, () )
                                        )
                                      ) )
                                  )
                                )
                              ) )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          ) )
"""
      , contextualized =
            Just
                [ Ok
                    (TypeAlias
                        { expr =
                            Function
                                { from =
                                    UserDefinedType
                                        { args =
                                            [ UserDefinedType
                                                { args = []
                                                , name = "Int"
                                                , qualifiedness = PossiblyQualified Nothing
                                                }
                                            ]
                                        , name = "List"
                                        , qualifiedness = PossiblyQualified Nothing
                                        }
                                , to =
                                    UserDefinedType
                                        { args =
                                            [ UserDefinedType
                                                { args =
                                                    [ UserDefinedType
                                                        { args = []
                                                        , name = "Int"
                                                        , qualifiedness = PossiblyQualified Nothing
                                                        }
                                                    ]
                                                , name = "List"
                                                , qualifiedness = PossiblyQualified Nothing
                                                }
                                            ]
                                        , name = "List"
                                        , qualifiedness = PossiblyQualified Nothing
                                        }
                                }
                        , genericArgs = []
                        , ty = UpperCase "Function"
                        }
                    )
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token (Keyword Type))
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token (Keyword Alias))
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 20, row = 1 }, start = { col = 12, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Function"), qualifiers = [] }))
                , Located { end = { col = 21, row = 1 }, start = { col = 20, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 22, row = 1 }, start = { col = 21, row = 1 } } (Token (Sigil Assign))
                , Located { end = { col = 23, row = 1 }, start = { col = 22, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 27, row = 1 }, start = { col = 23, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "List"), qualifiers = [] }))
                , Located { end = { col = 28, row = 1 }, start = { col = 27, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 31, row = 1 }, start = { col = 28, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Int"), qualifiers = [] }))
                , Located { end = { col = 32, row = 1 }, start = { col = 31, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 34, row = 1 }, start = { col = 32, row = 1 } } (Token (Sigil ThinArrow))
                , Located { end = { col = 35, row = 1 }, start = { col = 34, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 39, row = 1 }, start = { col = 35, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "List"), qualifiers = [] }))
                , Located { end = { col = 40, row = 1 }, start = { col = 39, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 41, row = 1 }, start = { col = 40, row = 1 } } (Token (Sigil (Bracket Round Open)))
                , Located { end = { col = 45, row = 1 }, start = { col = 41, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "List"), qualifiers = [] }))
                , Located { end = { col = 46, row = 1 }, start = { col = 45, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 49, row = 1 }, start = { col = 46, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Int"), qualifiers = [] }))
                , Located { end = { col = 50, row = 1 }, start = { col = 49, row = 1 } } (Token (Sigil (Bracket Round Close)))
                , Located { end = { col = 1, row = 2 }, start = { col = 50, row = 1 } } (Newlines [] 0)
                ]
      }
    , { name = "type-alias-function-binding-order"
      , source = """type alias Function = A -> B -> C
type alias Function = A -> B -> C -> D
"""
      , pretty = """
        ( ( Ok
          , ( TypeAlias
            , ( ty, Function )
            , ( genericArgs, () )
            , ( expr
              , ( Function
                , ( ( from
                    , ( UserDefinedType
                      , ( ( qualifiedness
                          , ( PossiblyQualified, Nothing )
                          )
                        , ( name, A )
                        , ( args, () )
                        )
                      )
                    )
                  , ( to
                    , ( Function
                      , ( ( from
                          , ( UserDefinedType
                            , ( ( qualifiedness
                                , ( PossiblyQualified, Nothing )
                                )
                              , ( name, B )
                              , ( args, () )
                              )
                            )
                          )
                        , ( to
                          , ( UserDefinedType
                            , ( ( qualifiedness
                                , ( PossiblyQualified, Nothing )
                                )
                              , ( name, C )
                              , ( args, () )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        , ( Ok
          , ( TypeAlias
            , ( ty, Function )
            , ( genericArgs, () )
            , ( expr
              , ( Function
                , ( ( from
                    , ( UserDefinedType
                      , ( ( qualifiedness
                          , ( PossiblyQualified, Nothing )
                          )
                        , ( name, A )
                        , ( args, () )
                        )
                      )
                    )
                  , ( to
                    , ( Function
                      , ( ( from
                          , ( UserDefinedType
                            , ( ( qualifiedness
                                , ( PossiblyQualified, Nothing )
                                )
                              , ( name, B )
                              , ( args, () )
                              )
                            )
                          )
                        , ( to
                          , ( Function
                            , ( ( from
                                , ( UserDefinedType
                                  , ( ( qualifiedness
                                      , ( PossiblyQualified, Nothing )
                                      )
                                    , ( name, C )
                                    , ( args, () )
                                    )
                                  )
                                )
                              , ( to
                                , ( UserDefinedType
                                  , ( ( qualifiedness
                                      , ( PossiblyQualified, Nothing )
                                      )
                                    , ( name, D )
                                    , ( args, () )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
"""
      , contextualized =
            Just
                [ Ok
                    (TypeAlias
                        { expr =
                            Function
                                { from =
                                    UserDefinedType
                                        { args = []
                                        , name = "A"
                                        , qualifiedness = PossiblyQualified Nothing
                                        }
                                , to =
                                    Function
                                        { from =
                                            UserDefinedType
                                                { args = []
                                                , name = "B"
                                                , qualifiedness = PossiblyQualified Nothing
                                                }
                                        , to =
                                            UserDefinedType
                                                { args = []
                                                , name = "C"
                                                , qualifiedness = PossiblyQualified Nothing
                                                }
                                        }
                                }
                        , genericArgs = []
                        , ty = UpperCase "Function"
                        }
                    )
                , Ok
                    (TypeAlias
                        { expr =
                            Function
                                { from =
                                    UserDefinedType
                                        { args = []
                                        , name = "A"
                                        , qualifiedness = PossiblyQualified Nothing
                                        }
                                , to =
                                    Function
                                        { from =
                                            UserDefinedType
                                                { args = []
                                                , name = "B"
                                                , qualifiedness = PossiblyQualified Nothing
                                                }
                                        , to =
                                            Function
                                                { from =
                                                    UserDefinedType
                                                        { args = []
                                                        , name = "C"
                                                        , qualifiedness = PossiblyQualified Nothing
                                                        }
                                                , to =
                                                    UserDefinedType
                                                        { args = []
                                                        , name = "D"
                                                        , qualifiedness = PossiblyQualified Nothing
                                                        }
                                                }
                                        }
                                }
                        , genericArgs = []
                        , ty = UpperCase "Function"
                        }
                    )
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token (Keyword Type))
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token (Keyword Alias))
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 20, row = 1 }, start = { col = 12, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Function"), qualifiers = [] }))
                , Located { end = { col = 21, row = 1 }, start = { col = 20, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 22, row = 1 }, start = { col = 21, row = 1 } } (Token (Sigil Assign))
                , Located { end = { col = 23, row = 1 }, start = { col = 22, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 24, row = 1 }, start = { col = 23, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "A"), qualifiers = [] }))
                , Located { end = { col = 25, row = 1 }, start = { col = 24, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 27, row = 1 }, start = { col = 25, row = 1 } } (Token (Sigil ThinArrow))
                , Located { end = { col = 28, row = 1 }, start = { col = 27, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 29, row = 1 }, start = { col = 28, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "B"), qualifiers = [] }))
                , Located { end = { col = 30, row = 1 }, start = { col = 29, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 32, row = 1 }, start = { col = 30, row = 1 } } (Token (Sigil ThinArrow))
                , Located { end = { col = 33, row = 1 }, start = { col = 32, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 34, row = 1 }, start = { col = 33, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "C"), qualifiers = [] }))
                , Located { end = { col = 1, row = 2 }, start = { col = 34, row = 1 } } (Newlines [] 0)
                , Located { end = { col = 5, row = 2 }, start = { col = 1, row = 2 } } (Token (Keyword Type))
                , Located { end = { col = 6, row = 2 }, start = { col = 5, row = 2 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 11, row = 2 }, start = { col = 6, row = 2 } } (Token (Keyword Alias))
                , Located { end = { col = 12, row = 2 }, start = { col = 11, row = 2 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 20, row = 2 }, start = { col = 12, row = 2 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Function"), qualifiers = [] }))
                , Located { end = { col = 21, row = 2 }, start = { col = 20, row = 2 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 22, row = 2 }, start = { col = 21, row = 2 } } (Token (Sigil Assign))
                , Located { end = { col = 23, row = 2 }, start = { col = 22, row = 2 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 24, row = 2 }, start = { col = 23, row = 2 } } (Token (Identifier { name = TokenUpperCase (UpperCase "A"), qualifiers = [] }))
                , Located { end = { col = 25, row = 2 }, start = { col = 24, row = 2 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 27, row = 2 }, start = { col = 25, row = 2 } } (Token (Sigil ThinArrow))
                , Located { end = { col = 28, row = 2 }, start = { col = 27, row = 2 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 29, row = 2 }, start = { col = 28, row = 2 } } (Token (Identifier { name = TokenUpperCase (UpperCase "B"), qualifiers = [] }))
                , Located { end = { col = 30, row = 2 }, start = { col = 29, row = 2 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 32, row = 2 }, start = { col = 30, row = 2 } } (Token (Sigil ThinArrow))
                , Located { end = { col = 33, row = 2 }, start = { col = 32, row = 2 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 34, row = 2 }, start = { col = 33, row = 2 } } (Token (Identifier { name = TokenUpperCase (UpperCase "C"), qualifiers = [] }))
                , Located { end = { col = 35, row = 2 }, start = { col = 34, row = 2 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 37, row = 2 }, start = { col = 35, row = 2 } } (Token (Sigil ThinArrow))
                , Located { end = { col = 38, row = 2 }, start = { col = 37, row = 2 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 39, row = 2 }, start = { col = 38, row = 2 } } (Token (Identifier { name = TokenUpperCase (UpperCase "D"), qualifiers = [] }))
                , Located { end = { col = 1, row = 3 }, start = { col = 39, row = 2 } } (Newlines [] 0)
                ]
      }
    , { name = "type-alias-function-generic"
      , source = """type alias Function a = List Int -> List (List a)

type alias Function b c = b -> c"""
      , pretty = """
        ( ( Ok
          , ( TypeAlias
            , ( ty, Function )
            , ( genericArgs
              , ( a )
              )
            , ( expr
              , ( Function
                , ( ( from
                    , ( UserDefinedType
                      , ( ( qualifiedness
                          , ( PossiblyQualified, Nothing )
                          )
                        , ( name, List )
                        , ( args
                          , ( ( UserDefinedType
                              , ( ( qualifiedness
                                  , ( PossiblyQualified, Nothing )
                                  )
                                , ( name, Int )
                                , ( args, () )
                                )
                              ) )
                          )
                        )
                      )
                    )
                  , ( to
                    , ( UserDefinedType
                      , ( ( qualifiedness
                          , ( PossiblyQualified, Nothing )
                          )
                        , ( name, List )
                        , ( args
                          , ( ( UserDefinedType
                              , ( ( qualifiedness
                                  , ( PossiblyQualified, Nothing )
                                  )
                                , ( name, List )
                                , ( args
                                  , ( ( UserDefinedType
                                      , ( ( qualifiedness
                                          , ( PossiblyQualified, Nothing )
                                          )
                                        , ( name, a )
                                        , ( args, () )
                                        )
                                      ) )
                                  )
                                )
                              ) )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        , ( Ok
          , ( TypeAlias
            , ( ty, Function )
            , ( genericArgs
              , ( b, c )
              )
            , ( expr
              , ( Function
                , ( ( from
                    , ( UserDefinedType
                      , ( ( qualifiedness
                          , ( PossiblyQualified, Nothing )
                          )
                        , ( name, b )
                        , ( args, () )
                        )
                      )
                    )
                  , ( to
                    , ( UserDefinedType
                      , ( ( qualifiedness
                          , ( PossiblyQualified, Nothing )
                          )
                        , ( name, c )
                        , ( args, () )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
"""
      , contextualized =
            Just
                [ Ok
                    (TypeAlias
                        { expr =
                            Function
                                { from =
                                    UserDefinedType
                                        { args =
                                            [ UserDefinedType
                                                { args = []
                                                , name = "Int"
                                                , qualifiedness = PossiblyQualified Nothing
                                                }
                                            ]
                                        , name = "List"
                                        , qualifiedness = PossiblyQualified Nothing
                                        }
                                , to =
                                    UserDefinedType
                                        { args =
                                            [ UserDefinedType
                                                { args =
                                                    [ UserDefinedType
                                                        { args = []
                                                        , name = "a"
                                                        , qualifiedness = PossiblyQualified Nothing
                                                        }
                                                    ]
                                                , name = "List"
                                                , qualifiedness = PossiblyQualified Nothing
                                                }
                                            ]
                                        , name = "List"
                                        , qualifiedness = PossiblyQualified Nothing
                                        }
                                }
                        , genericArgs =
                            [ LowerCase "a"
                            ]
                        , ty = UpperCase "Function"
                        }
                    )
                , Ok
                    (TypeAlias
                        { expr =
                            Function
                                { from =
                                    UserDefinedType
                                        { args = []
                                        , name = "b"
                                        , qualifiedness = PossiblyQualified Nothing
                                        }
                                , to =
                                    UserDefinedType
                                        { args = []
                                        , name = "c"
                                        , qualifiedness = PossiblyQualified Nothing
                                        }
                                }
                        , genericArgs =
                            [ LowerCase "b"
                            , LowerCase "c"
                            ]
                        , ty = UpperCase "Function"
                        }
                    )
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token (Keyword Type))
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token (Keyword Alias))
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 20, row = 1 }, start = { col = 12, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Function"), qualifiers = [] }))
                , Located { end = { col = 21, row = 1 }, start = { col = 20, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 22, row = 1 }, start = { col = 21, row = 1 } } (Token (Identifier { name = TokenLowerCase (LowerCase "a"), qualifiers = [] }))
                , Located { end = { col = 23, row = 1 }, start = { col = 22, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 24, row = 1 }, start = { col = 23, row = 1 } } (Token (Sigil Assign))
                , Located { end = { col = 25, row = 1 }, start = { col = 24, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 29, row = 1 }, start = { col = 25, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "List"), qualifiers = [] }))
                , Located { end = { col = 30, row = 1 }, start = { col = 29, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 33, row = 1 }, start = { col = 30, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Int"), qualifiers = [] }))
                , Located { end = { col = 34, row = 1 }, start = { col = 33, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 36, row = 1 }, start = { col = 34, row = 1 } } (Token (Sigil ThinArrow))
                , Located { end = { col = 37, row = 1 }, start = { col = 36, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 41, row = 1 }, start = { col = 37, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "List"), qualifiers = [] }))
                , Located { end = { col = 42, row = 1 }, start = { col = 41, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 43, row = 1 }, start = { col = 42, row = 1 } } (Token (Sigil (Bracket Round Open)))
                , Located { end = { col = 47, row = 1 }, start = { col = 43, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "List"), qualifiers = [] }))
                , Located { end = { col = 48, row = 1 }, start = { col = 47, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 49, row = 1 }, start = { col = 48, row = 1 } } (Token (Identifier { name = TokenLowerCase (LowerCase "a"), qualifiers = [] }))
                , Located { end = { col = 50, row = 1 }, start = { col = 49, row = 1 } } (Token (Sigil (Bracket Round Close)))
                , Located { end = { col = 1, row = 3 }, start = { col = 50, row = 1 } }
                    (Newlines
                        [ 0
                        ]
                        0
                    )
                , Located { end = { col = 5, row = 3 }, start = { col = 1, row = 3 } } (Token (Keyword Type))
                , Located { end = { col = 6, row = 3 }, start = { col = 5, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 11, row = 3 }, start = { col = 6, row = 3 } } (Token (Keyword Alias))
                , Located { end = { col = 12, row = 3 }, start = { col = 11, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 20, row = 3 }, start = { col = 12, row = 3 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Function"), qualifiers = [] }))
                , Located { end = { col = 21, row = 3 }, start = { col = 20, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 22, row = 3 }, start = { col = 21, row = 3 } } (Token (Identifier { name = TokenLowerCase (LowerCase "b"), qualifiers = [] }))
                , Located { end = { col = 23, row = 3 }, start = { col = 22, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 24, row = 3 }, start = { col = 23, row = 3 } } (Token (Identifier { name = TokenLowerCase (LowerCase "c"), qualifiers = [] }))
                , Located { end = { col = 25, row = 3 }, start = { col = 24, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 26, row = 3 }, start = { col = 25, row = 3 } } (Token (Sigil Assign))
                , Located { end = { col = 27, row = 3 }, start = { col = 26, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 28, row = 3 }, start = { col = 27, row = 3 } } (Token (Identifier { name = TokenLowerCase (LowerCase "b"), qualifiers = [] }))
                , Located { end = { col = 29, row = 3 }, start = { col = 28, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 31, row = 3 }, start = { col = 29, row = 3 } } (Token (Sigil ThinArrow))
                , Located { end = { col = 32, row = 3 }, start = { col = 31, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 33, row = 3 }, start = { col = 32, row = 3 } } (Token (Identifier { name = TokenLowerCase (LowerCase "c"), qualifiers = [] }))
                ]
      }
    , { name = "type-alias-function-nested"
      , source = """type alias Function = (() -> (Int, String))

type alias Function2 = { a: () -> (Int, String) }

type alias Function3 = (() -> (Int, String), ())

type alias Function3 = (Int, () -> (Int, String), ())
"""
      , pretty = """
        ( ( Ok
          , ( TypeAlias
            , ( ty, Function )
            , ( genericArgs, () )
            , ( expr
              , ( Function
                , ( ( from, Unit )
                  , ( to
                    , ( Tuple
                      , ( ( UserDefinedType
                          , ( ( qualifiedness
                              , ( PossiblyQualified, Nothing )
                              )
                            , ( name, Int )
                            , ( args, () )
                            )
                          )
                        , ( UserDefinedType
                          , ( ( qualifiedness
                              , ( PossiblyQualified, Nothing )
                              )
                            , ( name, String )
                            , ( args, () )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        , ( Ok
          , ( TypeAlias
            , ( ty, Function2 )
            , ( genericArgs, () )
            , ( expr
              , ( Record
                , ( ( a
                    , ( Function
                      , ( ( from, Unit )
                        , ( to
                          , ( Tuple
                            , ( ( UserDefinedType
                                , ( ( qualifiedness
                                    , ( PossiblyQualified, Nothing )
                                    )
                                  , ( name, Int )
                                  , ( args, () )
                                  )
                                )
                              , ( UserDefinedType
                                , ( ( qualifiedness
                                    , ( PossiblyQualified, Nothing )
                                    )
                                  , ( name, String )
                                  , ( args, () )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    ) )
                )
              )
            )
          )
        , ( Ok
          , ( TypeAlias
            , ( ty, Function3 )
            , ( genericArgs, () )
            , ( expr
              , ( Tuple
                , ( ( Function
                    , ( ( from, Unit )
                      , ( to
                        , ( Tuple
                          , ( ( UserDefinedType
                              , ( ( qualifiedness
                                  , ( PossiblyQualified, Nothing )
                                  )
                                , ( name, Int )
                                , ( args, () )
                                )
                              )
                            , ( UserDefinedType
                              , ( ( qualifiedness
                                  , ( PossiblyQualified, Nothing )
                                  )
                                , ( name, String )
                                , ( args, () )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  , Unit
                  )
                )
              )
            )
          )
        , ( Ok
          , ( TypeAlias
            , ( ty, Function3 )
            , ( genericArgs, () )
            , ( expr
              , ( Tuple
                , ( ( UserDefinedType
                    , ( ( qualifiedness
                        , ( PossiblyQualified, Nothing )
                        )
                      , ( name, Int )
                      , ( args, () )
                      )
                    )
                  , ( Function
                    , ( ( from, Unit )
                      , ( to
                        , ( Tuple
                          , ( ( UserDefinedType
                              , ( ( qualifiedness
                                  , ( PossiblyQualified, Nothing )
                                  )
                                , ( name, Int )
                                , ( args, () )
                                )
                              )
                            , ( UserDefinedType
                              , ( ( qualifiedness
                                  , ( PossiblyQualified, Nothing )
                                  )
                                , ( name, String )
                                , ( args, () )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  , Unit
                  )
                )
              )
            )
          )
        )
"""
      , contextualized =
            Just
                [ Ok
                    (TypeAlias
                        { expr =
                            Function
                                { from = Unit
                                , to =
                                    Tuple
                                        (UserDefinedType
                                            { args = []
                                            , name = "Int"
                                            , qualifiedness = PossiblyQualified Nothing
                                            }
                                        )
                                        (UserDefinedType
                                            { args = []
                                            , name = "String"
                                            , qualifiedness = PossiblyQualified Nothing
                                            }
                                        )
                                }
                        , genericArgs = []
                        , ty = UpperCase "Function"
                        }
                    )
                , Ok
                    (TypeAlias
                        { expr =
                            Record
                                (Dict.fromList
                                    [ ( "a"
                                      , Function
                                            { from = Unit
                                            , to =
                                                Tuple
                                                    (UserDefinedType
                                                        { args = []
                                                        , name = "Int"
                                                        , qualifiedness = PossiblyQualified Nothing
                                                        }
                                                    )
                                                    (UserDefinedType
                                                        { args = []
                                                        , name = "String"
                                                        , qualifiedness = PossiblyQualified Nothing
                                                        }
                                                    )
                                            }
                                      )
                                    ]
                                )
                        , genericArgs = []
                        , ty = UpperCase "Function2"
                        }
                    )
                , Ok
                    (TypeAlias
                        { expr =
                            Tuple
                                (Function
                                    { from = Unit
                                    , to =
                                        Tuple
                                            (UserDefinedType
                                                { args = []
                                                , name = "Int"
                                                , qualifiedness = PossiblyQualified Nothing
                                                }
                                            )
                                            (UserDefinedType
                                                { args = []
                                                , name = "String"
                                                , qualifiedness = PossiblyQualified Nothing
                                                }
                                            )
                                    }
                                )
                                Unit
                        , genericArgs = []
                        , ty = UpperCase "Function3"
                        }
                    )
                , Ok
                    (TypeAlias
                        { expr =
                            Tuple3
                                (UserDefinedType
                                    { args = []
                                    , name = "Int"
                                    , qualifiedness = PossiblyQualified Nothing
                                    }
                                )
                                (Function
                                    { from = Unit
                                    , to =
                                        Tuple
                                            (UserDefinedType
                                                { args = []
                                                , name = "Int"
                                                , qualifiedness = PossiblyQualified Nothing
                                                }
                                            )
                                            (UserDefinedType
                                                { args = []
                                                , name = "String"
                                                , qualifiedness = PossiblyQualified Nothing
                                                }
                                            )
                                    }
                                )
                                Unit
                        , genericArgs = []
                        , ty = UpperCase "Function3"
                        }
                    )
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token (Keyword Type))
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token (Keyword Alias))
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 20, row = 1 }, start = { col = 12, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Function"), qualifiers = [] }))
                , Located { end = { col = 21, row = 1 }, start = { col = 20, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 22, row = 1 }, start = { col = 21, row = 1 } } (Token (Sigil Assign))
                , Located { end = { col = 23, row = 1 }, start = { col = 22, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 24, row = 1 }, start = { col = 23, row = 1 } } (Token (Sigil (Bracket Round Open)))
                , Located { end = { col = 25, row = 1 }, start = { col = 24, row = 1 } } (Token (Sigil (Bracket Round Open)))
                , Located { end = { col = 26, row = 1 }, start = { col = 25, row = 1 } } (Token (Sigil (Bracket Round Close)))
                , Located { end = { col = 27, row = 1 }, start = { col = 26, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 29, row = 1 }, start = { col = 27, row = 1 } } (Token (Sigil ThinArrow))
                , Located { end = { col = 30, row = 1 }, start = { col = 29, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 31, row = 1 }, start = { col = 30, row = 1 } } (Token (Sigil (Bracket Round Open)))
                , Located { end = { col = 34, row = 1 }, start = { col = 31, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Int"), qualifiers = [] }))
                , Located { end = { col = 35, row = 1 }, start = { col = 34, row = 1 } } (Token (Sigil Comma))
                , Located { end = { col = 36, row = 1 }, start = { col = 35, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 42, row = 1 }, start = { col = 36, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "String"), qualifiers = [] }))
                , Located { end = { col = 43, row = 1 }, start = { col = 42, row = 1 } } (Token (Sigil (Bracket Round Close)))
                , Located { end = { col = 44, row = 1 }, start = { col = 43, row = 1 } } (Token (Sigil (Bracket Round Close)))
                , Located { end = { col = 1, row = 3 }, start = { col = 44, row = 1 } }
                    (Newlines
                        [ 0
                        ]
                        0
                    )
                , Located { end = { col = 5, row = 3 }, start = { col = 1, row = 3 } } (Token (Keyword Type))
                , Located { end = { col = 6, row = 3 }, start = { col = 5, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 11, row = 3 }, start = { col = 6, row = 3 } } (Token (Keyword Alias))
                , Located { end = { col = 12, row = 3 }, start = { col = 11, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 21, row = 3 }, start = { col = 12, row = 3 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Function2"), qualifiers = [] }))
                , Located { end = { col = 22, row = 3 }, start = { col = 21, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 23, row = 3 }, start = { col = 22, row = 3 } } (Token (Sigil Assign))
                , Located { end = { col = 24, row = 3 }, start = { col = 23, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 25, row = 3 }, start = { col = 24, row = 3 } } (Token (Sigil (Bracket Curly Open)))
                , Located { end = { col = 26, row = 3 }, start = { col = 25, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 27, row = 3 }, start = { col = 26, row = 3 } } (Token (Identifier { name = TokenLowerCase (LowerCase "a"), qualifiers = [] }))
                , Located { end = { col = 28, row = 3 }, start = { col = 27, row = 3 } } (Token (Sigil Colon))
                , Located { end = { col = 29, row = 3 }, start = { col = 28, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 30, row = 3 }, start = { col = 29, row = 3 } } (Token (Sigil (Bracket Round Open)))
                , Located { end = { col = 31, row = 3 }, start = { col = 30, row = 3 } } (Token (Sigil (Bracket Round Close)))
                , Located { end = { col = 32, row = 3 }, start = { col = 31, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 34, row = 3 }, start = { col = 32, row = 3 } } (Token (Sigil ThinArrow))
                , Located { end = { col = 35, row = 3 }, start = { col = 34, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 36, row = 3 }, start = { col = 35, row = 3 } } (Token (Sigil (Bracket Round Open)))
                , Located { end = { col = 39, row = 3 }, start = { col = 36, row = 3 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Int"), qualifiers = [] }))
                , Located { end = { col = 40, row = 3 }, start = { col = 39, row = 3 } } (Token (Sigil Comma))
                , Located { end = { col = 41, row = 3 }, start = { col = 40, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 47, row = 3 }, start = { col = 41, row = 3 } } (Token (Identifier { name = TokenUpperCase (UpperCase "String"), qualifiers = [] }))
                , Located { end = { col = 48, row = 3 }, start = { col = 47, row = 3 } } (Token (Sigil (Bracket Round Close)))
                , Located { end = { col = 49, row = 3 }, start = { col = 48, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 50, row = 3 }, start = { col = 49, row = 3 } } (Token (Sigil (Bracket Curly Close)))
                , Located { end = { col = 1, row = 5 }, start = { col = 50, row = 3 } }
                    (Newlines
                        [ 0
                        ]
                        0
                    )
                , Located { end = { col = 5, row = 5 }, start = { col = 1, row = 5 } } (Token (Keyword Type))
                , Located { end = { col = 6, row = 5 }, start = { col = 5, row = 5 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 11, row = 5 }, start = { col = 6, row = 5 } } (Token (Keyword Alias))
                , Located { end = { col = 12, row = 5 }, start = { col = 11, row = 5 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 21, row = 5 }, start = { col = 12, row = 5 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Function3"), qualifiers = [] }))
                , Located { end = { col = 22, row = 5 }, start = { col = 21, row = 5 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 23, row = 5 }, start = { col = 22, row = 5 } } (Token (Sigil Assign))
                , Located { end = { col = 24, row = 5 }, start = { col = 23, row = 5 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 25, row = 5 }, start = { col = 24, row = 5 } } (Token (Sigil (Bracket Round Open)))
                , Located { end = { col = 26, row = 5 }, start = { col = 25, row = 5 } } (Token (Sigil (Bracket Round Open)))
                , Located { end = { col = 27, row = 5 }, start = { col = 26, row = 5 } } (Token (Sigil (Bracket Round Close)))
                , Located { end = { col = 28, row = 5 }, start = { col = 27, row = 5 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 30, row = 5 }, start = { col = 28, row = 5 } } (Token (Sigil ThinArrow))
                , Located { end = { col = 31, row = 5 }, start = { col = 30, row = 5 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 32, row = 5 }, start = { col = 31, row = 5 } } (Token (Sigil (Bracket Round Open)))
                , Located { end = { col = 35, row = 5 }, start = { col = 32, row = 5 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Int"), qualifiers = [] }))
                , Located { end = { col = 36, row = 5 }, start = { col = 35, row = 5 } } (Token (Sigil Comma))
                , Located { end = { col = 37, row = 5 }, start = { col = 36, row = 5 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 43, row = 5 }, start = { col = 37, row = 5 } } (Token (Identifier { name = TokenUpperCase (UpperCase "String"), qualifiers = [] }))
                , Located { end = { col = 44, row = 5 }, start = { col = 43, row = 5 } } (Token (Sigil (Bracket Round Close)))
                , Located { end = { col = 45, row = 5 }, start = { col = 44, row = 5 } } (Token (Sigil Comma))
                , Located { end = { col = 46, row = 5 }, start = { col = 45, row = 5 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 47, row = 5 }, start = { col = 46, row = 5 } } (Token (Sigil (Bracket Round Open)))
                , Located { end = { col = 48, row = 5 }, start = { col = 47, row = 5 } } (Token (Sigil (Bracket Round Close)))
                , Located { end = { col = 49, row = 5 }, start = { col = 48, row = 5 } } (Token (Sigil (Bracket Round Close)))
                , Located { end = { col = 1, row = 7 }, start = { col = 49, row = 5 } }
                    (Newlines
                        [ 0
                        ]
                        0
                    )
                , Located { end = { col = 5, row = 7 }, start = { col = 1, row = 7 } } (Token (Keyword Type))
                , Located { end = { col = 6, row = 7 }, start = { col = 5, row = 7 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 11, row = 7 }, start = { col = 6, row = 7 } } (Token (Keyword Alias))
                , Located { end = { col = 12, row = 7 }, start = { col = 11, row = 7 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 21, row = 7 }, start = { col = 12, row = 7 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Function3"), qualifiers = [] }))
                , Located { end = { col = 22, row = 7 }, start = { col = 21, row = 7 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 23, row = 7 }, start = { col = 22, row = 7 } } (Token (Sigil Assign))
                , Located { end = { col = 24, row = 7 }, start = { col = 23, row = 7 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 25, row = 7 }, start = { col = 24, row = 7 } } (Token (Sigil (Bracket Round Open)))
                , Located { end = { col = 28, row = 7 }, start = { col = 25, row = 7 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Int"), qualifiers = [] }))
                , Located { end = { col = 29, row = 7 }, start = { col = 28, row = 7 } } (Token (Sigil Comma))
                , Located { end = { col = 30, row = 7 }, start = { col = 29, row = 7 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 31, row = 7 }, start = { col = 30, row = 7 } } (Token (Sigil (Bracket Round Open)))
                , Located { end = { col = 32, row = 7 }, start = { col = 31, row = 7 } } (Token (Sigil (Bracket Round Close)))
                , Located { end = { col = 33, row = 7 }, start = { col = 32, row = 7 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 35, row = 7 }, start = { col = 33, row = 7 } } (Token (Sigil ThinArrow))
                , Located { end = { col = 36, row = 7 }, start = { col = 35, row = 7 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 37, row = 7 }, start = { col = 36, row = 7 } } (Token (Sigil (Bracket Round Open)))
                , Located { end = { col = 40, row = 7 }, start = { col = 37, row = 7 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Int"), qualifiers = [] }))
                , Located { end = { col = 41, row = 7 }, start = { col = 40, row = 7 } } (Token (Sigil Comma))
                , Located { end = { col = 42, row = 7 }, start = { col = 41, row = 7 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 48, row = 7 }, start = { col = 42, row = 7 } } (Token (Identifier { name = TokenUpperCase (UpperCase "String"), qualifiers = [] }))
                , Located { end = { col = 49, row = 7 }, start = { col = 48, row = 7 } } (Token (Sigil (Bracket Round Close)))
                , Located { end = { col = 50, row = 7 }, start = { col = 49, row = 7 } } (Token (Sigil Comma))
                , Located { end = { col = 51, row = 7 }, start = { col = 50, row = 7 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 52, row = 7 }, start = { col = 51, row = 7 } } (Token (Sigil (Bracket Round Open)))
                , Located { end = { col = 53, row = 7 }, start = { col = 52, row = 7 } } (Token (Sigil (Bracket Round Close)))
                , Located { end = { col = 54, row = 7 }, start = { col = 53, row = 7 } } (Token (Sigil (Bracket Round Close)))
                , Located { end = { col = 1, row = 8 }, start = { col = 54, row = 7 } } (Newlines [] 0)
                ]
      }
    , { name = "type-alias-function-record"
      , source = """type alias Function = { a: { b: C}, d: E } -> {}
"""
      , pretty = """
        ( ( Ok
          , ( TypeAlias
            , ( ty, Function )
            , ( genericArgs, () )
            , ( expr
              , ( Function
                , ( ( from
                    , ( Record
                      , ( ( a
                          , ( Record
                            , ( ( b
                                , ( UserDefinedType
                                  , ( ( qualifiedness
                                      , ( PossiblyQualified, Nothing )
                                      )
                                    , ( name, C )
                                    , ( args, () )
                                    )
                                  )
                                ) )
                            )
                          )
                        , ( d
                          , ( UserDefinedType
                            , ( ( qualifiedness
                                , ( PossiblyQualified, Nothing )
                                )
                              , ( name, E )
                              , ( args, () )
                              )
                            )
                          )
                        )
                      )
                    )
                  , ( to
                    , ( Record, () )
                    )
                  )
                )
              )
            )
          ) )
"""
      , contextualized =
            Just
                [ Ok
                    (TypeAlias
                        { expr =
                            Function
                                { from =
                                    Record
                                        (Dict.fromList
                                            [ ( "a"
                                              , Record
                                                    (Dict.fromList
                                                        [ ( "b"
                                                          , UserDefinedType
                                                                { args = []
                                                                , name = "C"
                                                                , qualifiedness = PossiblyQualified Nothing
                                                                }
                                                          )
                                                        ]
                                                    )
                                              )
                                            , ( "d"
                                              , UserDefinedType
                                                    { args = []
                                                    , name = "E"
                                                    , qualifiedness = PossiblyQualified Nothing
                                                    }
                                              )
                                            ]
                                        )
                                , to = Record (Dict.fromList [])
                                }
                        , genericArgs = []
                        , ty = UpperCase "Function"
                        }
                    )
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token (Keyword Type))
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token (Keyword Alias))
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 20, row = 1 }, start = { col = 12, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Function"), qualifiers = [] }))
                , Located { end = { col = 21, row = 1 }, start = { col = 20, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 22, row = 1 }, start = { col = 21, row = 1 } } (Token (Sigil Assign))
                , Located { end = { col = 23, row = 1 }, start = { col = 22, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 24, row = 1 }, start = { col = 23, row = 1 } } (Token (Sigil (Bracket Curly Open)))
                , Located { end = { col = 25, row = 1 }, start = { col = 24, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 26, row = 1 }, start = { col = 25, row = 1 } } (Token (Identifier { name = TokenLowerCase (LowerCase "a"), qualifiers = [] }))
                , Located { end = { col = 27, row = 1 }, start = { col = 26, row = 1 } } (Token (Sigil Colon))
                , Located { end = { col = 28, row = 1 }, start = { col = 27, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 29, row = 1 }, start = { col = 28, row = 1 } } (Token (Sigil (Bracket Curly Open)))
                , Located { end = { col = 30, row = 1 }, start = { col = 29, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 31, row = 1 }, start = { col = 30, row = 1 } } (Token (Identifier { name = TokenLowerCase (LowerCase "b"), qualifiers = [] }))
                , Located { end = { col = 32, row = 1 }, start = { col = 31, row = 1 } } (Token (Sigil Colon))
                , Located { end = { col = 33, row = 1 }, start = { col = 32, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 34, row = 1 }, start = { col = 33, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "C"), qualifiers = [] }))
                , Located { end = { col = 35, row = 1 }, start = { col = 34, row = 1 } } (Token (Sigil (Bracket Curly Close)))
                , Located { end = { col = 36, row = 1 }, start = { col = 35, row = 1 } } (Token (Sigil Comma))
                , Located { end = { col = 37, row = 1 }, start = { col = 36, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 38, row = 1 }, start = { col = 37, row = 1 } } (Token (Identifier { name = TokenLowerCase (LowerCase "d"), qualifiers = [] }))
                , Located { end = { col = 39, row = 1 }, start = { col = 38, row = 1 } } (Token (Sigil Colon))
                , Located { end = { col = 40, row = 1 }, start = { col = 39, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 41, row = 1 }, start = { col = 40, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "E"), qualifiers = [] }))
                , Located { end = { col = 42, row = 1 }, start = { col = 41, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 43, row = 1 }, start = { col = 42, row = 1 } } (Token (Sigil (Bracket Curly Close)))
                , Located { end = { col = 44, row = 1 }, start = { col = 43, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 46, row = 1 }, start = { col = 44, row = 1 } } (Token (Sigil ThinArrow))
                , Located { end = { col = 47, row = 1 }, start = { col = 46, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 48, row = 1 }, start = { col = 47, row = 1 } } (Token (Sigil (Bracket Curly Open)))
                , Located { end = { col = 49, row = 1 }, start = { col = 48, row = 1 } } (Token (Sigil (Bracket Curly Close)))
                , Located { end = { col = 1, row = 2 }, start = { col = 49, row = 1 } } (Newlines [] 0)
                ]
      }
    , { name = "type-alias-function-tuple"
      , source = """type alias Function = () -> (Int, String)
"""
      , pretty = """
        ( ( Ok
          , ( TypeAlias
            , ( ty, Function )
            , ( genericArgs, () )
            , ( expr
              , ( Function
                , ( ( from, Unit )
                  , ( to
                    , ( Tuple
                      , ( ( UserDefinedType
                          , ( ( qualifiedness
                              , ( PossiblyQualified, Nothing )
                              )
                            , ( name, Int )
                            , ( args, () )
                            )
                          )
                        , ( UserDefinedType
                          , ( ( qualifiedness
                              , ( PossiblyQualified, Nothing )
                              )
                            , ( name, String )
                            , ( args, () )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          ) )
"""
      , contextualized =
            Just
                [ Ok
                    (TypeAlias
                        { expr =
                            Function
                                { from = Unit
                                , to =
                                    Tuple
                                        (UserDefinedType
                                            { args = []
                                            , name = "Int"
                                            , qualifiedness = PossiblyQualified Nothing
                                            }
                                        )
                                        (UserDefinedType
                                            { args = []
                                            , name = "String"
                                            , qualifiedness = PossiblyQualified Nothing
                                            }
                                        )
                                }
                        , genericArgs = []
                        , ty = UpperCase "Function"
                        }
                    )
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token (Keyword Type))
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token (Keyword Alias))
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 20, row = 1 }, start = { col = 12, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Function"), qualifiers = [] }))
                , Located { end = { col = 21, row = 1 }, start = { col = 20, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 22, row = 1 }, start = { col = 21, row = 1 } } (Token (Sigil Assign))
                , Located { end = { col = 23, row = 1 }, start = { col = 22, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 24, row = 1 }, start = { col = 23, row = 1 } } (Token (Sigil (Bracket Round Open)))
                , Located { end = { col = 25, row = 1 }, start = { col = 24, row = 1 } } (Token (Sigil (Bracket Round Close)))
                , Located { end = { col = 26, row = 1 }, start = { col = 25, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 28, row = 1 }, start = { col = 26, row = 1 } } (Token (Sigil ThinArrow))
                , Located { end = { col = 29, row = 1 }, start = { col = 28, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 30, row = 1 }, start = { col = 29, row = 1 } } (Token (Sigil (Bracket Round Open)))
                , Located { end = { col = 33, row = 1 }, start = { col = 30, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Int"), qualifiers = [] }))
                , Located { end = { col = 34, row = 1 }, start = { col = 33, row = 1 } } (Token (Sigil Comma))
                , Located { end = { col = 35, row = 1 }, start = { col = 34, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 41, row = 1 }, start = { col = 35, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "String"), qualifiers = [] }))
                , Located { end = { col = 42, row = 1 }, start = { col = 41, row = 1 } } (Token (Sigil (Bracket Round Close)))
                , Located { end = { col = 1, row = 2 }, start = { col = 42, row = 1 } } (Newlines [] 0)
                ]
      }
    , { name = "type-alias-function-type-args"
      , source = """type alias Function = List A -> List B C D -> List D E F
"""
      , pretty = """
        ( ( Ok
          , ( TypeAlias
            , ( ty, Function )
            , ( genericArgs, () )
            , ( expr
              , ( Function
                , ( ( from
                    , ( UserDefinedType
                      , ( ( qualifiedness
                          , ( PossiblyQualified, Nothing )
                          )
                        , ( name, List )
                        , ( args
                          , ( ( UserDefinedType
                              , ( ( qualifiedness
                                  , ( PossiblyQualified, Nothing )
                                  )
                                , ( name, A )
                                , ( args, () )
                                )
                              ) )
                          )
                        )
                      )
                    )
                  , ( to
                    , ( Function
                      , ( ( from
                          , ( UserDefinedType
                            , ( ( qualifiedness
                                , ( PossiblyQualified, Nothing )
                                )
                              , ( name, List )
                              , ( args
                                , ( ( UserDefinedType
                                    , ( ( qualifiedness
                                        , ( PossiblyQualified, Nothing )
                                        )
                                      , ( name, B )
                                      , ( args, () )
                                      )
                                    )
                                  , ( UserDefinedType
                                    , ( ( qualifiedness
                                        , ( PossiblyQualified, Nothing )
                                        )
                                      , ( name, C )
                                      , ( args, () )
                                      )
                                    )
                                  , ( UserDefinedType
                                    , ( ( qualifiedness
                                        , ( PossiblyQualified, Nothing )
                                        )
                                      , ( name, D )
                                      , ( args, () )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        , ( to
                          , ( UserDefinedType
                            , ( ( qualifiedness
                                , ( PossiblyQualified, Nothing )
                                )
                              , ( name, List )
                              , ( args
                                , ( ( UserDefinedType
                                    , ( ( qualifiedness
                                        , ( PossiblyQualified, Nothing )
                                        )
                                      , ( name, D )
                                      , ( args, () )
                                      )
                                    )
                                  , ( UserDefinedType
                                    , ( ( qualifiedness
                                        , ( PossiblyQualified, Nothing )
                                        )
                                      , ( name, E )
                                      , ( args, () )
                                      )
                                    )
                                  , ( UserDefinedType
                                    , ( ( qualifiedness
                                        , ( PossiblyQualified, Nothing )
                                        )
                                      , ( name, F )
                                      , ( args, () )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          ) )
"""
      , contextualized =
            Just
                [ Ok
                    (TypeAlias
                        { expr =
                            Function
                                { from =
                                    UserDefinedType
                                        { args =
                                            [ UserDefinedType
                                                { args = []
                                                , name = "A"
                                                , qualifiedness = PossiblyQualified Nothing
                                                }
                                            ]
                                        , name = "List"
                                        , qualifiedness = PossiblyQualified Nothing
                                        }
                                , to =
                                    Function
                                        { from =
                                            UserDefinedType
                                                { args =
                                                    [ UserDefinedType
                                                        { args = []
                                                        , name = "B"
                                                        , qualifiedness = PossiblyQualified Nothing
                                                        }
                                                    , UserDefinedType
                                                        { args = []
                                                        , name = "C"
                                                        , qualifiedness = PossiblyQualified Nothing
                                                        }
                                                    , UserDefinedType
                                                        { args = []
                                                        , name = "D"
                                                        , qualifiedness = PossiblyQualified Nothing
                                                        }
                                                    ]
                                                , name = "List"
                                                , qualifiedness = PossiblyQualified Nothing
                                                }
                                        , to =
                                            UserDefinedType
                                                { args =
                                                    [ UserDefinedType
                                                        { args = []
                                                        , name = "D"
                                                        , qualifiedness = PossiblyQualified Nothing
                                                        }
                                                    , UserDefinedType
                                                        { args = []
                                                        , name = "E"
                                                        , qualifiedness = PossiblyQualified Nothing
                                                        }
                                                    , UserDefinedType
                                                        { args = []
                                                        , name = "F"
                                                        , qualifiedness = PossiblyQualified Nothing
                                                        }
                                                    ]
                                                , name = "List"
                                                , qualifiedness = PossiblyQualified Nothing
                                                }
                                        }
                                }
                        , genericArgs = []
                        , ty = UpperCase "Function"
                        }
                    )
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token (Keyword Type))
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token (Keyword Alias))
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 20, row = 1 }, start = { col = 12, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Function"), qualifiers = [] }))
                , Located { end = { col = 21, row = 1 }, start = { col = 20, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 22, row = 1 }, start = { col = 21, row = 1 } } (Token (Sigil Assign))
                , Located { end = { col = 23, row = 1 }, start = { col = 22, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 27, row = 1 }, start = { col = 23, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "List"), qualifiers = [] }))
                , Located { end = { col = 28, row = 1 }, start = { col = 27, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 29, row = 1 }, start = { col = 28, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "A"), qualifiers = [] }))
                , Located { end = { col = 30, row = 1 }, start = { col = 29, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 32, row = 1 }, start = { col = 30, row = 1 } } (Token (Sigil ThinArrow))
                , Located { end = { col = 33, row = 1 }, start = { col = 32, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 37, row = 1 }, start = { col = 33, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "List"), qualifiers = [] }))
                , Located { end = { col = 38, row = 1 }, start = { col = 37, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 39, row = 1 }, start = { col = 38, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "B"), qualifiers = [] }))
                , Located { end = { col = 40, row = 1 }, start = { col = 39, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 41, row = 1 }, start = { col = 40, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "C"), qualifiers = [] }))
                , Located { end = { col = 42, row = 1 }, start = { col = 41, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 43, row = 1 }, start = { col = 42, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "D"), qualifiers = [] }))
                , Located { end = { col = 44, row = 1 }, start = { col = 43, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 46, row = 1 }, start = { col = 44, row = 1 } } (Token (Sigil ThinArrow))
                , Located { end = { col = 47, row = 1 }, start = { col = 46, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 51, row = 1 }, start = { col = 47, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "List"), qualifiers = [] }))
                , Located { end = { col = 52, row = 1 }, start = { col = 51, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 53, row = 1 }, start = { col = 52, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "D"), qualifiers = [] }))
                , Located { end = { col = 54, row = 1 }, start = { col = 53, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 55, row = 1 }, start = { col = 54, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "E"), qualifiers = [] }))
                , Located { end = { col = 56, row = 1 }, start = { col = 55, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 57, row = 1 }, start = { col = 56, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "F"), qualifiers = [] }))
                , Located { end = { col = 1, row = 2 }, start = { col = 57, row = 1 } } (Newlines [] 0)
                ]
      }
    , { name = "type-alias-funky-indentation"
      , source = """type alias
    Model = List Int
"""
      , pretty = """
        ( ( Ok
          , ( TypeAlias
            , ( ty, Model )
            , ( genericArgs, () )
            , ( expr
              , ( UserDefinedType
                , ( ( qualifiedness
                    , ( PossiblyQualified, Nothing )
                    )
                  , ( name, List )
                  , ( args
                    , ( ( UserDefinedType
                        , ( ( qualifiedness
                            , ( PossiblyQualified, Nothing )
                            )
                          , ( name, Int )
                          , ( args, () )
                          )
                        ) )
                    )
                  )
                )
              )
            )
          ) )
"""
      , contextualized =
            Just
                [ Ok
                    (TypeAlias
                        { expr =
                            UserDefinedType
                                { args =
                                    [ UserDefinedType
                                        { args = []
                                        , name = "Int"
                                        , qualifiedness = PossiblyQualified Nothing
                                        }
                                    ]
                                , name = "List"
                                , qualifiedness = PossiblyQualified Nothing
                                }
                        , genericArgs = []
                        , ty = UpperCase "Model"
                        }
                    )
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token (Keyword Type))
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token (Keyword Alias))
                , Located { end = { col = 5, row = 2 }, start = { col = 11, row = 1 } } (Newlines [] 4)
                , Located { end = { col = 10, row = 2 }, start = { col = 5, row = 2 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Model"), qualifiers = [] }))
                , Located { end = { col = 11, row = 2 }, start = { col = 10, row = 2 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 12, row = 2 }, start = { col = 11, row = 2 } } (Token (Sigil Assign))
                , Located { end = { col = 13, row = 2 }, start = { col = 12, row = 2 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 17, row = 2 }, start = { col = 13, row = 2 } } (Token (Identifier { name = TokenUpperCase (UpperCase "List"), qualifiers = [] }))
                , Located { end = { col = 18, row = 2 }, start = { col = 17, row = 2 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 21, row = 2 }, start = { col = 18, row = 2 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Int"), qualifiers = [] }))
                , Located { end = { col = 1, row = 3 }, start = { col = 21, row = 2 } } (Newlines [] 0)
                ]
      }
    , { name = "type-alias-funky-indentation-2"
      , source = """type alias
    Model =
 List Int
"""
      , pretty = """
        ( ( Ok
          , ( TypeAlias
            , ( ty, Model )
            , ( genericArgs, () )
            , ( expr
              , ( UserDefinedType
                , ( ( qualifiedness
                    , ( PossiblyQualified, Nothing )
                    )
                  , ( name, List )
                  , ( args
                    , ( ( UserDefinedType
                        , ( ( qualifiedness
                            , ( PossiblyQualified, Nothing )
                            )
                          , ( name, Int )
                          , ( args, () )
                          )
                        ) )
                    )
                  )
                )
              )
            )
          ) )
"""
      , contextualized =
            Just
                [ Ok
                    (TypeAlias
                        { expr =
                            UserDefinedType
                                { args =
                                    [ UserDefinedType
                                        { args = []
                                        , name = "Int"
                                        , qualifiedness = PossiblyQualified Nothing
                                        }
                                    ]
                                , name = "List"
                                , qualifiedness = PossiblyQualified Nothing
                                }
                        , genericArgs = []
                        , ty = UpperCase "Model"
                        }
                    )
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token (Keyword Type))
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token (Keyword Alias))
                , Located { end = { col = 5, row = 2 }, start = { col = 11, row = 1 } } (Newlines [] 4)
                , Located { end = { col = 10, row = 2 }, start = { col = 5, row = 2 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Model"), qualifiers = [] }))
                , Located { end = { col = 11, row = 2 }, start = { col = 10, row = 2 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 12, row = 2 }, start = { col = 11, row = 2 } } (Token (Sigil Assign))
                , Located { end = { col = 2, row = 3 }, start = { col = 12, row = 2 } } (Newlines [] 1)
                , Located { end = { col = 6, row = 3 }, start = { col = 2, row = 3 } } (Token (Identifier { name = TokenUpperCase (UpperCase "List"), qualifiers = [] }))
                , Located { end = { col = 7, row = 3 }, start = { col = 6, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 10, row = 3 }, start = { col = 7, row = 3 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Int"), qualifiers = [] }))
                , Located { end = { col = 1, row = 4 }, start = { col = 10, row = 3 } } (Newlines [] 0)
                ]
      }
    , { name = "type-alias-qualified-type"
      , source = """type alias A = B.C.D

type alias A = List B.C.D"""
      , pretty = """
        ( ( Ok
          , ( TypeAlias
            , ( ty, A )
            , ( genericArgs, () )
            , ( expr
              , ( UserDefinedType
                , ( ( qualifiedness
                    , ( PossiblyQualified
                      , ( Just, B.C )
                      )
                    )
                  , ( name, D )
                  , ( args, () )
                  )
                )
              )
            )
          )
        , ( Ok
          , ( TypeAlias
            , ( ty, A )
            , ( genericArgs, () )
            , ( expr
              , ( UserDefinedType
                , ( ( qualifiedness
                    , ( PossiblyQualified
                      , ( Just, B.C )
                      )
                    )
                  , ( name, List )
                  , ( args
                    , ( ( UserDefinedType
                        , ( ( qualifiedness
                            , ( PossiblyQualified
                              , ( Just, B.C )
                              )
                            )
                          , ( name, D )
                          , ( args, () )
                          )
                        ) )
                    )
                  )
                )
              )
            )
          )
        )
"""
      , contextualized =
            Just
                [ Ok
                    (TypeAlias
                        { expr =
                            UserDefinedType
                                { args = []
                                , name = "D"
                                , qualifiedness = PossiblyQualified (Just "B.C")
                                }
                        , genericArgs = []
                        , ty = UpperCase "A"
                        }
                    )
                , Ok
                    (TypeAlias
                        { expr =
                            UserDefinedType
                                { args =
                                    [ UserDefinedType
                                        { args = []
                                        , name = "D"
                                        , qualifiedness = PossiblyQualified (Just "B.C")
                                        }
                                    ]
                                , name = "List"
                                , qualifiedness = PossiblyQualified (Just "B.C")
                                }
                        , genericArgs = []
                        , ty = UpperCase "A"
                        }
                    )
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token (Keyword Type))
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token (Keyword Alias))
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 13, row = 1 }, start = { col = 12, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "A"), qualifiers = [] }))
                , Located { end = { col = 14, row = 1 }, start = { col = 13, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Token (Sigil Assign))
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 21, row = 1 }, start = { col = 16, row = 1 } }
                    (Token
                        (Identifier
                            { name = TokenUpperCase (UpperCase "D")
                            , qualifiers =
                                [ UpperCase "B"
                                , UpperCase "C"
                                ]
                            }
                        )
                    )
                , Located { end = { col = 1, row = 3 }, start = { col = 21, row = 1 } }
                    (Newlines
                        [ 0
                        ]
                        0
                    )
                , Located { end = { col = 5, row = 3 }, start = { col = 1, row = 3 } } (Token (Keyword Type))
                , Located { end = { col = 6, row = 3 }, start = { col = 5, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 11, row = 3 }, start = { col = 6, row = 3 } } (Token (Keyword Alias))
                , Located { end = { col = 12, row = 3 }, start = { col = 11, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 13, row = 3 }, start = { col = 12, row = 3 } } (Token (Identifier { name = TokenUpperCase (UpperCase "A"), qualifiers = [] }))
                , Located { end = { col = 14, row = 3 }, start = { col = 13, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 15, row = 3 }, start = { col = 14, row = 3 } } (Token (Sigil Assign))
                , Located { end = { col = 16, row = 3 }, start = { col = 15, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 20, row = 3 }, start = { col = 16, row = 3 } } (Token (Identifier { name = TokenUpperCase (UpperCase "List"), qualifiers = [] }))
                , Located { end = { col = 21, row = 3 }, start = { col = 20, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 26, row = 3 }, start = { col = 21, row = 3 } }
                    (Token
                        (Identifier
                            { name = TokenUpperCase (UpperCase "D")
                            , qualifiers =
                                [ UpperCase "B"
                                , UpperCase "C"
                                ]
                            }
                        )
                    )
                ]
      }
    , { name = "type-alias-record-3-entries"
      , source = """type alias Ty = { a: A, b: B, c: C }
"""
      , pretty = """
        ( ( Ok
          , ( TypeAlias
            , ( ty, Ty )
            , ( genericArgs, () )
            , ( expr
              , ( Record
                , ( ( a
                    , ( UserDefinedType
                      , ( ( qualifiedness
                          , ( PossiblyQualified, Nothing )
                          )
                        , ( name, A )
                        , ( args, () )
                        )
                      )
                    )
                  , ( b
                    , ( UserDefinedType
                      , ( ( qualifiedness
                          , ( PossiblyQualified, Nothing )
                          )
                        , ( name, B )
                        , ( args, () )
                        )
                      )
                    )
                  , ( c
                    , ( UserDefinedType
                      , ( ( qualifiedness
                          , ( PossiblyQualified, Nothing )
                          )
                        , ( name, C )
                        , ( args, () )
                        )
                      )
                    )
                  )
                )
              )
            )
          ) )
"""
      , contextualized =
            Just
                [ Ok
                    (TypeAlias
                        { expr =
                            Record
                                (Dict.fromList
                                    [ ( "a"
                                      , UserDefinedType
                                            { args = []
                                            , name = "A"
                                            , qualifiedness = PossiblyQualified Nothing
                                            }
                                      )
                                    , ( "b"
                                      , UserDefinedType
                                            { args = []
                                            , name = "B"
                                            , qualifiedness = PossiblyQualified Nothing
                                            }
                                      )
                                    , ( "c"
                                      , UserDefinedType
                                            { args = []
                                            , name = "C"
                                            , qualifiedness = PossiblyQualified Nothing
                                            }
                                      )
                                    ]
                                )
                        , genericArgs = []
                        , ty = UpperCase "Ty"
                        }
                    )
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token (Keyword Type))
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token (Keyword Alias))
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Ty"), qualifiers = [] }))
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Token (Sigil Assign))
                , Located { end = { col = 17, row = 1 }, start = { col = 16, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Token (Sigil (Bracket Curly Open)))
                , Located { end = { col = 19, row = 1 }, start = { col = 18, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 20, row = 1 }, start = { col = 19, row = 1 } } (Token (Identifier { name = TokenLowerCase (LowerCase "a"), qualifiers = [] }))
                , Located { end = { col = 21, row = 1 }, start = { col = 20, row = 1 } } (Token (Sigil Colon))
                , Located { end = { col = 22, row = 1 }, start = { col = 21, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 23, row = 1 }, start = { col = 22, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "A"), qualifiers = [] }))
                , Located { end = { col = 24, row = 1 }, start = { col = 23, row = 1 } } (Token (Sigil Comma))
                , Located { end = { col = 25, row = 1 }, start = { col = 24, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 26, row = 1 }, start = { col = 25, row = 1 } } (Token (Identifier { name = TokenLowerCase (LowerCase "b"), qualifiers = [] }))
                , Located { end = { col = 27, row = 1 }, start = { col = 26, row = 1 } } (Token (Sigil Colon))
                , Located { end = { col = 28, row = 1 }, start = { col = 27, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 29, row = 1 }, start = { col = 28, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "B"), qualifiers = [] }))
                , Located { end = { col = 30, row = 1 }, start = { col = 29, row = 1 } } (Token (Sigil Comma))
                , Located { end = { col = 31, row = 1 }, start = { col = 30, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 32, row = 1 }, start = { col = 31, row = 1 } } (Token (Identifier { name = TokenLowerCase (LowerCase "c"), qualifiers = [] }))
                , Located { end = { col = 33, row = 1 }, start = { col = 32, row = 1 } } (Token (Sigil Colon))
                , Located { end = { col = 34, row = 1 }, start = { col = 33, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 35, row = 1 }, start = { col = 34, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "C"), qualifiers = [] }))
                , Located { end = { col = 36, row = 1 }, start = { col = 35, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 37, row = 1 }, start = { col = 36, row = 1 } } (Token (Sigil (Bracket Curly Close)))
                , Located { end = { col = 1, row = 2 }, start = { col = 37, row = 1 } } (Newlines [] 0)
                ]
      }
    , { name = "type-alias-record-empty"
      , source = """type alias Ty = {}
"""
      , pretty = """
        ( ( Ok
          , ( TypeAlias
            , ( ty, Ty )
            , ( genericArgs, () )
            , ( expr
              , ( Record, () )
              )
            )
          ) )
"""
      , contextualized =
            Just
                [ Ok (TypeAlias { expr = Record (Dict.fromList []), genericArgs = [], ty = UpperCase "Ty" })
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token (Keyword Type))
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token (Keyword Alias))
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Ty"), qualifiers = [] }))
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Token (Sigil Assign))
                , Located { end = { col = 17, row = 1 }, start = { col = 16, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Token (Sigil (Bracket Curly Open)))
                , Located { end = { col = 19, row = 1 }, start = { col = 18, row = 1 } } (Token (Sigil (Bracket Curly Close)))
                , Located { end = { col = 1, row = 2 }, start = { col = 19, row = 1 } } (Newlines [] 0)
                ]
      }
    , { name = "type-alias-record-empty-multiline"
      , source = """type alias Ty = {


    }
"""
      , pretty = """
        ( ( Ok
          , ( TypeAlias
            , ( ty, Ty )
            , ( genericArgs, () )
            , ( expr
              , ( Record, () )
              )
            )
          ) )
"""
      , contextualized =
            Just
                [ Ok (TypeAlias { expr = Record (Dict.fromList []), genericArgs = [], ty = UpperCase "Ty" })
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token (Keyword Type))
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token (Keyword Alias))
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Ty"), qualifiers = [] }))
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Token (Sigil Assign))
                , Located { end = { col = 17, row = 1 }, start = { col = 16, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Token (Sigil (Bracket Curly Open)))
                , Located { end = { col = 5, row = 4 }, start = { col = 18, row = 1 } }
                    (Newlines
                        [ 0
                        , 0
                        ]
                        4
                    )
                , Located { end = { col = 6, row = 4 }, start = { col = 5, row = 4 } } (Token (Sigil (Bracket Curly Close)))
                , Located { end = { col = 1, row = 5 }, start = { col = 6, row = 4 } } (Newlines [] 0)
                ]
      }
    , { name = "type-alias-record-in-bracket"
      , source = """type alias Ty = ({ hi: Int })
"""
      , pretty = """
        ( ( Ok
          , ( TypeAlias
            , ( ty, Ty )
            , ( genericArgs, () )
            , ( expr
              , ( Record
                , ( ( hi
                    , ( UserDefinedType
                      , ( ( qualifiedness
                          , ( PossiblyQualified, Nothing )
                          )
                        , ( name, Int )
                        , ( args, () )
                        )
                      )
                    ) )
                )
              )
            )
          ) )
"""
      , contextualized =
            Just
                [ Ok
                    (TypeAlias
                        { expr =
                            Record
                                (Dict.fromList
                                    [ ( "hi"
                                      , UserDefinedType
                                            { args = []
                                            , name = "Int"
                                            , qualifiedness = PossiblyQualified Nothing
                                            }
                                      )
                                    ]
                                )
                        , genericArgs = []
                        , ty = UpperCase "Ty"
                        }
                    )
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token (Keyword Type))
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token (Keyword Alias))
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Ty"), qualifiers = [] }))
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Token (Sigil Assign))
                , Located { end = { col = 17, row = 1 }, start = { col = 16, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Token (Sigil (Bracket Round Open)))
                , Located { end = { col = 19, row = 1 }, start = { col = 18, row = 1 } } (Token (Sigil (Bracket Curly Open)))
                , Located { end = { col = 20, row = 1 }, start = { col = 19, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 22, row = 1 }, start = { col = 20, row = 1 } } (Token (Identifier { name = TokenLowerCase (LowerCase "hi"), qualifiers = [] }))
                , Located { end = { col = 23, row = 1 }, start = { col = 22, row = 1 } } (Token (Sigil Colon))
                , Located { end = { col = 24, row = 1 }, start = { col = 23, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 27, row = 1 }, start = { col = 24, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Int"), qualifiers = [] }))
                , Located { end = { col = 28, row = 1 }, start = { col = 27, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 29, row = 1 }, start = { col = 28, row = 1 } } (Token (Sigil (Bracket Curly Close)))
                , Located { end = { col = 30, row = 1 }, start = { col = 29, row = 1 } } (Token (Sigil (Bracket Round Close)))
                , Located { end = { col = 1, row = 2 }, start = { col = 30, row = 1 } } (Newlines [] 0)
                ]
      }
    , { name = "type-alias-record-nested"
      , source = """type alias Ty =
    { hi:  { a: Int, b: List String }
    , ih: CustomType A B C (D E)
    }
"""
      , pretty = """
        ( ( Ok
          , ( TypeAlias
            , ( ty, Ty )
            , ( genericArgs, () )
            , ( expr
              , ( Record
                , ( ( hi
                    , ( Record
                      , ( ( a
                          , ( UserDefinedType
                            , ( ( qualifiedness
                                , ( PossiblyQualified, Nothing )
                                )
                              , ( name, Int )
                              , ( args, () )
                              )
                            )
                          )
                        , ( b
                          , ( UserDefinedType
                            , ( ( qualifiedness
                                , ( PossiblyQualified, Nothing )
                                )
                              , ( name, List )
                              , ( args
                                , ( ( UserDefinedType
                                    , ( ( qualifiedness
                                        , ( PossiblyQualified, Nothing )
                                        )
                                      , ( name, String )
                                      , ( args, () )
                                      )
                                    ) )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  , ( ih
                    , ( UserDefinedType
                      , ( ( qualifiedness
                          , ( PossiblyQualified, Nothing )
                          )
                        , ( name, CustomType )
                        , ( args
                          , ( ( UserDefinedType
                              , ( ( qualifiedness
                                  , ( PossiblyQualified, Nothing )
                                  )
                                , ( name, A )
                                , ( args, () )
                                )
                              )
                            , ( UserDefinedType
                              , ( ( qualifiedness
                                  , ( PossiblyQualified, Nothing )
                                  )
                                , ( name, B )
                                , ( args, () )
                                )
                              )
                            , ( UserDefinedType
                              , ( ( qualifiedness
                                  , ( PossiblyQualified, Nothing )
                                  )
                                , ( name, C )
                                , ( args, () )
                                )
                              )
                            , ( UserDefinedType
                              , ( ( qualifiedness
                                  , ( PossiblyQualified, Nothing )
                                  )
                                , ( name, D )
                                , ( args
                                  , ( ( UserDefinedType
                                      , ( ( qualifiedness
                                          , ( PossiblyQualified, Nothing )
                                          )
                                        , ( name, E )
                                        , ( args, () )
                                        )
                                      ) )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          ) )
"""
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
                                                [ ( "a"
                                                  , UserDefinedType
                                                        { args = []
                                                        , name = "Int"
                                                        , qualifiedness = PossiblyQualified Nothing
                                                        }
                                                  )
                                                , ( "b"
                                                  , UserDefinedType
                                                        { args =
                                                            [ UserDefinedType
                                                                { args = []
                                                                , name = "String"
                                                                , qualifiedness = PossiblyQualified Nothing
                                                                }
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
                                                [ UserDefinedType
                                                    { args = []
                                                    , name = "A"
                                                    , qualifiedness = PossiblyQualified Nothing
                                                    }
                                                , UserDefinedType
                                                    { args = []
                                                    , name = "B"
                                                    , qualifiedness = PossiblyQualified Nothing
                                                    }
                                                , UserDefinedType
                                                    { args = []
                                                    , name = "C"
                                                    , qualifiedness = PossiblyQualified Nothing
                                                    }
                                                , UserDefinedType
                                                    { args =
                                                        [ UserDefinedType
                                                            { args = []
                                                            , name = "E"
                                                            , qualifiedness = PossiblyQualified Nothing
                                                            }
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
                        , genericArgs = []
                        , ty = UpperCase "Ty"
                        }
                    )
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token (Keyword Type))
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token (Keyword Alias))
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Ty"), qualifiers = [] }))
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Token (Sigil Assign))
                , Located { end = { col = 5, row = 2 }, start = { col = 16, row = 1 } } (Newlines [] 4)
                , Located { end = { col = 6, row = 2 }, start = { col = 5, row = 2 } } (Token (Sigil (Bracket Curly Open)))
                , Located { end = { col = 7, row = 2 }, start = { col = 6, row = 2 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 9, row = 2 }, start = { col = 7, row = 2 } } (Token (Identifier { name = TokenLowerCase (LowerCase "hi"), qualifiers = [] }))
                , Located { end = { col = 10, row = 2 }, start = { col = 9, row = 2 } } (Token (Sigil Colon))
                , Located { end = { col = 12, row = 2 }, start = { col = 10, row = 2 } } (Ignorable (Whitespace 2))
                , Located { end = { col = 13, row = 2 }, start = { col = 12, row = 2 } } (Token (Sigil (Bracket Curly Open)))
                , Located { end = { col = 14, row = 2 }, start = { col = 13, row = 2 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 15, row = 2 }, start = { col = 14, row = 2 } } (Token (Identifier { name = TokenLowerCase (LowerCase "a"), qualifiers = [] }))
                , Located { end = { col = 16, row = 2 }, start = { col = 15, row = 2 } } (Token (Sigil Colon))
                , Located { end = { col = 17, row = 2 }, start = { col = 16, row = 2 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 20, row = 2 }, start = { col = 17, row = 2 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Int"), qualifiers = [] }))
                , Located { end = { col = 21, row = 2 }, start = { col = 20, row = 2 } } (Token (Sigil Comma))
                , Located { end = { col = 22, row = 2 }, start = { col = 21, row = 2 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 23, row = 2 }, start = { col = 22, row = 2 } } (Token (Identifier { name = TokenLowerCase (LowerCase "b"), qualifiers = [] }))
                , Located { end = { col = 24, row = 2 }, start = { col = 23, row = 2 } } (Token (Sigil Colon))
                , Located { end = { col = 25, row = 2 }, start = { col = 24, row = 2 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 29, row = 2 }, start = { col = 25, row = 2 } } (Token (Identifier { name = TokenUpperCase (UpperCase "List"), qualifiers = [] }))
                , Located { end = { col = 30, row = 2 }, start = { col = 29, row = 2 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 36, row = 2 }, start = { col = 30, row = 2 } } (Token (Identifier { name = TokenUpperCase (UpperCase "String"), qualifiers = [] }))
                , Located { end = { col = 37, row = 2 }, start = { col = 36, row = 2 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 38, row = 2 }, start = { col = 37, row = 2 } } (Token (Sigil (Bracket Curly Close)))
                , Located { end = { col = 5, row = 3 }, start = { col = 38, row = 2 } } (Newlines [] 4)
                , Located { end = { col = 6, row = 3 }, start = { col = 5, row = 3 } } (Token (Sigil Comma))
                , Located { end = { col = 7, row = 3 }, start = { col = 6, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 9, row = 3 }, start = { col = 7, row = 3 } } (Token (Identifier { name = TokenLowerCase (LowerCase "ih"), qualifiers = [] }))
                , Located { end = { col = 10, row = 3 }, start = { col = 9, row = 3 } } (Token (Sigil Colon))
                , Located { end = { col = 11, row = 3 }, start = { col = 10, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 21, row = 3 }, start = { col = 11, row = 3 } } (Token (Identifier { name = TokenUpperCase (UpperCase "CustomType"), qualifiers = [] }))
                , Located { end = { col = 22, row = 3 }, start = { col = 21, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 23, row = 3 }, start = { col = 22, row = 3 } } (Token (Identifier { name = TokenUpperCase (UpperCase "A"), qualifiers = [] }))
                , Located { end = { col = 24, row = 3 }, start = { col = 23, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 25, row = 3 }, start = { col = 24, row = 3 } } (Token (Identifier { name = TokenUpperCase (UpperCase "B"), qualifiers = [] }))
                , Located { end = { col = 26, row = 3 }, start = { col = 25, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 27, row = 3 }, start = { col = 26, row = 3 } } (Token (Identifier { name = TokenUpperCase (UpperCase "C"), qualifiers = [] }))
                , Located { end = { col = 28, row = 3 }, start = { col = 27, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 29, row = 3 }, start = { col = 28, row = 3 } } (Token (Sigil (Bracket Round Open)))
                , Located { end = { col = 30, row = 3 }, start = { col = 29, row = 3 } } (Token (Identifier { name = TokenUpperCase (UpperCase "D"), qualifiers = [] }))
                , Located { end = { col = 31, row = 3 }, start = { col = 30, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 32, row = 3 }, start = { col = 31, row = 3 } } (Token (Identifier { name = TokenUpperCase (UpperCase "E"), qualifiers = [] }))
                , Located { end = { col = 33, row = 3 }, start = { col = 32, row = 3 } } (Token (Sigil (Bracket Round Close)))
                , Located { end = { col = 5, row = 4 }, start = { col = 33, row = 3 } } (Newlines [] 4)
                , Located { end = { col = 6, row = 4 }, start = { col = 5, row = 4 } } (Token (Sigil (Bracket Curly Close)))
                , Located { end = { col = 1, row = 5 }, start = { col = 6, row = 4 } } (Newlines [] 0)
                ]
      }
    , { name = "type-alias-record-simple"
      , source = """type alias Ty = { hi: Int }
"""
      , pretty = """
        ( ( Ok
          , ( TypeAlias
            , ( ty, Ty )
            , ( genericArgs, () )
            , ( expr
              , ( Record
                , ( ( hi
                    , ( UserDefinedType
                      , ( ( qualifiedness
                          , ( PossiblyQualified, Nothing )
                          )
                        , ( name, Int )
                        , ( args, () )
                        )
                      )
                    ) )
                )
              )
            )
          ) )
"""
      , contextualized =
            Just
                [ Ok
                    (TypeAlias
                        { expr =
                            Record
                                (Dict.fromList
                                    [ ( "hi"
                                      , UserDefinedType
                                            { args = []
                                            , name = "Int"
                                            , qualifiedness = PossiblyQualified Nothing
                                            }
                                      )
                                    ]
                                )
                        , genericArgs = []
                        , ty = UpperCase "Ty"
                        }
                    )
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token (Keyword Type))
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token (Keyword Alias))
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Ty"), qualifiers = [] }))
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Token (Sigil Assign))
                , Located { end = { col = 17, row = 1 }, start = { col = 16, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Token (Sigil (Bracket Curly Open)))
                , Located { end = { col = 19, row = 1 }, start = { col = 18, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 21, row = 1 }, start = { col = 19, row = 1 } } (Token (Identifier { name = TokenLowerCase (LowerCase "hi"), qualifiers = [] }))
                , Located { end = { col = 22, row = 1 }, start = { col = 21, row = 1 } } (Token (Sigil Colon))
                , Located { end = { col = 23, row = 1 }, start = { col = 22, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 26, row = 1 }, start = { col = 23, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Int"), qualifiers = [] }))
                , Located { end = { col = 27, row = 1 }, start = { col = 26, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 28, row = 1 }, start = { col = 27, row = 1 } } (Token (Sigil (Bracket Curly Close)))
                , Located { end = { col = 1, row = 2 }, start = { col = 28, row = 1 } } (Newlines [] 0)
                ]
      }
    , { name = "type-alias-record-two-entries"
      , source = """type alias Ty = { hi: (), buy: String }
"""
      , pretty = """
        ( ( Ok
          , ( TypeAlias
            , ( ty, Ty )
            , ( genericArgs, () )
            , ( expr
              , ( Record
                , ( ( buy
                    , ( UserDefinedType
                      , ( ( qualifiedness
                          , ( PossiblyQualified, Nothing )
                          )
                        , ( name, String )
                        , ( args, () )
                        )
                      )
                    )
                  , ( hi, Unit )
                  )
                )
              )
            )
          ) )
"""
      , contextualized =
            Just
                [ Ok
                    (TypeAlias
                        { expr =
                            Record
                                (Dict.fromList
                                    [ ( "buy"
                                      , UserDefinedType
                                            { args = []
                                            , name = "String"
                                            , qualifiedness = PossiblyQualified Nothing
                                            }
                                      )
                                    , ( "hi", Unit )
                                    ]
                                )
                        , genericArgs = []
                        , ty = UpperCase "Ty"
                        }
                    )
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token (Keyword Type))
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token (Keyword Alias))
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Ty"), qualifiers = [] }))
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Token (Sigil Assign))
                , Located { end = { col = 17, row = 1 }, start = { col = 16, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Token (Sigil (Bracket Curly Open)))
                , Located { end = { col = 19, row = 1 }, start = { col = 18, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 21, row = 1 }, start = { col = 19, row = 1 } } (Token (Identifier { name = TokenLowerCase (LowerCase "hi"), qualifiers = [] }))
                , Located { end = { col = 22, row = 1 }, start = { col = 21, row = 1 } } (Token (Sigil Colon))
                , Located { end = { col = 23, row = 1 }, start = { col = 22, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 24, row = 1 }, start = { col = 23, row = 1 } } (Token (Sigil (Bracket Round Open)))
                , Located { end = { col = 25, row = 1 }, start = { col = 24, row = 1 } } (Token (Sigil (Bracket Round Close)))
                , Located { end = { col = 26, row = 1 }, start = { col = 25, row = 1 } } (Token (Sigil Comma))
                , Located { end = { col = 27, row = 1 }, start = { col = 26, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 30, row = 1 }, start = { col = 27, row = 1 } } (Token (Identifier { name = TokenLowerCase (LowerCase "buy"), qualifiers = [] }))
                , Located { end = { col = 31, row = 1 }, start = { col = 30, row = 1 } } (Token (Sigil Colon))
                , Located { end = { col = 32, row = 1 }, start = { col = 31, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 38, row = 1 }, start = { col = 32, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "String"), qualifiers = [] }))
                , Located { end = { col = 39, row = 1 }, start = { col = 38, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 40, row = 1 }, start = { col = 39, row = 1 } } (Token (Sigil (Bracket Curly Close)))
                , Located { end = { col = 1, row = 2 }, start = { col = 40, row = 1 } } (Newlines [] 0)
                ]
      }
    , { name = "type-alias-unit"
      , source = """type alias Hi = ()
"""
      , pretty = """
        ( ( Ok
          , ( TypeAlias
            , ( ty, Hi )
            , ( genericArgs, () )
            , ( expr, Unit )
            )
          ) )
"""
      , contextualized =
            Just
                [ Ok (TypeAlias { expr = Unit, genericArgs = [], ty = UpperCase "Hi" })
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token (Keyword Type))
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token (Keyword Alias))
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Hi"), qualifiers = [] }))
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Token (Sigil Assign))
                , Located { end = { col = 17, row = 1 }, start = { col = 16, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Token (Sigil (Bracket Round Open)))
                , Located { end = { col = 19, row = 1 }, start = { col = 18, row = 1 } } (Token (Sigil (Bracket Round Close)))
                , Located { end = { col = 1, row = 2 }, start = { col = 19, row = 1 } } (Newlines [] 0)
                ]
      }
    , { name = "type-alias-with-bracket"
      , source = """type alias Hi = (Int)
"""
      , pretty = """
        ( ( Ok
          , ( TypeAlias
            , ( ty, Hi )
            , ( genericArgs, () )
            , ( expr
              , ( UserDefinedType
                , ( ( qualifiedness
                    , ( PossiblyQualified, Nothing )
                    )
                  , ( name, Int )
                  , ( args, () )
                  )
                )
              )
            )
          ) )
"""
      , contextualized =
            Just
                [ Ok
                    (TypeAlias
                        { expr =
                            UserDefinedType
                                { args = []
                                , name = "Int"
                                , qualifiedness = PossiblyQualified Nothing
                                }
                        , genericArgs = []
                        , ty = UpperCase "Hi"
                        }
                    )
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token (Keyword Type))
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token (Keyword Alias))
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Hi"), qualifiers = [] }))
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Token (Sigil Assign))
                , Located { end = { col = 17, row = 1 }, start = { col = 16, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Token (Sigil (Bracket Round Open)))
                , Located { end = { col = 21, row = 1 }, start = { col = 18, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Int"), qualifiers = [] }))
                , Located { end = { col = 22, row = 1 }, start = { col = 21, row = 1 } } (Token (Sigil (Bracket Round Close)))
                , Located { end = { col = 1, row = 2 }, start = { col = 22, row = 1 } } (Newlines [] 0)
                ]
      }
    , { name = "type-alias-with-bracket-2"
      , source = """type alias Hi = (List Int)
"""
      , pretty = """
        ( ( Ok
          , ( TypeAlias
            , ( ty, Hi )
            , ( genericArgs, () )
            , ( expr
              , ( UserDefinedType
                , ( ( qualifiedness
                    , ( PossiblyQualified, Nothing )
                    )
                  , ( name, List )
                  , ( args
                    , ( ( UserDefinedType
                        , ( ( qualifiedness
                            , ( PossiblyQualified, Nothing )
                            )
                          , ( name, Int )
                          , ( args, () )
                          )
                        ) )
                    )
                  )
                )
              )
            )
          ) )
"""
      , contextualized =
            Just
                [ Ok
                    (TypeAlias
                        { expr =
                            UserDefinedType
                                { args =
                                    [ UserDefinedType
                                        { args = []
                                        , name = "Int"
                                        , qualifiedness = PossiblyQualified Nothing
                                        }
                                    ]
                                , name = "List"
                                , qualifiedness = PossiblyQualified Nothing
                                }
                        , genericArgs = []
                        , ty = UpperCase "Hi"
                        }
                    )
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token (Keyword Type))
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token (Keyword Alias))
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Hi"), qualifiers = [] }))
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Token (Sigil Assign))
                , Located { end = { col = 17, row = 1 }, start = { col = 16, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Token (Sigil (Bracket Round Open)))
                , Located { end = { col = 22, row = 1 }, start = { col = 18, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "List"), qualifiers = [] }))
                , Located { end = { col = 23, row = 1 }, start = { col = 22, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 26, row = 1 }, start = { col = 23, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Int"), qualifiers = [] }))
                , Located { end = { col = 27, row = 1 }, start = { col = 26, row = 1 } } (Token (Sigil (Bracket Round Close)))
                , Located { end = { col = 1, row = 2 }, start = { col = 27, row = 1 } } (Newlines [] 0)
                ]
      }
    , { name = "type-alias-with-pair"
      , source = """type alias Hi = (Int, List String)
type alias Hi = (Int)
"""
      , pretty = """
        ( ( Ok
          , ( TypeAlias
            , ( ty, Hi )
            , ( genericArgs, () )
            , ( expr
              , ( Tuple
                , ( ( UserDefinedType
                    , ( ( qualifiedness
                        , ( PossiblyQualified, Nothing )
                        )
                      , ( name, Int )
                      , ( args, () )
                      )
                    )
                  , ( UserDefinedType
                    , ( ( qualifiedness
                        , ( PossiblyQualified, Nothing )
                        )
                      , ( name, List )
                      , ( args
                        , ( ( UserDefinedType
                            , ( ( qualifiedness
                                , ( PossiblyQualified, Nothing )
                                )
                              , ( name, String )
                              , ( args, () )
                              )
                            ) )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        , ( Ok
          , ( TypeAlias
            , ( ty, Hi )
            , ( genericArgs, () )
            , ( expr
              , ( UserDefinedType
                , ( ( qualifiedness
                    , ( PossiblyQualified, Nothing )
                    )
                  , ( name, Int )
                  , ( args, () )
                  )
                )
              )
            )
          )
        )
"""
      , contextualized =
            Just
                [ Ok
                    (TypeAlias
                        { expr =
                            Tuple
                                (UserDefinedType
                                    { args = []
                                    , name = "Int"
                                    , qualifiedness = PossiblyQualified Nothing
                                    }
                                )
                                (UserDefinedType
                                    { args =
                                        [ UserDefinedType
                                            { args = []
                                            , name = "String"
                                            , qualifiedness = PossiblyQualified Nothing
                                            }
                                        ]
                                    , name = "List"
                                    , qualifiedness = PossiblyQualified Nothing
                                    }
                                )
                        , genericArgs = []
                        , ty = UpperCase "Hi"
                        }
                    )
                , Ok
                    (TypeAlias
                        { expr =
                            UserDefinedType
                                { args = []
                                , name = "Int"
                                , qualifiedness = PossiblyQualified Nothing
                                }
                        , genericArgs = []
                        , ty = UpperCase "Hi"
                        }
                    )
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token (Keyword Type))
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token (Keyword Alias))
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Hi"), qualifiers = [] }))
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Token (Sigil Assign))
                , Located { end = { col = 17, row = 1 }, start = { col = 16, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Token (Sigil (Bracket Round Open)))
                , Located { end = { col = 21, row = 1 }, start = { col = 18, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Int"), qualifiers = [] }))
                , Located { end = { col = 22, row = 1 }, start = { col = 21, row = 1 } } (Token (Sigil Comma))
                , Located { end = { col = 23, row = 1 }, start = { col = 22, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 27, row = 1 }, start = { col = 23, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "List"), qualifiers = [] }))
                , Located { end = { col = 28, row = 1 }, start = { col = 27, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 34, row = 1 }, start = { col = 28, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "String"), qualifiers = [] }))
                , Located { end = { col = 35, row = 1 }, start = { col = 34, row = 1 } } (Token (Sigil (Bracket Round Close)))
                , Located { end = { col = 1, row = 2 }, start = { col = 35, row = 1 } } (Newlines [] 0)
                , Located { end = { col = 5, row = 2 }, start = { col = 1, row = 2 } } (Token (Keyword Type))
                , Located { end = { col = 6, row = 2 }, start = { col = 5, row = 2 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 11, row = 2 }, start = { col = 6, row = 2 } } (Token (Keyword Alias))
                , Located { end = { col = 12, row = 2 }, start = { col = 11, row = 2 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 14, row = 2 }, start = { col = 12, row = 2 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Hi"), qualifiers = [] }))
                , Located { end = { col = 15, row = 2 }, start = { col = 14, row = 2 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 16, row = 2 }, start = { col = 15, row = 2 } } (Token (Sigil Assign))
                , Located { end = { col = 17, row = 2 }, start = { col = 16, row = 2 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 18, row = 2 }, start = { col = 17, row = 2 } } (Token (Sigil (Bracket Round Open)))
                , Located { end = { col = 21, row = 2 }, start = { col = 18, row = 2 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Int"), qualifiers = [] }))
                , Located { end = { col = 22, row = 2 }, start = { col = 21, row = 2 } } (Token (Sigil (Bracket Round Close)))
                , Located { end = { col = 1, row = 3 }, start = { col = 22, row = 2 } } (Newlines [] 0)
                ]
      }
    , { name = "type-alias-with-tripple"
      , source = """type alias Hi = (Int, Two, Three)
type alias Hi = ((), (), ())
"""
      , pretty = """
        ( ( Ok
          , ( TypeAlias
            , ( ty, Hi )
            , ( genericArgs, () )
            , ( expr
              , ( Tuple
                , ( ( UserDefinedType
                    , ( ( qualifiedness
                        , ( PossiblyQualified, Nothing )
                        )
                      , ( name, Int )
                      , ( args, () )
                      )
                    )
                  , ( UserDefinedType
                    , ( ( qualifiedness
                        , ( PossiblyQualified, Nothing )
                        )
                      , ( name, Two )
                      , ( args, () )
                      )
                    )
                  , ( UserDefinedType
                    , ( ( qualifiedness
                        , ( PossiblyQualified, Nothing )
                        )
                      , ( name, Three )
                      , ( args, () )
                      )
                    )
                  )
                )
              )
            )
          )
        , ( Ok
          , ( TypeAlias
            , ( ty, Hi )
            , ( genericArgs, () )
            , ( expr
              , ( Tuple
                , ( Unit
                  , Unit
                  , Unit
                  )
                )
              )
            )
          )
        )
"""
      , contextualized =
            Just
                [ Ok
                    (TypeAlias
                        { expr =
                            Tuple3
                                (UserDefinedType
                                    { args = []
                                    , name = "Int"
                                    , qualifiedness = PossiblyQualified Nothing
                                    }
                                )
                                (UserDefinedType
                                    { args = []
                                    , name = "Two"
                                    , qualifiedness = PossiblyQualified Nothing
                                    }
                                )
                                (UserDefinedType
                                    { args = []
                                    , name = "Three"
                                    , qualifiedness = PossiblyQualified Nothing
                                    }
                                )
                        , genericArgs = []
                        , ty = UpperCase "Hi"
                        }
                    )
                , Ok (TypeAlias { expr = Tuple3 Unit Unit Unit, genericArgs = [], ty = UpperCase "Hi" })
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token (Keyword Type))
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token (Keyword Alias))
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Hi"), qualifiers = [] }))
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Token (Sigil Assign))
                , Located { end = { col = 17, row = 1 }, start = { col = 16, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Token (Sigil (Bracket Round Open)))
                , Located { end = { col = 21, row = 1 }, start = { col = 18, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Int"), qualifiers = [] }))
                , Located { end = { col = 22, row = 1 }, start = { col = 21, row = 1 } } (Token (Sigil Comma))
                , Located { end = { col = 23, row = 1 }, start = { col = 22, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 26, row = 1 }, start = { col = 23, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Two"), qualifiers = [] }))
                , Located { end = { col = 27, row = 1 }, start = { col = 26, row = 1 } } (Token (Sigil Comma))
                , Located { end = { col = 28, row = 1 }, start = { col = 27, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 33, row = 1 }, start = { col = 28, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Three"), qualifiers = [] }))
                , Located { end = { col = 34, row = 1 }, start = { col = 33, row = 1 } } (Token (Sigil (Bracket Round Close)))
                , Located { end = { col = 1, row = 2 }, start = { col = 34, row = 1 } } (Newlines [] 0)
                , Located { end = { col = 5, row = 2 }, start = { col = 1, row = 2 } } (Token (Keyword Type))
                , Located { end = { col = 6, row = 2 }, start = { col = 5, row = 2 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 11, row = 2 }, start = { col = 6, row = 2 } } (Token (Keyword Alias))
                , Located { end = { col = 12, row = 2 }, start = { col = 11, row = 2 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 14, row = 2 }, start = { col = 12, row = 2 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Hi"), qualifiers = [] }))
                , Located { end = { col = 15, row = 2 }, start = { col = 14, row = 2 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 16, row = 2 }, start = { col = 15, row = 2 } } (Token (Sigil Assign))
                , Located { end = { col = 17, row = 2 }, start = { col = 16, row = 2 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 18, row = 2 }, start = { col = 17, row = 2 } } (Token (Sigil (Bracket Round Open)))
                , Located { end = { col = 19, row = 2 }, start = { col = 18, row = 2 } } (Token (Sigil (Bracket Round Open)))
                , Located { end = { col = 20, row = 2 }, start = { col = 19, row = 2 } } (Token (Sigil (Bracket Round Close)))
                , Located { end = { col = 21, row = 2 }, start = { col = 20, row = 2 } } (Token (Sigil Comma))
                , Located { end = { col = 22, row = 2 }, start = { col = 21, row = 2 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 23, row = 2 }, start = { col = 22, row = 2 } } (Token (Sigil (Bracket Round Open)))
                , Located { end = { col = 24, row = 2 }, start = { col = 23, row = 2 } } (Token (Sigil (Bracket Round Close)))
                , Located { end = { col = 25, row = 2 }, start = { col = 24, row = 2 } } (Token (Sigil Comma))
                , Located { end = { col = 26, row = 2 }, start = { col = 25, row = 2 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 27, row = 2 }, start = { col = 26, row = 2 } } (Token (Sigil (Bracket Round Open)))
                , Located { end = { col = 28, row = 2 }, start = { col = 27, row = 2 } } (Token (Sigil (Bracket Round Close)))
                , Located { end = { col = 29, row = 2 }, start = { col = 28, row = 2 } } (Token (Sigil (Bracket Round Close)))
                , Located { end = { col = 1, row = 3 }, start = { col = 29, row = 2 } } (Newlines [] 0)
                ]
      }
    , { name = "type-alias-with-tripple-in-record"
      , source = """type alias Hi =
    { a: (Int, Int, Int)
    , b: ({ good_bye: () })
    }
"""
      , pretty = """
        ( ( Ok
          , ( TypeAlias
            , ( ty, Hi )
            , ( genericArgs, () )
            , ( expr
              , ( Record
                , ( ( a
                    , ( Tuple
                      , ( ( UserDefinedType
                          , ( ( qualifiedness
                              , ( PossiblyQualified, Nothing )
                              )
                            , ( name, Int )
                            , ( args, () )
                            )
                          )
                        , ( UserDefinedType
                          , ( ( qualifiedness
                              , ( PossiblyQualified, Nothing )
                              )
                            , ( name, Int )
                            , ( args, () )
                            )
                          )
                        , ( UserDefinedType
                          , ( ( qualifiedness
                              , ( PossiblyQualified, Nothing )
                              )
                            , ( name, Int )
                            , ( args, () )
                            )
                          )
                        )
                      )
                    )
                  , ( b
                    , ( Record
                      , ( ( good_bye, Unit ) )
                      )
                    )
                  )
                )
              )
            )
          ) )
"""
      , contextualized =
            Just
                [ Ok
                    (TypeAlias
                        { expr =
                            Record
                                (Dict.fromList
                                    [ ( "a"
                                      , Tuple3
                                            (UserDefinedType
                                                { args = []
                                                , name = "Int"
                                                , qualifiedness = PossiblyQualified Nothing
                                                }
                                            )
                                            (UserDefinedType
                                                { args = []
                                                , name = "Int"
                                                , qualifiedness = PossiblyQualified Nothing
                                                }
                                            )
                                            (UserDefinedType
                                                { args = []
                                                , name = "Int"
                                                , qualifiedness = PossiblyQualified Nothing
                                                }
                                            )
                                      )
                                    , ( "b"
                                      , Record
                                            (Dict.fromList
                                                [ ( "good_bye", Unit )
                                                ]
                                            )
                                      )
                                    ]
                                )
                        , genericArgs = []
                        , ty = UpperCase "Hi"
                        }
                    )
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token (Keyword Type))
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token (Keyword Alias))
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Hi"), qualifiers = [] }))
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Token (Sigil Assign))
                , Located { end = { col = 5, row = 2 }, start = { col = 16, row = 1 } } (Newlines [] 4)
                , Located { end = { col = 6, row = 2 }, start = { col = 5, row = 2 } } (Token (Sigil (Bracket Curly Open)))
                , Located { end = { col = 7, row = 2 }, start = { col = 6, row = 2 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 8, row = 2 }, start = { col = 7, row = 2 } } (Token (Identifier { name = TokenLowerCase (LowerCase "a"), qualifiers = [] }))
                , Located { end = { col = 9, row = 2 }, start = { col = 8, row = 2 } } (Token (Sigil Colon))
                , Located { end = { col = 10, row = 2 }, start = { col = 9, row = 2 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 11, row = 2 }, start = { col = 10, row = 2 } } (Token (Sigil (Bracket Round Open)))
                , Located { end = { col = 14, row = 2 }, start = { col = 11, row = 2 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Int"), qualifiers = [] }))
                , Located { end = { col = 15, row = 2 }, start = { col = 14, row = 2 } } (Token (Sigil Comma))
                , Located { end = { col = 16, row = 2 }, start = { col = 15, row = 2 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 19, row = 2 }, start = { col = 16, row = 2 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Int"), qualifiers = [] }))
                , Located { end = { col = 20, row = 2 }, start = { col = 19, row = 2 } } (Token (Sigil Comma))
                , Located { end = { col = 21, row = 2 }, start = { col = 20, row = 2 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 24, row = 2 }, start = { col = 21, row = 2 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Int"), qualifiers = [] }))
                , Located { end = { col = 25, row = 2 }, start = { col = 24, row = 2 } } (Token (Sigil (Bracket Round Close)))
                , Located { end = { col = 5, row = 3 }, start = { col = 25, row = 2 } } (Newlines [] 4)
                , Located { end = { col = 6, row = 3 }, start = { col = 5, row = 3 } } (Token (Sigil Comma))
                , Located { end = { col = 7, row = 3 }, start = { col = 6, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 8, row = 3 }, start = { col = 7, row = 3 } } (Token (Identifier { name = TokenLowerCase (LowerCase "b"), qualifiers = [] }))
                , Located { end = { col = 9, row = 3 }, start = { col = 8, row = 3 } } (Token (Sigil Colon))
                , Located { end = { col = 10, row = 3 }, start = { col = 9, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 11, row = 3 }, start = { col = 10, row = 3 } } (Token (Sigil (Bracket Round Open)))
                , Located { end = { col = 12, row = 3 }, start = { col = 11, row = 3 } } (Token (Sigil (Bracket Curly Open)))
                , Located { end = { col = 13, row = 3 }, start = { col = 12, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 21, row = 3 }, start = { col = 13, row = 3 } } (Token (Identifier { name = TokenLowerCase (LowerCase "good_bye"), qualifiers = [] }))
                , Located { end = { col = 22, row = 3 }, start = { col = 21, row = 3 } } (Token (Sigil Colon))
                , Located { end = { col = 23, row = 3 }, start = { col = 22, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 24, row = 3 }, start = { col = 23, row = 3 } } (Token (Sigil (Bracket Round Open)))
                , Located { end = { col = 25, row = 3 }, start = { col = 24, row = 3 } } (Token (Sigil (Bracket Round Close)))
                , Located { end = { col = 26, row = 3 }, start = { col = 25, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 27, row = 3 }, start = { col = 26, row = 3 } } (Token (Sigil (Bracket Curly Close)))
                , Located { end = { col = 28, row = 3 }, start = { col = 27, row = 3 } } (Token (Sigil (Bracket Round Close)))
                , Located { end = { col = 5, row = 4 }, start = { col = 28, row = 3 } } (Newlines [] 4)
                , Located { end = { col = 6, row = 4 }, start = { col = 5, row = 4 } } (Token (Sigil (Bracket Curly Close)))
                , Located { end = { col = 1, row = 5 }, start = { col = 6, row = 4 } } (Newlines [] 0)
                ]
      }
    ]


shouldNotParseTestCases :
    List
        { contextualized : Maybe (List Contextualize.RunResult)
        , pretty : String
        , lexed : Result Never (List (Located LexItem))
        , name : String
        , source : String
        }
shouldNotParseTestCases =
    [ { name = "dots"
      , source = """Foo.

Foo.Bar

Foo.Bar.baz

Boor.Bing.

Bor. Big.

.sf

sfsdf .sdfsd

asfasf.sdgsghj

(shdf).helloLiteral

(sjhsf) .helloFunction

sfhsdf(.helloFunction)

case.hi

Hi.case

case .hi

Hi. case"""
      , pretty = """
        ( ( Err, todo )
        , ( Err, todo )
        , ( Err, todo )
        , ( Err, todo )
        , ( Err, todo )
        , ( Err, todo )
        , ( Err, todo )
        , ( Err, todo )
        , ( Err, todo )
        , ( Err, todo )
        , ( Err, todo )
        , ( Err, todo )
        , ( Err, todo )
        , ( Err, todo )
        , ( Err, todo )
        , ( Err, todo )
        )
"""
      , contextualized =
            Just
                [ Err
                    { error = Error_InvalidToken Expecting_Unknown
                    , item = Just (Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Invalid (IdentifierWithTrailingDot { name = UpperCase "Foo", qualifiers = [] })))
                    , state = State_BlockStart
                    }
                , Err
                    { error =
                        Error_BlockStartsWithQualifiedName
                            { name = TokenUpperCase (UpperCase "Bar")
                            , qualifiers =
                                [ UpperCase "Foo"
                                ]
                            }
                    , item =
                        Just
                            (Located { end = { col = 8, row = 3 }, start = { col = 1, row = 3 } }
                                (Token
                                    (Identifier
                                        { name = TokenUpperCase (UpperCase "Bar")
                                        , qualifiers =
                                            [ UpperCase "Foo"
                                            ]
                                        }
                                    )
                                )
                            )
                    , state = State_BlockStart
                    }
                , Err
                    { error =
                        Error_BlockStartsWithQualifiedName
                            { name = TokenLowerCase (LowerCase "baz")
                            , qualifiers =
                                [ UpperCase "Foo"
                                , UpperCase "Bar"
                                ]
                            }
                    , item =
                        Just
                            (Located { end = { col = 12, row = 5 }, start = { col = 1, row = 5 } }
                                (Token
                                    (Identifier
                                        { name = TokenLowerCase (LowerCase "baz")
                                        , qualifiers =
                                            [ UpperCase "Foo"
                                            , UpperCase "Bar"
                                            ]
                                        }
                                    )
                                )
                            )
                    , state = State_BlockStart
                    }
                , Err
                    { error = Error_InvalidToken Expecting_Unknown
                    , item =
                        Just
                            (Located { end = { col = 11, row = 7 }, start = { col = 1, row = 7 } }
                                (Invalid
                                    (IdentifierWithTrailingDot
                                        { name = UpperCase "Bing"
                                        , qualifiers =
                                            [ UpperCase "Boor"
                                            ]
                                        }
                                    )
                                )
                            )
                    , state = State_BlockStart
                    }
                , Err
                    { error = Error_InvalidToken Expecting_Unknown
                    , item = Just (Located { end = { col = 5, row = 9 }, start = { col = 1, row = 9 } } (Invalid (IdentifierWithTrailingDot { name = UpperCase "Bor", qualifiers = [] })))
                    , state = State_BlockStart
                    }
                , Err
                    { error = Error_InvalidToken Expecting_Unknown
                    , item = Just (Located { end = { col = 10, row = 9 }, start = { col = 6, row = 9 } } (Invalid (IdentifierWithTrailingDot { name = UpperCase "Big", qualifiers = [] })))
                    , state = State_Error_Recovery
                    }
                , Err
                    { error = Error_InvalidToken Expecting_Block
                    , item = Just (Located { end = { col = 4, row = 11 }, start = { col = 1, row = 11 } } (Token (RecordAccessorFunction (LowerCase "sf"))))
                    , state = State_BlockStart
                    }
                , Err
                    { error = Error_InvalidToken (Expecting_Sigil Assign)
                    , item = Just (Located { end = { col = 13, row = 13 }, start = { col = 7, row = 13 } } (Token (RecordAccessorFunction (LowerCase "sdfsd"))))
                    , state = State_BlockValueDeclaration (BlockValueDeclaration_Named { args = Stack [], name = Located { end = { col = 6, row = 13 }, start = { col = 1, row = 13 } } (LowerCase "sfsdf") })
                    }
                , Err
                    { error = Error_InvalidToken (Expecting_Sigil Assign)
                    , item = Just (Located { end = { col = 15, row = 15 }, start = { col = 7, row = 15 } } (Token (RecordAccessorLiteral (LowerCase "sdgsghj"))))
                    , state = State_BlockValueDeclaration (BlockValueDeclaration_Named { args = Stack [], name = Located { end = { col = 7, row = 15 }, start = { col = 1, row = 15 } } (LowerCase "asfasf") })
                    }
                , Err
                    { error = Error_InvalidToken Expecting_Block
                    , item = Just (Located { end = { col = 2, row = 17 }, start = { col = 1, row = 17 } } (Token (Sigil (Bracket Round Open))))
                    , state = State_BlockStart
                    }
                , Err
                    { error = Error_InvalidToken Expecting_Block
                    , item = Just (Located { end = { col = 2, row = 19 }, start = { col = 1, row = 19 } } (Token (Sigil (Bracket Round Open))))
                    , state = State_BlockStart
                    }
                , Err
                    { error = Error_InvalidToken (Expecting_Sigil Assign)
                    , item = Just (Located { end = { col = 8, row = 21 }, start = { col = 7, row = 21 } } (Token (Sigil (Bracket Round Open))))
                    , state = State_BlockValueDeclaration (BlockValueDeclaration_Named { args = Stack [], name = Located { end = { col = 7, row = 21 }, start = { col = 1, row = 21 } } (LowerCase "sfhsdf") })
                    }
                , Err
                    { error = Error_MisplacedKeyword Case
                    , item = Just (Located { end = { col = 5, row = 23 }, start = { col = 1, row = 23 } } (Token (Keyword Case)))
                    , state = State_BlockStart
                    }
                , Err
                    { error =
                        Error_BlockStartsWithQualifiedName
                            { name = TokenLowerCase (LowerCase "case")
                            , qualifiers =
                                [ UpperCase "Hi"
                                ]
                            }
                    , item =
                        Just
                            (Located { end = { col = 8, row = 25 }, start = { col = 1, row = 25 } }
                                (Token
                                    (Identifier
                                        { name = TokenLowerCase (LowerCase "case")
                                        , qualifiers =
                                            [ UpperCase "Hi"
                                            ]
                                        }
                                    )
                                )
                            )
                    , state = State_BlockStart
                    }
                , Err
                    { error = Error_MisplacedKeyword Case
                    , item = Just (Located { end = { col = 5, row = 27 }, start = { col = 1, row = 27 } } (Token (Keyword Case)))
                    , state = State_BlockStart
                    }
                , Err
                    { error = Error_InvalidToken Expecting_Unknown
                    , item = Just (Located { end = { col = 4, row = 29 }, start = { col = 1, row = 29 } } (Invalid (IdentifierWithTrailingDot { name = UpperCase "Hi", qualifiers = [] })))
                    , state = State_BlockStart
                    }
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Invalid (IdentifierWithTrailingDot { name = UpperCase "Foo", qualifiers = [] }))
                , Located { end = { col = 1, row = 3 }, start = { col = 5, row = 1 } }
                    (Newlines
                        [ 0
                        ]
                        0
                    )
                , Located { end = { col = 8, row = 3 }, start = { col = 1, row = 3 } }
                    (Token
                        (Identifier
                            { name = TokenUpperCase (UpperCase "Bar")
                            , qualifiers =
                                [ UpperCase "Foo"
                                ]
                            }
                        )
                    )
                , Located { end = { col = 1, row = 5 }, start = { col = 8, row = 3 } }
                    (Newlines
                        [ 0
                        ]
                        0
                    )
                , Located { end = { col = 12, row = 5 }, start = { col = 1, row = 5 } }
                    (Token
                        (Identifier
                            { name = TokenLowerCase (LowerCase "baz")
                            , qualifiers =
                                [ UpperCase "Foo"
                                , UpperCase "Bar"
                                ]
                            }
                        )
                    )
                , Located { end = { col = 1, row = 7 }, start = { col = 12, row = 5 } }
                    (Newlines
                        [ 0
                        ]
                        0
                    )
                , Located { end = { col = 11, row = 7 }, start = { col = 1, row = 7 } }
                    (Invalid
                        (IdentifierWithTrailingDot
                            { name = UpperCase "Bing"
                            , qualifiers =
                                [ UpperCase "Boor"
                                ]
                            }
                        )
                    )
                , Located { end = { col = 1, row = 9 }, start = { col = 11, row = 7 } }
                    (Newlines
                        [ 0
                        ]
                        0
                    )
                , Located { end = { col = 5, row = 9 }, start = { col = 1, row = 9 } } (Invalid (IdentifierWithTrailingDot { name = UpperCase "Bor", qualifiers = [] }))
                , Located { end = { col = 6, row = 9 }, start = { col = 5, row = 9 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 10, row = 9 }, start = { col = 6, row = 9 } } (Invalid (IdentifierWithTrailingDot { name = UpperCase "Big", qualifiers = [] }))
                , Located { end = { col = 1, row = 11 }, start = { col = 10, row = 9 } }
                    (Newlines
                        [ 0
                        ]
                        0
                    )
                , Located { end = { col = 4, row = 11 }, start = { col = 1, row = 11 } } (Token (RecordAccessorFunction (LowerCase "sf")))
                , Located { end = { col = 1, row = 13 }, start = { col = 4, row = 11 } }
                    (Newlines
                        [ 0
                        ]
                        0
                    )
                , Located { end = { col = 6, row = 13 }, start = { col = 1, row = 13 } } (Token (Identifier { name = TokenLowerCase (LowerCase "sfsdf"), qualifiers = [] }))
                , Located { end = { col = 7, row = 13 }, start = { col = 6, row = 13 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 13, row = 13 }, start = { col = 7, row = 13 } } (Token (RecordAccessorFunction (LowerCase "sdfsd")))
                , Located { end = { col = 1, row = 15 }, start = { col = 13, row = 13 } }
                    (Newlines
                        [ 0
                        ]
                        0
                    )
                , Located { end = { col = 7, row = 15 }, start = { col = 1, row = 15 } } (Token (Identifier { name = TokenLowerCase (LowerCase "asfasf"), qualifiers = [] }))
                , Located { end = { col = 15, row = 15 }, start = { col = 7, row = 15 } } (Token (RecordAccessorLiteral (LowerCase "sdgsghj")))
                , Located { end = { col = 1, row = 17 }, start = { col = 15, row = 15 } }
                    (Newlines
                        [ 0
                        ]
                        0
                    )
                , Located { end = { col = 2, row = 17 }, start = { col = 1, row = 17 } } (Token (Sigil (Bracket Round Open)))
                , Located { end = { col = 6, row = 17 }, start = { col = 2, row = 17 } } (Token (Identifier { name = TokenLowerCase (LowerCase "shdf"), qualifiers = [] }))
                , Located { end = { col = 7, row = 17 }, start = { col = 6, row = 17 } } (Token (Sigil (Bracket Round Close)))
                , Located { end = { col = 20, row = 17 }, start = { col = 7, row = 17 } } (Token (RecordAccessorLiteral (LowerCase "helloLiteral")))
                , Located { end = { col = 1, row = 19 }, start = { col = 20, row = 17 } }
                    (Newlines
                        [ 0
                        ]
                        0
                    )
                , Located { end = { col = 2, row = 19 }, start = { col = 1, row = 19 } } (Token (Sigil (Bracket Round Open)))
                , Located { end = { col = 7, row = 19 }, start = { col = 2, row = 19 } } (Token (Identifier { name = TokenLowerCase (LowerCase "sjhsf"), qualifiers = [] }))
                , Located { end = { col = 8, row = 19 }, start = { col = 7, row = 19 } } (Token (Sigil (Bracket Round Close)))
                , Located { end = { col = 9, row = 19 }, start = { col = 8, row = 19 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 23, row = 19 }, start = { col = 9, row = 19 } } (Token (RecordAccessorFunction (LowerCase "helloFunction")))
                , Located { end = { col = 1, row = 21 }, start = { col = 23, row = 19 } }
                    (Newlines
                        [ 0
                        ]
                        0
                    )
                , Located { end = { col = 7, row = 21 }, start = { col = 1, row = 21 } } (Token (Identifier { name = TokenLowerCase (LowerCase "sfhsdf"), qualifiers = [] }))
                , Located { end = { col = 8, row = 21 }, start = { col = 7, row = 21 } } (Token (Sigil (Bracket Round Open)))
                , Located { end = { col = 22, row = 21 }, start = { col = 8, row = 21 } } (Token (RecordAccessorFunction (LowerCase "helloFunction")))
                , Located { end = { col = 23, row = 21 }, start = { col = 22, row = 21 } } (Token (Sigil (Bracket Round Close)))
                , Located { end = { col = 1, row = 23 }, start = { col = 23, row = 21 } }
                    (Newlines
                        [ 0
                        ]
                        0
                    )
                , Located { end = { col = 5, row = 23 }, start = { col = 1, row = 23 } } (Token (Keyword Case))
                , Located { end = { col = 8, row = 23 }, start = { col = 5, row = 23 } } (Token (RecordAccessorFunction (LowerCase "hi")))
                , Located { end = { col = 1, row = 25 }, start = { col = 8, row = 23 } }
                    (Newlines
                        [ 0
                        ]
                        0
                    )
                , Located { end = { col = 8, row = 25 }, start = { col = 1, row = 25 } }
                    (Token
                        (Identifier
                            { name = TokenLowerCase (LowerCase "case")
                            , qualifiers =
                                [ UpperCase "Hi"
                                ]
                            }
                        )
                    )
                , Located { end = { col = 1, row = 27 }, start = { col = 8, row = 25 } }
                    (Newlines
                        [ 0
                        ]
                        0
                    )
                , Located { end = { col = 5, row = 27 }, start = { col = 1, row = 27 } } (Token (Keyword Case))
                , Located { end = { col = 6, row = 27 }, start = { col = 5, row = 27 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 9, row = 27 }, start = { col = 6, row = 27 } } (Token (RecordAccessorFunction (LowerCase "hi")))
                , Located { end = { col = 1, row = 29 }, start = { col = 9, row = 27 } }
                    (Newlines
                        [ 0
                        ]
                        0
                    )
                , Located { end = { col = 4, row = 29 }, start = { col = 1, row = 29 } } (Invalid (IdentifierWithTrailingDot { name = UpperCase "Hi", qualifiers = [] }))
                , Located { end = { col = 5, row = 29 }, start = { col = 4, row = 29 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 9, row = 29 }, start = { col = 5, row = 29 } } (Token (Keyword Case))
                ]
      }
    , { name = "type-alias-function-nested-missing-return"
      , source = """type alias Function = (() -> )

type alias Function2 = { a: () ->  }

type alias Function3 = (() -> , ())

type alias Function3 = (Int, () ->, ())
"""
      , pretty = """
        ( ( Err, todo )
        , ( Err, todo )
        , ( Err, todo )
        , ( Err, todo )
        )
"""
      , contextualized =
            Just
                [ Err
                    { error = Error_MissingFunctionReturnType
                    , item = Just (Located { end = { col = 31, row = 1 }, start = { col = 30, row = 1 } } (Token (Sigil (Bracket Round Close))))
                    , state = State_BlockTypeAlias (BlockTypeAlias_Completish (UpperCase "Function") [] (TypeExpressionNestingLeaf_Function { firstInput = TypeExpression_Unit, otherInputs = Stack [], output = Nothing, parent = Just (NestingParentType_Bracket { expressions = Stack [], parent = Nothing }) }))
                    }
                , Err
                    { error = Error_MissingFunctionReturnType
                    , item = Just (Located { end = { col = 37, row = 3 }, start = { col = 36, row = 3 } } (Token (Sigil (Bracket Curly Close))))
                    , state = State_BlockTypeAlias (BlockTypeAlias_Completish (UpperCase "Function2") [] (TypeExpressionNestingLeaf_Function { firstInput = TypeExpression_Unit, otherInputs = Stack [], output = Nothing, parent = Just (NestingParentType_PartialRecord { firstEntries = Stack [], lastEntryName = LowerCase "a", parent = Nothing }) }))
                    }
                , Err
                    { error = Error_InvalidToken Expecting_Unknown
                    , item = Just (Located { end = { col = 32, row = 5 }, start = { col = 31, row = 5 } } (Token (Sigil Comma)))
                    , state = State_BlockTypeAlias (BlockTypeAlias_Completish (UpperCase "Function3") [] (TypeExpressionNestingLeaf_Function { firstInput = TypeExpression_Unit, otherInputs = Stack [], output = Nothing, parent = Just (NestingParentType_Bracket { expressions = Stack [], parent = Nothing }) }))
                    }
                , Err
                    { error = Error_InvalidToken Expecting_Unknown
                    , item = Just (Located { end = { col = 36, row = 7 }, start = { col = 35, row = 7 } } (Token (Sigil Comma)))
                    , state =
                        State_BlockTypeAlias
                            (BlockTypeAlias_Completish (UpperCase "Function3")
                                []
                                (TypeExpressionNestingLeaf_Function
                                    { firstInput = TypeExpression_Unit
                                    , otherInputs = Stack []
                                    , output = Nothing
                                    , parent =
                                        Just
                                            (NestingParentType_Bracket
                                                { expressions =
                                                    Stack
                                                        [ TypeExpression_NamedType { args = Stack [], name = UpperCase "Int", qualifiers = [] }
                                                        ]
                                                , parent = Nothing
                                                }
                                            )
                                    }
                                )
                            )
                    }
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token (Keyword Type))
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token (Keyword Alias))
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 20, row = 1 }, start = { col = 12, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Function"), qualifiers = [] }))
                , Located { end = { col = 21, row = 1 }, start = { col = 20, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 22, row = 1 }, start = { col = 21, row = 1 } } (Token (Sigil Assign))
                , Located { end = { col = 23, row = 1 }, start = { col = 22, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 24, row = 1 }, start = { col = 23, row = 1 } } (Token (Sigil (Bracket Round Open)))
                , Located { end = { col = 25, row = 1 }, start = { col = 24, row = 1 } } (Token (Sigil (Bracket Round Open)))
                , Located { end = { col = 26, row = 1 }, start = { col = 25, row = 1 } } (Token (Sigil (Bracket Round Close)))
                , Located { end = { col = 27, row = 1 }, start = { col = 26, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 29, row = 1 }, start = { col = 27, row = 1 } } (Token (Sigil ThinArrow))
                , Located { end = { col = 30, row = 1 }, start = { col = 29, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 31, row = 1 }, start = { col = 30, row = 1 } } (Token (Sigil (Bracket Round Close)))
                , Located { end = { col = 1, row = 3 }, start = { col = 31, row = 1 } }
                    (Newlines
                        [ 0
                        ]
                        0
                    )
                , Located { end = { col = 5, row = 3 }, start = { col = 1, row = 3 } } (Token (Keyword Type))
                , Located { end = { col = 6, row = 3 }, start = { col = 5, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 11, row = 3 }, start = { col = 6, row = 3 } } (Token (Keyword Alias))
                , Located { end = { col = 12, row = 3 }, start = { col = 11, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 21, row = 3 }, start = { col = 12, row = 3 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Function2"), qualifiers = [] }))
                , Located { end = { col = 22, row = 3 }, start = { col = 21, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 23, row = 3 }, start = { col = 22, row = 3 } } (Token (Sigil Assign))
                , Located { end = { col = 24, row = 3 }, start = { col = 23, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 25, row = 3 }, start = { col = 24, row = 3 } } (Token (Sigil (Bracket Curly Open)))
                , Located { end = { col = 26, row = 3 }, start = { col = 25, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 27, row = 3 }, start = { col = 26, row = 3 } } (Token (Identifier { name = TokenLowerCase (LowerCase "a"), qualifiers = [] }))
                , Located { end = { col = 28, row = 3 }, start = { col = 27, row = 3 } } (Token (Sigil Colon))
                , Located { end = { col = 29, row = 3 }, start = { col = 28, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 30, row = 3 }, start = { col = 29, row = 3 } } (Token (Sigil (Bracket Round Open)))
                , Located { end = { col = 31, row = 3 }, start = { col = 30, row = 3 } } (Token (Sigil (Bracket Round Close)))
                , Located { end = { col = 32, row = 3 }, start = { col = 31, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 34, row = 3 }, start = { col = 32, row = 3 } } (Token (Sigil ThinArrow))
                , Located { end = { col = 36, row = 3 }, start = { col = 34, row = 3 } } (Ignorable (Whitespace 2))
                , Located { end = { col = 37, row = 3 }, start = { col = 36, row = 3 } } (Token (Sigil (Bracket Curly Close)))
                , Located { end = { col = 1, row = 5 }, start = { col = 37, row = 3 } }
                    (Newlines
                        [ 0
                        ]
                        0
                    )
                , Located { end = { col = 5, row = 5 }, start = { col = 1, row = 5 } } (Token (Keyword Type))
                , Located { end = { col = 6, row = 5 }, start = { col = 5, row = 5 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 11, row = 5 }, start = { col = 6, row = 5 } } (Token (Keyword Alias))
                , Located { end = { col = 12, row = 5 }, start = { col = 11, row = 5 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 21, row = 5 }, start = { col = 12, row = 5 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Function3"), qualifiers = [] }))
                , Located { end = { col = 22, row = 5 }, start = { col = 21, row = 5 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 23, row = 5 }, start = { col = 22, row = 5 } } (Token (Sigil Assign))
                , Located { end = { col = 24, row = 5 }, start = { col = 23, row = 5 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 25, row = 5 }, start = { col = 24, row = 5 } } (Token (Sigil (Bracket Round Open)))
                , Located { end = { col = 26, row = 5 }, start = { col = 25, row = 5 } } (Token (Sigil (Bracket Round Open)))
                , Located { end = { col = 27, row = 5 }, start = { col = 26, row = 5 } } (Token (Sigil (Bracket Round Close)))
                , Located { end = { col = 28, row = 5 }, start = { col = 27, row = 5 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 30, row = 5 }, start = { col = 28, row = 5 } } (Token (Sigil ThinArrow))
                , Located { end = { col = 31, row = 5 }, start = { col = 30, row = 5 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 32, row = 5 }, start = { col = 31, row = 5 } } (Token (Sigil Comma))
                , Located { end = { col = 33, row = 5 }, start = { col = 32, row = 5 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 34, row = 5 }, start = { col = 33, row = 5 } } (Token (Sigil (Bracket Round Open)))
                , Located { end = { col = 35, row = 5 }, start = { col = 34, row = 5 } } (Token (Sigil (Bracket Round Close)))
                , Located { end = { col = 36, row = 5 }, start = { col = 35, row = 5 } } (Token (Sigil (Bracket Round Close)))
                , Located { end = { col = 1, row = 7 }, start = { col = 36, row = 5 } }
                    (Newlines
                        [ 0
                        ]
                        0
                    )
                , Located { end = { col = 5, row = 7 }, start = { col = 1, row = 7 } } (Token (Keyword Type))
                , Located { end = { col = 6, row = 7 }, start = { col = 5, row = 7 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 11, row = 7 }, start = { col = 6, row = 7 } } (Token (Keyword Alias))
                , Located { end = { col = 12, row = 7 }, start = { col = 11, row = 7 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 21, row = 7 }, start = { col = 12, row = 7 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Function3"), qualifiers = [] }))
                , Located { end = { col = 22, row = 7 }, start = { col = 21, row = 7 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 23, row = 7 }, start = { col = 22, row = 7 } } (Token (Sigil Assign))
                , Located { end = { col = 24, row = 7 }, start = { col = 23, row = 7 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 25, row = 7 }, start = { col = 24, row = 7 } } (Token (Sigil (Bracket Round Open)))
                , Located { end = { col = 28, row = 7 }, start = { col = 25, row = 7 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Int"), qualifiers = [] }))
                , Located { end = { col = 29, row = 7 }, start = { col = 28, row = 7 } } (Token (Sigil Comma))
                , Located { end = { col = 30, row = 7 }, start = { col = 29, row = 7 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 31, row = 7 }, start = { col = 30, row = 7 } } (Token (Sigil (Bracket Round Open)))
                , Located { end = { col = 32, row = 7 }, start = { col = 31, row = 7 } } (Token (Sigil (Bracket Round Close)))
                , Located { end = { col = 33, row = 7 }, start = { col = 32, row = 7 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 35, row = 7 }, start = { col = 33, row = 7 } } (Token (Sigil ThinArrow))
                , Located { end = { col = 36, row = 7 }, start = { col = 35, row = 7 } } (Token (Sigil Comma))
                , Located { end = { col = 37, row = 7 }, start = { col = 36, row = 7 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 38, row = 7 }, start = { col = 37, row = 7 } } (Token (Sigil (Bracket Round Open)))
                , Located { end = { col = 39, row = 7 }, start = { col = 38, row = 7 } } (Token (Sigil (Bracket Round Close)))
                , Located { end = { col = 40, row = 7 }, start = { col = 39, row = 7 } } (Token (Sigil (Bracket Round Close)))
                , Located { end = { col = 1, row = 8 }, start = { col = 40, row = 7 } } (Newlines [] 0)
                ]
      }
    , { name = "type-alias-invalid-multiple-brackets"
      , source = """type alias Hi = (Int) ()
"""
      , pretty = """
        ( ( Err, todo ) )
"""
      , contextualized =
            Just
                [ Err
                    { error = Error_TypeDoesNotTakeArgs2 (TypeExpression_Bracketed (TypeExpression_NamedType { args = Stack [], name = UpperCase "Int", qualifiers = [] }))
                    , item = Just (Located { end = { col = 24, row = 1 }, start = { col = 23, row = 1 } } (Token (Sigil (Bracket Round Open))))
                    , state = State_BlockTypeAlias (BlockTypeAlias_Completish (UpperCase "Hi") [] (TypeExpressionNestingLeaf_Expr (TypeExpression_Bracketed (TypeExpression_NamedType { args = Stack [], name = UpperCase "Int", qualifiers = [] }))))
                    }
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token (Keyword Type))
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token (Keyword Alias))
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Hi"), qualifiers = [] }))
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Token (Sigil Assign))
                , Located { end = { col = 17, row = 1 }, start = { col = 16, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Token (Sigil (Bracket Round Open)))
                , Located { end = { col = 21, row = 1 }, start = { col = 18, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Int"), qualifiers = [] }))
                , Located { end = { col = 22, row = 1 }, start = { col = 21, row = 1 } } (Token (Sigil (Bracket Round Close)))
                , Located { end = { col = 23, row = 1 }, start = { col = 22, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 24, row = 1 }, start = { col = 23, row = 1 } } (Token (Sigil (Bracket Round Open)))
                , Located { end = { col = 25, row = 1 }, start = { col = 24, row = 1 } } (Token (Sigil (Bracket Round Close)))
                , Located { end = { col = 1, row = 2 }, start = { col = 25, row = 1 } } (Newlines [] 0)
                ]
      }
    , { name = "type-alias-invalid-multiple-brackets-2"
      , source = """type alias Hi = () ()
"""
      , pretty = """
        ( ( Err, todo ) )
"""
      , contextualized =
            Just
                [ Err
                    { error = Error_TypeDoesNotTakeArgs2 TypeExpression_Unit
                    , item = Just (Located { end = { col = 21, row = 1 }, start = { col = 20, row = 1 } } (Token (Sigil (Bracket Round Open))))
                    , state = State_BlockTypeAlias (BlockTypeAlias_Completish (UpperCase "Hi") [] (TypeExpressionNestingLeaf_Expr TypeExpression_Unit))
                    }
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token (Keyword Type))
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token (Keyword Alias))
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Hi"), qualifiers = [] }))
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Token (Sigil Assign))
                , Located { end = { col = 17, row = 1 }, start = { col = 16, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Token (Sigil (Bracket Round Open)))
                , Located { end = { col = 19, row = 1 }, start = { col = 18, row = 1 } } (Token (Sigil (Bracket Round Close)))
                , Located { end = { col = 20, row = 1 }, start = { col = 19, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 21, row = 1 }, start = { col = 20, row = 1 } } (Token (Sigil (Bracket Round Open)))
                , Located { end = { col = 22, row = 1 }, start = { col = 21, row = 1 } } (Token (Sigil (Bracket Round Close)))
                , Located { end = { col = 1, row = 2 }, start = { col = 22, row = 1 } } (Newlines [] 0)
                ]
      }
    , { name = "type-alias-invalid-multiple-brackets-3"
      , source = """type alias Hi = () (Int)
"""
      , pretty = """
        ( ( Err, todo ) )
"""
      , contextualized =
            Just
                [ Err
                    { error = Error_TypeDoesNotTakeArgs2 TypeExpression_Unit
                    , item = Just (Located { end = { col = 21, row = 1 }, start = { col = 20, row = 1 } } (Token (Sigil (Bracket Round Open))))
                    , state = State_BlockTypeAlias (BlockTypeAlias_Completish (UpperCase "Hi") [] (TypeExpressionNestingLeaf_Expr TypeExpression_Unit))
                    }
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token (Keyword Type))
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token (Keyword Alias))
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Hi"), qualifiers = [] }))
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Token (Sigil Assign))
                , Located { end = { col = 17, row = 1 }, start = { col = 16, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Token (Sigil (Bracket Round Open)))
                , Located { end = { col = 19, row = 1 }, start = { col = 18, row = 1 } } (Token (Sigil (Bracket Round Close)))
                , Located { end = { col = 20, row = 1 }, start = { col = 19, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 21, row = 1 }, start = { col = 20, row = 1 } } (Token (Sigil (Bracket Round Open)))
                , Located { end = { col = 24, row = 1 }, start = { col = 21, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Int"), qualifiers = [] }))
                , Located { end = { col = 25, row = 1 }, start = { col = 24, row = 1 } } (Token (Sigil (Bracket Round Close)))
                , Located { end = { col = 1, row = 2 }, start = { col = 25, row = 1 } } (Newlines [] 0)
                ]
      }
    , { name = "type-alias-lower-case-types"
      , source = """type alias A = B.C.d

type alias B = List A.d

type alias C = list A

type alias D a = a -> B.c
type alias E a = B.c -> a"""
      , pretty = """
        ( ( Err, todo )
        , ( Err, todo )
        , ( Err, todo )
        , ( Err, todo )
        , ( Err, todo )
        )
"""
      , contextualized =
            Just
                [ Err
                    { error =
                        Error_LowerCasedTypename
                            { name = LowerCase "d"
                            , qualifiers =
                                [ UpperCase "B"
                                , UpperCase "C"
                                ]
                            }
                    , item =
                        Just
                            (Located { end = { col = 21, row = 1 }, start = { col = 16, row = 1 } }
                                (Token
                                    (Identifier
                                        { name = TokenLowerCase (LowerCase "d")
                                        , qualifiers =
                                            [ UpperCase "B"
                                            , UpperCase "C"
                                            ]
                                        }
                                    )
                                )
                            )
                    , state = State_BlockTypeAlias (BlockTypeAlias_NamedAssigns (UpperCase "A") [])
                    }
                , Err
                    { error =
                        Error_LowerCasedTypename
                            { name = LowerCase "d"
                            , qualifiers =
                                [ UpperCase "A"
                                ]
                            }
                    , item =
                        Just
                            (Located { end = { col = 24, row = 3 }, start = { col = 21, row = 3 } }
                                (Token
                                    (Identifier
                                        { name = TokenLowerCase (LowerCase "d")
                                        , qualifiers =
                                            [ UpperCase "A"
                                            ]
                                        }
                                    )
                                )
                            )
                    , state = State_BlockTypeAlias (BlockTypeAlias_Completish (UpperCase "B") [] (TypeExpressionNestingLeaf_TypeWithArgs { args = Stack [], name = UpperCase "List", parent = Nothing, phantom = (), qualifiers = [] }))
                    }
                , Err
                    { error = Error_TypeDoesNotTakeArgs (TypeExpression_GenericType (LowerCase "list")) (TypeExpression_NamedType { args = Stack [], name = UpperCase "A", qualifiers = [] })
                    , item = Just (Located { end = { col = 22, row = 5 }, start = { col = 21, row = 5 } } (Token (Identifier { name = TokenUpperCase (UpperCase "A"), qualifiers = [] })))
                    , state = State_BlockTypeAlias (BlockTypeAlias_Completish (UpperCase "C") [] (TypeExpressionNestingLeaf_Expr (TypeExpression_GenericType (LowerCase "list"))))
                    }
                , Err
                    { error =
                        Error_LowerCasedTypename
                            { name = LowerCase "c"
                            , qualifiers =
                                [ UpperCase "B"
                                ]
                            }
                    , item =
                        Just
                            (Located { end = { col = 26, row = 7 }, start = { col = 23, row = 7 } }
                                (Token
                                    (Identifier
                                        { name = TokenLowerCase (LowerCase "c")
                                        , qualifiers =
                                            [ UpperCase "B"
                                            ]
                                        }
                                    )
                                )
                            )
                    , state =
                        State_BlockTypeAlias
                            (BlockTypeAlias_Completish (UpperCase "D")
                                [ LowerCase "a"
                                ]
                                (TypeExpressionNestingLeaf_Function { firstInput = TypeExpression_GenericType (LowerCase "a"), otherInputs = Stack [], output = Nothing, parent = Nothing })
                            )
                    }
                , Err
                    { error =
                        Error_LowerCasedTypename
                            { name = LowerCase "c"
                            , qualifiers =
                                [ UpperCase "B"
                                ]
                            }
                    , item =
                        Just
                            (Located { end = { col = 21, row = 8 }, start = { col = 18, row = 8 } }
                                (Token
                                    (Identifier
                                        { name = TokenLowerCase (LowerCase "c")
                                        , qualifiers =
                                            [ UpperCase "B"
                                            ]
                                        }
                                    )
                                )
                            )
                    , state =
                        State_BlockTypeAlias
                            (BlockTypeAlias_NamedAssigns (UpperCase "E")
                                [ LowerCase "a"
                                ]
                            )
                    }
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token (Keyword Type))
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token (Keyword Alias))
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 13, row = 1 }, start = { col = 12, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "A"), qualifiers = [] }))
                , Located { end = { col = 14, row = 1 }, start = { col = 13, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Token (Sigil Assign))
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 21, row = 1 }, start = { col = 16, row = 1 } }
                    (Token
                        (Identifier
                            { name = TokenLowerCase (LowerCase "d")
                            , qualifiers =
                                [ UpperCase "B"
                                , UpperCase "C"
                                ]
                            }
                        )
                    )
                , Located { end = { col = 1, row = 3 }, start = { col = 21, row = 1 } }
                    (Newlines
                        [ 0
                        ]
                        0
                    )
                , Located { end = { col = 5, row = 3 }, start = { col = 1, row = 3 } } (Token (Keyword Type))
                , Located { end = { col = 6, row = 3 }, start = { col = 5, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 11, row = 3 }, start = { col = 6, row = 3 } } (Token (Keyword Alias))
                , Located { end = { col = 12, row = 3 }, start = { col = 11, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 13, row = 3 }, start = { col = 12, row = 3 } } (Token (Identifier { name = TokenUpperCase (UpperCase "B"), qualifiers = [] }))
                , Located { end = { col = 14, row = 3 }, start = { col = 13, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 15, row = 3 }, start = { col = 14, row = 3 } } (Token (Sigil Assign))
                , Located { end = { col = 16, row = 3 }, start = { col = 15, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 20, row = 3 }, start = { col = 16, row = 3 } } (Token (Identifier { name = TokenUpperCase (UpperCase "List"), qualifiers = [] }))
                , Located { end = { col = 21, row = 3 }, start = { col = 20, row = 3 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 24, row = 3 }, start = { col = 21, row = 3 } }
                    (Token
                        (Identifier
                            { name = TokenLowerCase (LowerCase "d")
                            , qualifiers =
                                [ UpperCase "A"
                                ]
                            }
                        )
                    )
                , Located { end = { col = 1, row = 5 }, start = { col = 24, row = 3 } }
                    (Newlines
                        [ 0
                        ]
                        0
                    )
                , Located { end = { col = 5, row = 5 }, start = { col = 1, row = 5 } } (Token (Keyword Type))
                , Located { end = { col = 6, row = 5 }, start = { col = 5, row = 5 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 11, row = 5 }, start = { col = 6, row = 5 } } (Token (Keyword Alias))
                , Located { end = { col = 12, row = 5 }, start = { col = 11, row = 5 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 13, row = 5 }, start = { col = 12, row = 5 } } (Token (Identifier { name = TokenUpperCase (UpperCase "C"), qualifiers = [] }))
                , Located { end = { col = 14, row = 5 }, start = { col = 13, row = 5 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 15, row = 5 }, start = { col = 14, row = 5 } } (Token (Sigil Assign))
                , Located { end = { col = 16, row = 5 }, start = { col = 15, row = 5 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 20, row = 5 }, start = { col = 16, row = 5 } } (Token (Identifier { name = TokenLowerCase (LowerCase "list"), qualifiers = [] }))
                , Located { end = { col = 21, row = 5 }, start = { col = 20, row = 5 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 22, row = 5 }, start = { col = 21, row = 5 } } (Token (Identifier { name = TokenUpperCase (UpperCase "A"), qualifiers = [] }))
                , Located { end = { col = 1, row = 7 }, start = { col = 22, row = 5 } }
                    (Newlines
                        [ 0
                        ]
                        0
                    )
                , Located { end = { col = 5, row = 7 }, start = { col = 1, row = 7 } } (Token (Keyword Type))
                , Located { end = { col = 6, row = 7 }, start = { col = 5, row = 7 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 11, row = 7 }, start = { col = 6, row = 7 } } (Token (Keyword Alias))
                , Located { end = { col = 12, row = 7 }, start = { col = 11, row = 7 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 13, row = 7 }, start = { col = 12, row = 7 } } (Token (Identifier { name = TokenUpperCase (UpperCase "D"), qualifiers = [] }))
                , Located { end = { col = 14, row = 7 }, start = { col = 13, row = 7 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 15, row = 7 }, start = { col = 14, row = 7 } } (Token (Identifier { name = TokenLowerCase (LowerCase "a"), qualifiers = [] }))
                , Located { end = { col = 16, row = 7 }, start = { col = 15, row = 7 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 17, row = 7 }, start = { col = 16, row = 7 } } (Token (Sigil Assign))
                , Located { end = { col = 18, row = 7 }, start = { col = 17, row = 7 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 19, row = 7 }, start = { col = 18, row = 7 } } (Token (Identifier { name = TokenLowerCase (LowerCase "a"), qualifiers = [] }))
                , Located { end = { col = 20, row = 7 }, start = { col = 19, row = 7 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 22, row = 7 }, start = { col = 20, row = 7 } } (Token (Sigil ThinArrow))
                , Located { end = { col = 23, row = 7 }, start = { col = 22, row = 7 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 26, row = 7 }, start = { col = 23, row = 7 } }
                    (Token
                        (Identifier
                            { name = TokenLowerCase (LowerCase "c")
                            , qualifiers =
                                [ UpperCase "B"
                                ]
                            }
                        )
                    )
                , Located { end = { col = 1, row = 8 }, start = { col = 26, row = 7 } } (Newlines [] 0)
                , Located { end = { col = 5, row = 8 }, start = { col = 1, row = 8 } } (Token (Keyword Type))
                , Located { end = { col = 6, row = 8 }, start = { col = 5, row = 8 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 11, row = 8 }, start = { col = 6, row = 8 } } (Token (Keyword Alias))
                , Located { end = { col = 12, row = 8 }, start = { col = 11, row = 8 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 13, row = 8 }, start = { col = 12, row = 8 } } (Token (Identifier { name = TokenUpperCase (UpperCase "E"), qualifiers = [] }))
                , Located { end = { col = 14, row = 8 }, start = { col = 13, row = 8 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 15, row = 8 }, start = { col = 14, row = 8 } } (Token (Identifier { name = TokenLowerCase (LowerCase "a"), qualifiers = [] }))
                , Located { end = { col = 16, row = 8 }, start = { col = 15, row = 8 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 17, row = 8 }, start = { col = 16, row = 8 } } (Token (Sigil Assign))
                , Located { end = { col = 18, row = 8 }, start = { col = 17, row = 8 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 21, row = 8 }, start = { col = 18, row = 8 } }
                    (Token
                        (Identifier
                            { name = TokenLowerCase (LowerCase "c")
                            , qualifiers =
                                [ UpperCase "B"
                                ]
                            }
                        )
                    )
                , Located { end = { col = 22, row = 8 }, start = { col = 21, row = 8 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 24, row = 8 }, start = { col = 22, row = 8 } } (Token (Sigil ThinArrow))
                , Located { end = { col = 25, row = 8 }, start = { col = 24, row = 8 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 26, row = 8 }, start = { col = 25, row = 8 } } (Token (Identifier { name = TokenLowerCase (LowerCase "a"), qualifiers = [] }))
                ]
      }
    , { name = "type-alias-multiline-missing-indentation"
      , source = """type alias Model =
List Int
"""
      , pretty = """
        ( ( Err, todo )
        , ( Err, todo )
        )
"""
      , contextualized =
            Just
                [ Err
                    { error = Error_PartwayThroughBlock
                    , item = Just (Located { end = { col = 1, row = 2 }, start = { col = 19, row = 1 } } (Newlines [] 0))
                    , state = State_BlockTypeAlias (BlockTypeAlias_NamedAssigns (UpperCase "Model") [])
                    }
                , Err
                    { error = Error_BlockStartsWithUpperCase (UpperCase "List")
                    , item = Just (Located { end = { col = 5, row = 2 }, start = { col = 1, row = 2 } } (Token (Identifier { name = TokenUpperCase (UpperCase "List"), qualifiers = [] })))
                    , state = State_BlockStart
                    }
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token (Keyword Type))
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token (Keyword Alias))
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 17, row = 1 }, start = { col = 12, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Model"), qualifiers = [] }))
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 19, row = 1 }, start = { col = 18, row = 1 } } (Token (Sigil Assign))
                , Located { end = { col = 1, row = 2 }, start = { col = 19, row = 1 } } (Newlines [] 0)
                , Located { end = { col = 5, row = 2 }, start = { col = 1, row = 2 } } (Token (Identifier { name = TokenUpperCase (UpperCase "List"), qualifiers = [] }))
                , Located { end = { col = 6, row = 2 }, start = { col = 5, row = 2 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 9, row = 2 }, start = { col = 6, row = 2 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Int"), qualifiers = [] }))
                , Located { end = { col = 1, row = 3 }, start = { col = 9, row = 2 } } (Newlines [] 0)
                ]
      }
    , { name = "type-alias-partial"
      , source = """type alias
"""
      , pretty = """
        ( ( Err, todo ) )
"""
      , contextualized =
            Just
                [ Err
                    { error = Error_PartwayThroughBlock
                    , item = Just (Located { end = { col = 1, row = 2 }, start = { col = 11, row = 1 } } (Newlines [] 0))
                    , state = State_BlockTypeAlias BlockTypeAlias_Keywords
                    }
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token (Keyword Type))
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token (Keyword Alias))
                , Located { end = { col = 1, row = 2 }, start = { col = 11, row = 1 } } (Newlines [] 0)
                ]
      }
    , { name = "type-alias-partial-2"
      , source = """type alias Hi
"""
      , pretty = """
        ( ( Err, todo ) )
"""
      , contextualized =
            Just
                [ Err
                    { error = Error_PartwayThroughBlock
                    , item = Just (Located { end = { col = 1, row = 2 }, start = { col = 14, row = 1 } } (Newlines [] 0))
                    , state = State_BlockTypeAlias (BlockTypeAlias_Named (UpperCase "Hi") (Stack []))
                    }
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token (Keyword Type))
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token (Keyword Alias))
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Hi"), qualifiers = [] }))
                , Located { end = { col = 1, row = 2 }, start = { col = 14, row = 1 } } (Newlines [] 0)
                ]
      }
    , { name = "type-alias-partial-3"
      , source = """type alias Hi =
"""
      , pretty = """
        ( ( Err, todo ) )
"""
      , contextualized =
            Just
                [ Err
                    { error = Error_PartwayThroughBlock
                    , item = Just (Located { end = { col = 1, row = 2 }, start = { col = 16, row = 1 } } (Newlines [] 0))
                    , state = State_BlockTypeAlias (BlockTypeAlias_NamedAssigns (UpperCase "Hi") [])
                    }
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token (Keyword Type))
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token (Keyword Alias))
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Hi"), qualifiers = [] }))
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Token (Sigil Assign))
                , Located { end = { col = 1, row = 2 }, start = { col = 16, row = 1 } } (Newlines [] 0)
                ]
      }
    , { name = "type-alias-partial-with-bracket"
      , source = """type alias Hi = (
"""
      , pretty = """
        ( ( Err, todo ) )
"""
      , contextualized =
            Just
                [ Err
                    { error = Error_PartwayThroughBlock
                    , item = Just (Located { end = { col = 1, row = 2 }, start = { col = 18, row = 1 } } (Newlines [] 0))
                    , state = State_BlockTypeAlias (BlockTypeAlias_Completish (UpperCase "Hi") [] (TypeExpressionNestingLeaf_Bracket { firstExpressions = Stack [], parent = Nothing, trailingExpression = Nothing }))
                    }
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token (Keyword Type))
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token (Keyword Alias))
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Hi"), qualifiers = [] }))
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Token (Sigil Assign))
                , Located { end = { col = 17, row = 1 }, start = { col = 16, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Token (Sigil (Bracket Round Open)))
                , Located { end = { col = 1, row = 2 }, start = { col = 18, row = 1 } } (Newlines [] 0)
                ]
      }
    , { name = "type-alias-partial-with-bracket-2"
      , source = """type alias Hi = (
        Int
"""
      , pretty = """
        ( ( Err, todo ) )
"""
      , contextualized =
            Just
                [ Err
                    { error = Error_PartwayThroughBlock
                    , item = Just (Located { end = { col = 1, row = 3 }, start = { col = 12, row = 2 } } (Newlines [] 0))
                    , state = State_BlockTypeAlias (BlockTypeAlias_Completish (UpperCase "Hi") [] (TypeExpressionNestingLeaf_TypeWithArgs { args = Stack [], name = UpperCase "Int", parent = Just (NestingParentType_Bracket { expressions = Stack [], parent = Nothing }), phantom = (), qualifiers = [] }))
                    }
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token (Keyword Type))
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token (Keyword Alias))
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Hi"), qualifiers = [] }))
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Token (Sigil Assign))
                , Located { end = { col = 17, row = 1 }, start = { col = 16, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Token (Sigil (Bracket Round Open)))
                , Located { end = { col = 9, row = 2 }, start = { col = 18, row = 1 } } (Newlines [] 8)
                , Located { end = { col = 12, row = 2 }, start = { col = 9, row = 2 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Int"), qualifiers = [] }))
                , Located { end = { col = 1, row = 3 }, start = { col = 12, row = 2 } } (Newlines [] 0)
                ]
      }
    , { name = "type-alias-record-half-empty"
      , source = """type alias Ty = {
"""
      , pretty = """
        ( ( Err, todo ) )
"""
      , contextualized =
            Just
                [ Err
                    { error = Error_PartwayThroughBlock
                    , item = Just (Located { end = { col = 1, row = 2 }, start = { col = 18, row = 1 } } (Newlines [] 0))
                    , state = State_BlockTypeAlias (BlockTypeAlias_Completish (UpperCase "Ty") [] (TypeExpressionNestingLeaf_PartialRecord { firstEntries = Stack [], lastEntry = LastEntryOfRecord_Empty, parent = Nothing }))
                    }
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token (Keyword Type))
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token (Keyword Alias))
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Ty"), qualifiers = [] }))
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Token (Sigil Assign))
                , Located { end = { col = 17, row = 1 }, start = { col = 16, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Token (Sigil (Bracket Curly Open)))
                , Located { end = { col = 1, row = 2 }, start = { col = 18, row = 1 } } (Newlines [] 0)
                ]
      }
    , { name = "type-alias-record-missing-colon"
      , source = """type alias Ty = { hi j7 }
"""
      , pretty = """
        ( ( Err, todo ) )
"""
      , contextualized =
            Just
                [ Err
                    { error = Error_ExpectedColonWhilstParsingRecord
                    , item = Just (Located { end = { col = 24, row = 1 }, start = { col = 22, row = 1 } } (Token (Identifier { name = TokenLowerCase (LowerCase "j7"), qualifiers = [] })))
                    , state = State_BlockTypeAlias (BlockTypeAlias_Completish (UpperCase "Ty") [] (TypeExpressionNestingLeaf_PartialRecord { firstEntries = Stack [], lastEntry = LastEntryOfRecord_Key (LowerCase "hi"), parent = Nothing }))
                    }
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token (Keyword Type))
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token (Keyword Alias))
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Ty"), qualifiers = [] }))
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Token (Sigil Assign))
                , Located { end = { col = 17, row = 1 }, start = { col = 16, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Token (Sigil (Bracket Curly Open)))
                , Located { end = { col = 19, row = 1 }, start = { col = 18, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 21, row = 1 }, start = { col = 19, row = 1 } } (Token (Identifier { name = TokenLowerCase (LowerCase "hi"), qualifiers = [] }))
                , Located { end = { col = 22, row = 1 }, start = { col = 21, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 24, row = 1 }, start = { col = 22, row = 1 } } (Token (Identifier { name = TokenLowerCase (LowerCase "j7"), qualifiers = [] }))
                , Located { end = { col = 25, row = 1 }, start = { col = 24, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 26, row = 1 }, start = { col = 25, row = 1 } } (Token (Sigil (Bracket Curly Close)))
                , Located { end = { col = 1, row = 2 }, start = { col = 26, row = 1 } } (Newlines [] 0)
                ]
      }
    , { name = "type-alias-with-quadruple"
      , source = """type alias Hi = (Int, A, B, C, D)
type alias Hi = (A Int, C D E F, H I (J K), L M () O P)
"""
      , pretty = """
        ( ( Err, todo )
        , ( Err, todo )
        )
"""
      , contextualized =
            Just
                [ Err
                    { error =
                        Error_TooManyTupleArgs
                            (UserDefinedType
                                { args = []
                                , name = "Int"
                                , qualifiedness = PossiblyQualified Nothing
                                }
                            )
                            (UserDefinedType
                                { args = []
                                , name = "A"
                                , qualifiedness = PossiblyQualified Nothing
                                }
                            )
                            (UserDefinedType
                                { args = []
                                , name = "B"
                                , qualifiedness = PossiblyQualified Nothing
                                }
                            )
                            (UserDefinedType
                                { args = []
                                , name = "C"
                                , qualifiedness = PossiblyQualified Nothing
                                }
                            )
                            [ UserDefinedType
                                { args = []
                                , name = "D"
                                , qualifiedness = PossiblyQualified Nothing
                                }
                            ]
                    , item = Just (Located { end = { col = 1, row = 2 }, start = { col = 34, row = 1 } } (Newlines [] 0))
                    , state =
                        State_BlockTypeAlias
                            (BlockTypeAlias_Completish (UpperCase "Hi")
                                []
                                (TypeExpressionNestingLeaf_Expr
                                    (TypeExpression_Tuple (TypeExpression_NamedType { args = Stack [], name = UpperCase "Int", qualifiers = [] })
                                        (TypeExpression_NamedType { args = Stack [], name = UpperCase "A", qualifiers = [] })
                                        [ TypeExpression_NamedType { args = Stack [], name = UpperCase "B", qualifiers = [] }
                                        , TypeExpression_NamedType { args = Stack [], name = UpperCase "C", qualifiers = [] }
                                        , TypeExpression_NamedType { args = Stack [], name = UpperCase "D", qualifiers = [] }
                                        ]
                                    )
                                )
                            )
                    }
                , Err
                    { error =
                        Error_TooManyTupleArgs
                            (UserDefinedType
                                { args =
                                    [ UserDefinedType
                                        { args = []
                                        , name = "Int"
                                        , qualifiedness = PossiblyQualified Nothing
                                        }
                                    ]
                                , name = "A"
                                , qualifiedness = PossiblyQualified Nothing
                                }
                            )
                            (UserDefinedType
                                { args =
                                    [ UserDefinedType
                                        { args = []
                                        , name = "D"
                                        , qualifiedness = PossiblyQualified Nothing
                                        }
                                    , UserDefinedType
                                        { args = []
                                        , name = "E"
                                        , qualifiedness = PossiblyQualified Nothing
                                        }
                                    , UserDefinedType
                                        { args = []
                                        , name = "F"
                                        , qualifiedness = PossiblyQualified Nothing
                                        }
                                    ]
                                , name = "C"
                                , qualifiedness = PossiblyQualified Nothing
                                }
                            )
                            (UserDefinedType
                                { args =
                                    [ UserDefinedType
                                        { args = []
                                        , name = "I"
                                        , qualifiedness = PossiblyQualified Nothing
                                        }
                                    , UserDefinedType
                                        { args =
                                            [ UserDefinedType
                                                { args = []
                                                , name = "K"
                                                , qualifiedness = PossiblyQualified Nothing
                                                }
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
                                    [ UserDefinedType
                                        { args = []
                                        , name = "M"
                                        , qualifiedness = PossiblyQualified Nothing
                                        }
                                    , Unit
                                    , UserDefinedType
                                        { args = []
                                        , name = "O"
                                        , qualifiedness = PossiblyQualified Nothing
                                        }
                                    , UserDefinedType
                                        { args = []
                                        , name = "P"
                                        , qualifiedness = PossiblyQualified Nothing
                                        }
                                    ]
                                , name = "L"
                                , qualifiedness = PossiblyQualified Nothing
                                }
                            )
                            []
                    , item = Just (Located { end = { col = 1, row = 3 }, start = { col = 56, row = 2 } } (Newlines [] 0))
                    , state =
                        State_BlockTypeAlias
                            (BlockTypeAlias_Completish (UpperCase "Hi")
                                []
                                (TypeExpressionNestingLeaf_Expr
                                    (TypeExpression_Tuple
                                        (TypeExpression_NamedType
                                            { args =
                                                Stack
                                                    [ TypeExpression_NamedType { args = Stack [], name = UpperCase "Int", qualifiers = [] }
                                                    ]
                                            , name = UpperCase "A"
                                            , qualifiers = []
                                            }
                                        )
                                        (TypeExpression_NamedType
                                            { args =
                                                Stack
                                                    [ TypeExpression_NamedType { args = Stack [], name = UpperCase "F", qualifiers = [] }
                                                    , TypeExpression_NamedType { args = Stack [], name = UpperCase "E", qualifiers = [] }
                                                    , TypeExpression_NamedType { args = Stack [], name = UpperCase "D", qualifiers = [] }
                                                    ]
                                            , name = UpperCase "C"
                                            , qualifiers = []
                                            }
                                        )
                                        [ TypeExpression_NamedType
                                            { args =
                                                Stack
                                                    [ TypeExpression_Bracketed
                                                        (TypeExpression_NamedType
                                                            { args =
                                                                Stack
                                                                    [ TypeExpression_NamedType { args = Stack [], name = UpperCase "K", qualifiers = [] }
                                                                    ]
                                                            , name = UpperCase "J"
                                                            , qualifiers = []
                                                            }
                                                        )
                                                    , TypeExpression_NamedType { args = Stack [], name = UpperCase "I", qualifiers = [] }
                                                    ]
                                            , name = UpperCase "H"
                                            , qualifiers = []
                                            }
                                        , TypeExpression_NamedType
                                            { args =
                                                Stack
                                                    [ TypeExpression_NamedType { args = Stack [], name = UpperCase "P", qualifiers = [] }
                                                    , TypeExpression_NamedType { args = Stack [], name = UpperCase "O", qualifiers = [] }
                                                    , TypeExpression_Unit
                                                    , TypeExpression_NamedType { args = Stack [], name = UpperCase "M", qualifiers = [] }
                                                    ]
                                            , name = UpperCase "L"
                                            , qualifiers = []
                                            }
                                        ]
                                    )
                                )
                            )
                    }
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token (Keyword Type))
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token (Keyword Alias))
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Hi"), qualifiers = [] }))
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Token (Sigil Assign))
                , Located { end = { col = 17, row = 1 }, start = { col = 16, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Token (Sigil (Bracket Round Open)))
                , Located { end = { col = 21, row = 1 }, start = { col = 18, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Int"), qualifiers = [] }))
                , Located { end = { col = 22, row = 1 }, start = { col = 21, row = 1 } } (Token (Sigil Comma))
                , Located { end = { col = 23, row = 1 }, start = { col = 22, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 24, row = 1 }, start = { col = 23, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "A"), qualifiers = [] }))
                , Located { end = { col = 25, row = 1 }, start = { col = 24, row = 1 } } (Token (Sigil Comma))
                , Located { end = { col = 26, row = 1 }, start = { col = 25, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 27, row = 1 }, start = { col = 26, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "B"), qualifiers = [] }))
                , Located { end = { col = 28, row = 1 }, start = { col = 27, row = 1 } } (Token (Sigil Comma))
                , Located { end = { col = 29, row = 1 }, start = { col = 28, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 30, row = 1 }, start = { col = 29, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "C"), qualifiers = [] }))
                , Located { end = { col = 31, row = 1 }, start = { col = 30, row = 1 } } (Token (Sigil Comma))
                , Located { end = { col = 32, row = 1 }, start = { col = 31, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 33, row = 1 }, start = { col = 32, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "D"), qualifiers = [] }))
                , Located { end = { col = 34, row = 1 }, start = { col = 33, row = 1 } } (Token (Sigil (Bracket Round Close)))
                , Located { end = { col = 1, row = 2 }, start = { col = 34, row = 1 } } (Newlines [] 0)
                , Located { end = { col = 5, row = 2 }, start = { col = 1, row = 2 } } (Token (Keyword Type))
                , Located { end = { col = 6, row = 2 }, start = { col = 5, row = 2 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 11, row = 2 }, start = { col = 6, row = 2 } } (Token (Keyword Alias))
                , Located { end = { col = 12, row = 2 }, start = { col = 11, row = 2 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 14, row = 2 }, start = { col = 12, row = 2 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Hi"), qualifiers = [] }))
                , Located { end = { col = 15, row = 2 }, start = { col = 14, row = 2 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 16, row = 2 }, start = { col = 15, row = 2 } } (Token (Sigil Assign))
                , Located { end = { col = 17, row = 2 }, start = { col = 16, row = 2 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 18, row = 2 }, start = { col = 17, row = 2 } } (Token (Sigil (Bracket Round Open)))
                , Located { end = { col = 19, row = 2 }, start = { col = 18, row = 2 } } (Token (Identifier { name = TokenUpperCase (UpperCase "A"), qualifiers = [] }))
                , Located { end = { col = 20, row = 2 }, start = { col = 19, row = 2 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 23, row = 2 }, start = { col = 20, row = 2 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Int"), qualifiers = [] }))
                , Located { end = { col = 24, row = 2 }, start = { col = 23, row = 2 } } (Token (Sigil Comma))
                , Located { end = { col = 25, row = 2 }, start = { col = 24, row = 2 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 26, row = 2 }, start = { col = 25, row = 2 } } (Token (Identifier { name = TokenUpperCase (UpperCase "C"), qualifiers = [] }))
                , Located { end = { col = 27, row = 2 }, start = { col = 26, row = 2 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 28, row = 2 }, start = { col = 27, row = 2 } } (Token (Identifier { name = TokenUpperCase (UpperCase "D"), qualifiers = [] }))
                , Located { end = { col = 29, row = 2 }, start = { col = 28, row = 2 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 30, row = 2 }, start = { col = 29, row = 2 } } (Token (Identifier { name = TokenUpperCase (UpperCase "E"), qualifiers = [] }))
                , Located { end = { col = 31, row = 2 }, start = { col = 30, row = 2 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 32, row = 2 }, start = { col = 31, row = 2 } } (Token (Identifier { name = TokenUpperCase (UpperCase "F"), qualifiers = [] }))
                , Located { end = { col = 33, row = 2 }, start = { col = 32, row = 2 } } (Token (Sigil Comma))
                , Located { end = { col = 34, row = 2 }, start = { col = 33, row = 2 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 35, row = 2 }, start = { col = 34, row = 2 } } (Token (Identifier { name = TokenUpperCase (UpperCase "H"), qualifiers = [] }))
                , Located { end = { col = 36, row = 2 }, start = { col = 35, row = 2 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 37, row = 2 }, start = { col = 36, row = 2 } } (Token (Identifier { name = TokenUpperCase (UpperCase "I"), qualifiers = [] }))
                , Located { end = { col = 38, row = 2 }, start = { col = 37, row = 2 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 39, row = 2 }, start = { col = 38, row = 2 } } (Token (Sigil (Bracket Round Open)))
                , Located { end = { col = 40, row = 2 }, start = { col = 39, row = 2 } } (Token (Identifier { name = TokenUpperCase (UpperCase "J"), qualifiers = [] }))
                , Located { end = { col = 41, row = 2 }, start = { col = 40, row = 2 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 42, row = 2 }, start = { col = 41, row = 2 } } (Token (Identifier { name = TokenUpperCase (UpperCase "K"), qualifiers = [] }))
                , Located { end = { col = 43, row = 2 }, start = { col = 42, row = 2 } } (Token (Sigil (Bracket Round Close)))
                , Located { end = { col = 44, row = 2 }, start = { col = 43, row = 2 } } (Token (Sigil Comma))
                , Located { end = { col = 45, row = 2 }, start = { col = 44, row = 2 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 46, row = 2 }, start = { col = 45, row = 2 } } (Token (Identifier { name = TokenUpperCase (UpperCase "L"), qualifiers = [] }))
                , Located { end = { col = 47, row = 2 }, start = { col = 46, row = 2 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 48, row = 2 }, start = { col = 47, row = 2 } } (Token (Identifier { name = TokenUpperCase (UpperCase "M"), qualifiers = [] }))
                , Located { end = { col = 49, row = 2 }, start = { col = 48, row = 2 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 50, row = 2 }, start = { col = 49, row = 2 } } (Token (Sigil (Bracket Round Open)))
                , Located { end = { col = 51, row = 2 }, start = { col = 50, row = 2 } } (Token (Sigil (Bracket Round Close)))
                , Located { end = { col = 52, row = 2 }, start = { col = 51, row = 2 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 53, row = 2 }, start = { col = 52, row = 2 } } (Token (Identifier { name = TokenUpperCase (UpperCase "O"), qualifiers = [] }))
                , Located { end = { col = 54, row = 2 }, start = { col = 53, row = 2 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 55, row = 2 }, start = { col = 54, row = 2 } } (Token (Identifier { name = TokenUpperCase (UpperCase "P"), qualifiers = [] }))
                , Located { end = { col = 56, row = 2 }, start = { col = 55, row = 2 } } (Token (Sigil (Bracket Round Close)))
                , Located { end = { col = 1, row = 3 }, start = { col = 56, row = 2 } } (Newlines [] 0)
                ]
      }
    , { name = "type-alias-with-tuple-double-comma"
      , source = """type alias Hi = (Int, , A)
"""
      , pretty = """
        ( ( Err, todo ) )
"""
      , contextualized =
            Just
                [ Err
                    { error = Error_InvalidToken Expecting_Unknown
                    , item = Just (Located { end = { col = 24, row = 1 }, start = { col = 23, row = 1 } } (Token (Sigil Comma)))
                    , state =
                        State_BlockTypeAlias
                            (BlockTypeAlias_Completish (UpperCase "Hi")
                                []
                                (TypeExpressionNestingLeaf_Bracket
                                    { firstExpressions =
                                        Stack
                                            [ TypeExpression_NamedType { args = Stack [], name = UpperCase "Int", qualifiers = [] }
                                            ]
                                    , parent = Nothing
                                    , trailingExpression = Nothing
                                    }
                                )
                            )
                    }
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token (Keyword Type))
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token (Keyword Alias))
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Hi"), qualifiers = [] }))
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Token (Sigil Assign))
                , Located { end = { col = 17, row = 1 }, start = { col = 16, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Token (Sigil (Bracket Round Open)))
                , Located { end = { col = 21, row = 1 }, start = { col = 18, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Int"), qualifiers = [] }))
                , Located { end = { col = 22, row = 1 }, start = { col = 21, row = 1 } } (Token (Sigil Comma))
                , Located { end = { col = 23, row = 1 }, start = { col = 22, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 24, row = 1 }, start = { col = 23, row = 1 } } (Token (Sigil Comma))
                , Located { end = { col = 25, row = 1 }, start = { col = 24, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 26, row = 1 }, start = { col = 25, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "A"), qualifiers = [] }))
                , Located { end = { col = 27, row = 1 }, start = { col = 26, row = 1 } } (Token (Sigil (Bracket Round Close)))
                , Located { end = { col = 1, row = 2 }, start = { col = 27, row = 1 } } (Newlines [] 0)
                ]
      }
    , { name = "type-alias-with-tuple-not-closed"
      , source = """type alias Hi = (Int,



"""
      , pretty = """
        ( ( Err, todo ) )
"""
      , contextualized =
            Just
                [ Err
                    { error = Error_PartwayThroughBlock
                    , item =
                        Just
                            (Located { end = { col = 1, row = 5 }, start = { col = 22, row = 1 } }
                                (Newlines
                                    [ 0
                                    , 0
                                    , 0
                                    ]
                                    0
                                )
                            )
                    , state =
                        State_BlockTypeAlias
                            (BlockTypeAlias_Completish (UpperCase "Hi")
                                []
                                (TypeExpressionNestingLeaf_Bracket
                                    { firstExpressions =
                                        Stack
                                            [ TypeExpression_NamedType { args = Stack [], name = UpperCase "Int", qualifiers = [] }
                                            ]
                                    , parent = Nothing
                                    , trailingExpression = Nothing
                                    }
                                )
                            )
                    }
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token (Keyword Type))
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Token (Keyword Alias))
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Hi"), qualifiers = [] }))
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Token (Sigil Assign))
                , Located { end = { col = 17, row = 1 }, start = { col = 16, row = 1 } } (Ignorable (Whitespace 1))
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Token (Sigil (Bracket Round Open)))
                , Located { end = { col = 21, row = 1 }, start = { col = 18, row = 1 } } (Token (Identifier { name = TokenUpperCase (UpperCase "Int"), qualifiers = [] }))
                , Located { end = { col = 22, row = 1 }, start = { col = 21, row = 1 } } (Token (Sigil Comma))
                , Located { end = { col = 1, row = 5 }, start = { col = 22, row = 1 } }
                    (Newlines
                        [ 0
                        , 0
                        , 0
                        ]
                        0
                    )
                ]
      }
    , { name = "type-partial"
      , source = """type
"""
      , pretty = """
        ( ( Err, todo ) )
"""
      , contextualized =
            Just
                [ Err
                    { error = Error_PartwayThroughBlock
                    , item = Just (Located { end = { col = 1, row = 2 }, start = { col = 5, row = 1 } } (Newlines [] 0))
                    , state = State_BlockFirstItem BlockFirstItem_Type
                    }
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Token (Keyword Type))
                , Located { end = { col = 1, row = 2 }, start = { col = 5, row = 1 } } (Newlines [] 0)
                ]
      }
    ]
