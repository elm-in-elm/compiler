module ParserLexerTestCases exposing (shouldNotParseTestCases, shouldParseTestCases)

import Dict
import Elm.AST.Frontend as Frontend
import Elm.Data.Located exposing (Located(..))
import Elm.Data.Operator exposing (Operator(..))
import Elm.Data.Qualifiedness exposing (PossiblyQualified(..))
import Elm.Data.Type.Concrete exposing (ConcreteType(..))
import Stage.Parse.Contextualize as Contextualize exposing (..)
import Stage.Parse.Lexer exposing (..)
import Stage.Parse.Token exposing (Keyword(..), TypeOrConstructor(..), ValueOrFunctionOrGenericType(..))



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
                [ Ok (ValueDeclaration { args = [], name = Located { end = { col = 2, row = 1 }, start = { col = 1, row = 1 } } "a", valueExpr__ = Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Frontend.Int 5) })
                , Ok (ValueDeclaration { args = [], name = Located { end = { col = 2, row = 3 }, start = { col = 1, row = 3 } } "b", valueExpr__ = Located { end = { col = 7, row = 3 }, start = { col = 5, row = 3 } } (Frontend.Int 78) })
                ]
      , lexed =
            Ok
                [ Located { end = { col = 2, row = 1 }, start = { col = 1, row = 1 } } (Identifier { name = "a", qualifiers = [] })
                , Located { end = { col = 3, row = 1 }, start = { col = 2, row = 1 } } (Whitespace 1)
                , Located { end = { col = 4, row = 1 }, start = { col = 3, row = 1 } } (Sigil Assign)
                , Located { end = { col = 5, row = 1 }, start = { col = 4, row = 1 } } (Whitespace 1)
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (NumericLiteral "5")
                , Located { end = { col = 1, row = 3 }, start = { col = 6, row = 1 } }
                    (Newlines
                        [ 0
                        ]
                        0
                    )
                , Located { end = { col = 2, row = 3 }, start = { col = 1, row = 3 } } (Identifier { name = "b", qualifiers = [] })
                , Located { end = { col = 3, row = 3 }, start = { col = 2, row = 3 } } (Whitespace 1)
                , Located { end = { col = 4, row = 3 }, start = { col = 3, row = 3 } } (Sigil Assign)
                , Located { end = { col = 5, row = 3 }, start = { col = 4, row = 3 } } (Whitespace 1)
                , Located { end = { col = 7, row = 3 }, start = { col = 5, row = 3 } } (NumericLiteral "78")
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
                [ Ok (ValueDeclaration { args = [], name = Located { end = { col = 2, row = 1 }, start = { col = 1, row = 1 } } "a", valueExpr__ = Located { end = { col = 10, row = 1 }, start = { col = 5, row = 1 } } (Frontend.Operator (Located { end = { col = 8, row = 1 }, start = { col = 7, row = 1 } } Add) (Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Frontend.Int 5)) (Located { end = { col = 10, row = 1 }, start = { col = 9, row = 1 } } (Frontend.Int 5))) })
                , Ok (ValueDeclaration { args = [], name = Located { end = { col = 2, row = 3 }, start = { col = 1, row = 3 } } "b", valueExpr__ = Located { end = { col = 18, row = 3 }, start = { col = 5, row = 3 } } (Frontend.Operator (Located { end = { col = 16, row = 3 }, start = { col = 15, row = 3 } } Add) (Located { end = { col = 15, row = 3 }, start = { col = 5, row = 3 } } (Frontend.Operator (Located { end = { col = 13, row = 3 }, start = { col = 12, row = 3 } } Add) (Located { end = { col = 11, row = 3 }, start = { col = 5, row = 3 } } (Frontend.Operator (Located { end = { col = 9, row = 3 }, start = { col = 8, row = 3 } } Add) (Located { end = { col = 7, row = 3 }, start = { col = 5, row = 3 } } (Frontend.Int 78)) (Located { end = { col = 11, row = 3 }, start = { col = 10, row = 3 } } (Frontend.Int 5)))) (Located { end = { col = 15, row = 3 }, start = { col = 14, row = 3 } } (Frontend.Int 2)))) (Located { end = { col = 18, row = 3 }, start = { col = 17, row = 3 } } (Frontend.Int 4))) })
                ]
      , lexed =
            Ok
                [ Located { end = { col = 2, row = 1 }, start = { col = 1, row = 1 } } (Identifier { name = "a", qualifiers = [] })
                , Located { end = { col = 3, row = 1 }, start = { col = 2, row = 1 } } (Whitespace 1)
                , Located { end = { col = 4, row = 1 }, start = { col = 3, row = 1 } } (Sigil Assign)
                , Located { end = { col = 5, row = 1 }, start = { col = 4, row = 1 } } (Whitespace 1)
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (NumericLiteral "5")
                , Located { end = { col = 7, row = 1 }, start = { col = 6, row = 1 } } (Whitespace 1)
                , Located { end = { col = 8, row = 1 }, start = { col = 7, row = 1 } } (Sigil (Operator Add))
                , Located { end = { col = 9, row = 1 }, start = { col = 8, row = 1 } } (Whitespace 1)
                , Located { end = { col = 10, row = 1 }, start = { col = 9, row = 1 } } (NumericLiteral "5")
                , Located { end = { col = 1, row = 3 }, start = { col = 10, row = 1 } }
                    (Newlines
                        [ 0
                        ]
                        0
                    )
                , Located { end = { col = 2, row = 3 }, start = { col = 1, row = 3 } } (Identifier { name = "b", qualifiers = [] })
                , Located { end = { col = 3, row = 3 }, start = { col = 2, row = 3 } } (Whitespace 1)
                , Located { end = { col = 4, row = 3 }, start = { col = 3, row = 3 } } (Sigil Assign)
                , Located { end = { col = 5, row = 3 }, start = { col = 4, row = 3 } } (Whitespace 1)
                , Located { end = { col = 7, row = 3 }, start = { col = 5, row = 3 } } (NumericLiteral "78")
                , Located { end = { col = 8, row = 3 }, start = { col = 7, row = 3 } } (Whitespace 1)
                , Located { end = { col = 9, row = 3 }, start = { col = 8, row = 3 } } (Sigil (Operator Add))
                , Located { end = { col = 10, row = 3 }, start = { col = 9, row = 3 } } (Whitespace 1)
                , Located { end = { col = 11, row = 3 }, start = { col = 10, row = 3 } } (NumericLiteral "5")
                , Located { end = { col = 12, row = 3 }, start = { col = 11, row = 3 } } (Whitespace 1)
                , Located { end = { col = 13, row = 3 }, start = { col = 12, row = 3 } } (Sigil (Operator Add))
                , Located { end = { col = 14, row = 3 }, start = { col = 13, row = 3 } } (Whitespace 1)
                , Located { end = { col = 15, row = 3 }, start = { col = 14, row = 3 } } (NumericLiteral "2")
                , Located { end = { col = 16, row = 3 }, start = { col = 15, row = 3 } } (Sigil (Operator Add))
                , Located { end = { col = 17, row = 3 }, start = { col = 16, row = 3 } } (Whitespace 1)
                , Located { end = { col = 18, row = 3 }, start = { col = 17, row = 3 } } (NumericLiteral "4")
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
                [ Ok (ValueDeclaration { args = [], name = Located { end = { col = 2, row = 1 }, start = { col = 1, row = 1 } } "a", valueExpr__ = Located { end = { col = 10, row = 1 }, start = { col = 5, row = 1 } } (Frontend.Operator (Located { end = { col = 8, row = 1 }, start = { col = 7, row = 1 } } Multiply) (Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Frontend.Int 5)) (Located { end = { col = 10, row = 1 }, start = { col = 9, row = 1 } } (Frontend.Int 5))) })
                , Ok (ValueDeclaration { args = [], name = Located { end = { col = 2, row = 3 }, start = { col = 1, row = 3 } } "b", valueExpr__ = Located { end = { col = 23, row = 3 }, start = { col = 5, row = 3 } } (Frontend.Operator (Located { end = { col = 21, row = 3 }, start = { col = 20, row = 3 } } Multiply) (Located { end = { col = 19, row = 3 }, start = { col = 5, row = 3 } } (Frontend.Operator (Located { end = { col = 17, row = 3 }, start = { col = 16, row = 3 } } Divide) (Located { end = { col = 15, row = 3 }, start = { col = 5, row = 3 } } (Frontend.Operator (Located { end = { col = 13, row = 3 }, start = { col = 12, row = 3 } } Multiply) (Located { end = { col = 11, row = 3 }, start = { col = 5, row = 3 } } (Frontend.Operator (Located { end = { col = 9, row = 3 }, start = { col = 8, row = 3 } } Multiply) (Located { end = { col = 7, row = 3 }, start = { col = 5, row = 3 } } (Frontend.Int 78)) (Located { end = { col = 11, row = 3 }, start = { col = 10, row = 3 } } (Frontend.Int 5)))) (Located { end = { col = 15, row = 3 }, start = { col = 14, row = 3 } } (Frontend.Int 2)))) (Located { end = { col = 19, row = 3 }, start = { col = 18, row = 3 } } (Frontend.Int 4)))) (Located { end = { col = 23, row = 3 }, start = { col = 22, row = 3 } } (Frontend.Int 5))) })
                ]
      , lexed =
            Ok
                [ Located { end = { col = 2, row = 1 }, start = { col = 1, row = 1 } } (Identifier { name = "a", qualifiers = [] })
                , Located { end = { col = 3, row = 1 }, start = { col = 2, row = 1 } } (Whitespace 1)
                , Located { end = { col = 4, row = 1 }, start = { col = 3, row = 1 } } (Sigil Assign)
                , Located { end = { col = 5, row = 1 }, start = { col = 4, row = 1 } } (Whitespace 1)
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (NumericLiteral "5")
                , Located { end = { col = 7, row = 1 }, start = { col = 6, row = 1 } } (Whitespace 1)
                , Located { end = { col = 8, row = 1 }, start = { col = 7, row = 1 } } (Sigil (Operator Multiply))
                , Located { end = { col = 9, row = 1 }, start = { col = 8, row = 1 } } (Whitespace 1)
                , Located { end = { col = 10, row = 1 }, start = { col = 9, row = 1 } } (NumericLiteral "5")
                , Located { end = { col = 1, row = 3 }, start = { col = 10, row = 1 } }
                    (Newlines
                        [ 0
                        ]
                        0
                    )
                , Located { end = { col = 2, row = 3 }, start = { col = 1, row = 3 } } (Identifier { name = "b", qualifiers = [] })
                , Located { end = { col = 3, row = 3 }, start = { col = 2, row = 3 } } (Whitespace 1)
                , Located { end = { col = 4, row = 3 }, start = { col = 3, row = 3 } } (Sigil Assign)
                , Located { end = { col = 5, row = 3 }, start = { col = 4, row = 3 } } (Whitespace 1)
                , Located { end = { col = 7, row = 3 }, start = { col = 5, row = 3 } } (NumericLiteral "78")
                , Located { end = { col = 8, row = 3 }, start = { col = 7, row = 3 } } (Whitespace 1)
                , Located { end = { col = 9, row = 3 }, start = { col = 8, row = 3 } } (Sigil (Operator Multiply))
                , Located { end = { col = 10, row = 3 }, start = { col = 9, row = 3 } } (Whitespace 1)
                , Located { end = { col = 11, row = 3 }, start = { col = 10, row = 3 } } (NumericLiteral "5")
                , Located { end = { col = 12, row = 3 }, start = { col = 11, row = 3 } } (Whitespace 1)
                , Located { end = { col = 13, row = 3 }, start = { col = 12, row = 3 } } (Sigil (Operator Multiply))
                , Located { end = { col = 14, row = 3 }, start = { col = 13, row = 3 } } (Whitespace 1)
                , Located { end = { col = 15, row = 3 }, start = { col = 14, row = 3 } } (NumericLiteral "2")
                , Located { end = { col = 16, row = 3 }, start = { col = 15, row = 3 } } (Whitespace 1)
                , Located { end = { col = 17, row = 3 }, start = { col = 16, row = 3 } } (Sigil (Operator Divide))
                , Located { end = { col = 18, row = 3 }, start = { col = 17, row = 3 } } (Whitespace 1)
                , Located { end = { col = 19, row = 3 }, start = { col = 18, row = 3 } } (NumericLiteral "4")
                , Located { end = { col = 20, row = 3 }, start = { col = 19, row = 3 } } (Whitespace 1)
                , Located { end = { col = 21, row = 3 }, start = { col = 20, row = 3 } } (Sigil (Operator Multiply))
                , Located { end = { col = 22, row = 3 }, start = { col = 21, row = 3 } } (Whitespace 1)
                , Located { end = { col = 23, row = 3 }, start = { col = 22, row = 3 } } (NumericLiteral "5")
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
                [ Ok (ValueDeclaration { args = [], name = Located { end = { col = 2, row = 1 }, start = { col = 1, row = 1 } } "a", valueExpr__ = Located { end = { col = 14, row = 1 }, start = { col = 5, row = 1 } } (Frontend.Operator (Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } Add) (Located { end = { col = 10, row = 1 }, start = { col = 5, row = 1 } } (Frontend.Operator (Located { end = { col = 8, row = 1 }, start = { col = 7, row = 1 } } Multiply) (Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Frontend.Int 5)) (Located { end = { col = 10, row = 1 }, start = { col = 9, row = 1 } } (Frontend.Int 5)))) (Located { end = { col = 14, row = 1 }, start = { col = 13, row = 1 } } (Frontend.Int 6))) })
                , Ok (ValueDeclaration { args = [], name = Located { end = { col = 3, row = 2 }, start = { col = 1, row = 2 } } "a1", valueExpr__ = Located { end = { col = 16, row = 2 }, start = { col = 7, row = 2 } } (Frontend.Operator (Located { end = { col = 10, row = 2 }, start = { col = 9, row = 2 } } Add) (Located { end = { col = 8, row = 2 }, start = { col = 7, row = 2 } } (Frontend.Int 7)) (Located { end = { col = 16, row = 2 }, start = { col = 11, row = 2 } } (Frontend.Operator (Located { end = { col = 14, row = 2 }, start = { col = 13, row = 2 } } Multiply) (Located { end = { col = 12, row = 2 }, start = { col = 11, row = 2 } } (Frontend.Int 5)) (Located { end = { col = 16, row = 2 }, start = { col = 15, row = 2 } } (Frontend.Int 5))))) })
                , Ok (ValueDeclaration { args = [], name = Located { end = { col = 4, row = 3 }, start = { col = 1, row = 3 } } "a11", valueExpr__ = Located { end = { col = 21, row = 3 }, start = { col = 8, row = 3 } } (Frontend.Operator (Located { end = { col = 11, row = 3 }, start = { col = 10, row = 3 } } Add) (Located { end = { col = 9, row = 3 }, start = { col = 8, row = 3 } } (Frontend.Int 7)) (Located { end = { col = 21, row = 3 }, start = { col = 12, row = 3 } } (Frontend.Operator (Located { end = { col = 19, row = 3 }, start = { col = 18, row = 3 } } Add) (Located { end = { col = 17, row = 3 }, start = { col = 12, row = 3 } } (Frontend.Operator (Located { end = { col = 15, row = 3 }, start = { col = 14, row = 3 } } Multiply) (Located { end = { col = 13, row = 3 }, start = { col = 12, row = 3 } } (Frontend.Int 5)) (Located { end = { col = 17, row = 3 }, start = { col = 16, row = 3 } } (Frontend.Int 5)))) (Located { end = { col = 21, row = 3 }, start = { col = 20, row = 3 } } (Frontend.Int 6))))) })
                , Ok (ValueDeclaration { args = [], name = Located { end = { col = 3, row = 4 }, start = { col = 1, row = 4 } } "a2", valueExpr__ = Located { end = { col = 18, row = 4 }, start = { col = 6, row = 4 } } (Frontend.Operator (Located { end = { col = 12, row = 4 }, start = { col = 11, row = 4 } } Add) (Located { end = { col = 9, row = 4 }, start = { col = 6, row = 4 } } (Frontend.Int 100)) (Located { end = { col = 18, row = 4 }, start = { col = 13, row = 4 } } (Frontend.Operator (Located { end = { col = 16, row = 4 }, start = { col = 15, row = 4 } } Multiply) (Located { end = { col = 14, row = 4 }, start = { col = 13, row = 4 } } (Frontend.Int 5)) (Located { end = { col = 18, row = 4 }, start = { col = 17, row = 4 } } (Frontend.Int 5))))) })
                , Ok (ValueDeclaration { args = [], name = Located { end = { col = 3, row = 5 }, start = { col = 1, row = 5 } } "a3", valueExpr__ = Located { end = { col = 30, row = 5 }, start = { col = 6, row = 5 } } (Frontend.Operator (Located { end = { col = 18, row = 5 }, start = { col = 17, row = 5 } } Add) (Located { end = { col = 16, row = 5 }, start = { col = 6, row = 5 } } (Frontend.Operator (Located { end = { col = 11, row = 5 }, start = { col = 10, row = 5 } } Multiply) (Located { end = { col = 9, row = 5 }, start = { col = 6, row = 5 } } (Frontend.Int 345)) (Located { end = { col = 16, row = 5 }, start = { col = 12, row = 5 } } (Frontend.Int 2234)))) (Located { end = { col = 30, row = 5 }, start = { col = 19, row = 5 } } (Frontend.Operator (Located { end = { col = 25, row = 5 }, start = { col = 24, row = 5 } } Multiply) (Located { end = { col = 23, row = 5 }, start = { col = 19, row = 5 } } (Frontend.Int 2342)) (Located { end = { col = 30, row = 5 }, start = { col = 26, row = 5 } } (Frontend.Int 1010))))) })
                , Ok (ValueDeclaration { args = [], name = Located { end = { col = 2, row = 8 }, start = { col = 1, row = 8 } } "b", valueExpr__ = Located { end = { col = 23, row = 8 }, start = { col = 5, row = 8 } } (Frontend.Operator (Located { end = { col = 9, row = 8 }, start = { col = 8, row = 8 } } Add) (Located { end = { col = 7, row = 8 }, start = { col = 5, row = 8 } } (Frontend.Int 78)) (Located { end = { col = 23, row = 8 }, start = { col = 10, row = 8 } } (Frontend.Operator (Located { end = { col = 21, row = 8 }, start = { col = 20, row = 8 } } Multiply) (Located { end = { col = 19, row = 8 }, start = { col = 10, row = 8 } } (Frontend.Operator (Located { end = { col = 17, row = 8 }, start = { col = 16, row = 8 } } Divide) (Located { end = { col = 15, row = 8 }, start = { col = 10, row = 8 } } (Frontend.Operator (Located { end = { col = 13, row = 8 }, start = { col = 12, row = 8 } } Multiply) (Located { end = { col = 11, row = 8 }, start = { col = 10, row = 8 } } (Frontend.Int 5)) (Located { end = { col = 15, row = 8 }, start = { col = 14, row = 8 } } (Frontend.Int 2)))) (Located { end = { col = 19, row = 8 }, start = { col = 18, row = 8 } } (Frontend.Int 4)))) (Located { end = { col = 23, row = 8 }, start = { col = 22, row = 8 } } (Frontend.Int 5))))) })
                , Ok (ValueDeclaration { args = [], name = Located { end = { col = 3, row = 9 }, start = { col = 1, row = 9 } } "b1", valueExpr__ = Located { end = { col = 24, row = 9 }, start = { col = 6, row = 9 } } (Frontend.Operator (Located { end = { col = 22, row = 9 }, start = { col = 21, row = 9 } } Subtract) (Located { end = { col = 20, row = 9 }, start = { col = 6, row = 9 } } (Frontend.Operator (Located { end = { col = 18, row = 9 }, start = { col = 17, row = 9 } } Divide) (Located { end = { col = 16, row = 9 }, start = { col = 6, row = 9 } } (Frontend.Operator (Located { end = { col = 14, row = 9 }, start = { col = 13, row = 9 } } Multiply) (Located { end = { col = 12, row = 9 }, start = { col = 6, row = 9 } } (Frontend.Operator (Located { end = { col = 10, row = 9 }, start = { col = 9, row = 9 } } Multiply) (Located { end = { col = 8, row = 9 }, start = { col = 6, row = 9 } } (Frontend.Int 78)) (Located { end = { col = 12, row = 9 }, start = { col = 11, row = 9 } } (Frontend.Int 5)))) (Located { end = { col = 16, row = 9 }, start = { col = 15, row = 9 } } (Frontend.Int 2)))) (Located { end = { col = 20, row = 9 }, start = { col = 19, row = 9 } } (Frontend.Int 4)))) (Located { end = { col = 24, row = 9 }, start = { col = 23, row = 9 } } (Frontend.Int 5))) })
                , Ok (ValueDeclaration { args = [], name = Located { end = { col = 3, row = 10 }, start = { col = 1, row = 10 } } "b2", valueExpr__ = Located { end = { col = 24, row = 10 }, start = { col = 6, row = 10 } } (Frontend.Operator (Located { end = { col = 14, row = 10 }, start = { col = 13, row = 10 } } Subtract) (Located { end = { col = 12, row = 10 }, start = { col = 6, row = 10 } } (Frontend.Operator (Located { end = { col = 10, row = 10 }, start = { col = 9, row = 10 } } Multiply) (Located { end = { col = 8, row = 10 }, start = { col = 6, row = 10 } } (Frontend.Int 78)) (Located { end = { col = 12, row = 10 }, start = { col = 11, row = 10 } } (Frontend.Int 5)))) (Located { end = { col = 24, row = 10 }, start = { col = 15, row = 10 } } (Frontend.Operator (Located { end = { col = 22, row = 10 }, start = { col = 21, row = 10 } } Multiply) (Located { end = { col = 20, row = 10 }, start = { col = 15, row = 10 } } (Frontend.Operator (Located { end = { col = 18, row = 10 }, start = { col = 17, row = 10 } } Divide) (Located { end = { col = 16, row = 10 }, start = { col = 15, row = 10 } } (Frontend.Int 2)) (Located { end = { col = 20, row = 10 }, start = { col = 19, row = 10 } } (Frontend.Int 4)))) (Located { end = { col = 24, row = 10 }, start = { col = 23, row = 10 } } (Frontend.Int 5))))) })
                , Ok (ValueDeclaration { args = [], name = Located { end = { col = 3, row = 11 }, start = { col = 1, row = 11 } } "b3", valueExpr__ = Located { end = { col = 24, row = 11 }, start = { col = 6, row = 11 } } (Frontend.Operator (Located { end = { col = 14, row = 11 }, start = { col = 13, row = 11 } } Add) (Located { end = { col = 12, row = 11 }, start = { col = 6, row = 11 } } (Frontend.Operator (Located { end = { col = 10, row = 11 }, start = { col = 9, row = 11 } } Subtract) (Located { end = { col = 8, row = 11 }, start = { col = 6, row = 11 } } (Frontend.Int 78)) (Located { end = { col = 12, row = 11 }, start = { col = 11, row = 11 } } (Frontend.Int 5)))) (Located { end = { col = 24, row = 11 }, start = { col = 15, row = 11 } } (Frontend.Operator (Located { end = { col = 22, row = 11 }, start = { col = 21, row = 11 } } Multiply) (Located { end = { col = 20, row = 11 }, start = { col = 15, row = 11 } } (Frontend.Operator (Located { end = { col = 18, row = 11 }, start = { col = 17, row = 11 } } Divide) (Located { end = { col = 16, row = 11 }, start = { col = 15, row = 11 } } (Frontend.Int 2)) (Located { end = { col = 20, row = 11 }, start = { col = 19, row = 11 } } (Frontend.Int 4)))) (Located { end = { col = 24, row = 11 }, start = { col = 23, row = 11 } } (Frontend.Int 5))))) })
                , Ok (ValueDeclaration { args = [], name = Located { end = { col = 3, row = 12 }, start = { col = 1, row = 12 } } "b4", valueExpr__ = Located { end = { col = 24, row = 12 }, start = { col = 6, row = 12 } } (Frontend.Operator (Located { end = { col = 22, row = 12 }, start = { col = 21, row = 12 } } Add) (Located { end = { col = 20, row = 12 }, start = { col = 6, row = 12 } } (Frontend.Operator (Located { end = { col = 18, row = 12 }, start = { col = 17, row = 12 } } Divide) (Located { end = { col = 16, row = 12 }, start = { col = 6, row = 12 } } (Frontend.Operator (Located { end = { col = 14, row = 12 }, start = { col = 13, row = 12 } } Divide) (Located { end = { col = 12, row = 12 }, start = { col = 6, row = 12 } } (Frontend.Operator (Located { end = { col = 10, row = 12 }, start = { col = 9, row = 12 } } Divide) (Located { end = { col = 8, row = 12 }, start = { col = 6, row = 12 } } (Frontend.Int 78)) (Located { end = { col = 12, row = 12 }, start = { col = 11, row = 12 } } (Frontend.Int 5)))) (Located { end = { col = 16, row = 12 }, start = { col = 15, row = 12 } } (Frontend.Int 2)))) (Located { end = { col = 20, row = 12 }, start = { col = 19, row = 12 } } (Frontend.Int 4)))) (Located { end = { col = 24, row = 12 }, start = { col = 23, row = 12 } } (Frontend.Int 5))) })
                ]
      , lexed =
            Ok
                [ Located { end = { col = 2, row = 1 }, start = { col = 1, row = 1 } } (Identifier { name = "a", qualifiers = [] })
                , Located { end = { col = 3, row = 1 }, start = { col = 2, row = 1 } } (Whitespace 1)
                , Located { end = { col = 4, row = 1 }, start = { col = 3, row = 1 } } (Sigil Assign)
                , Located { end = { col = 5, row = 1 }, start = { col = 4, row = 1 } } (Whitespace 1)
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (NumericLiteral "5")
                , Located { end = { col = 7, row = 1 }, start = { col = 6, row = 1 } } (Whitespace 1)
                , Located { end = { col = 8, row = 1 }, start = { col = 7, row = 1 } } (Sigil (Operator Multiply))
                , Located { end = { col = 9, row = 1 }, start = { col = 8, row = 1 } } (Whitespace 1)
                , Located { end = { col = 10, row = 1 }, start = { col = 9, row = 1 } } (NumericLiteral "5")
                , Located { end = { col = 11, row = 1 }, start = { col = 10, row = 1 } } (Whitespace 1)
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Sigil (Operator Add))
                , Located { end = { col = 13, row = 1 }, start = { col = 12, row = 1 } } (Whitespace 1)
                , Located { end = { col = 14, row = 1 }, start = { col = 13, row = 1 } } (NumericLiteral "6")
                , Located { end = { col = 1, row = 2 }, start = { col = 14, row = 1 } } (Newlines [] 0)
                , Located { end = { col = 3, row = 2 }, start = { col = 1, row = 2 } } (Identifier { name = "a1", qualifiers = [] })
                , Located { end = { col = 4, row = 2 }, start = { col = 3, row = 2 } } (Whitespace 1)
                , Located { end = { col = 5, row = 2 }, start = { col = 4, row = 2 } } (Sigil Assign)
                , Located { end = { col = 7, row = 2 }, start = { col = 5, row = 2 } } (Whitespace 2)
                , Located { end = { col = 8, row = 2 }, start = { col = 7, row = 2 } } (NumericLiteral "7")
                , Located { end = { col = 9, row = 2 }, start = { col = 8, row = 2 } } (Whitespace 1)
                , Located { end = { col = 10, row = 2 }, start = { col = 9, row = 2 } } (Sigil (Operator Add))
                , Located { end = { col = 11, row = 2 }, start = { col = 10, row = 2 } } (Whitespace 1)
                , Located { end = { col = 12, row = 2 }, start = { col = 11, row = 2 } } (NumericLiteral "5")
                , Located { end = { col = 13, row = 2 }, start = { col = 12, row = 2 } } (Whitespace 1)
                , Located { end = { col = 14, row = 2 }, start = { col = 13, row = 2 } } (Sigil (Operator Multiply))
                , Located { end = { col = 15, row = 2 }, start = { col = 14, row = 2 } } (Whitespace 1)
                , Located { end = { col = 16, row = 2 }, start = { col = 15, row = 2 } } (NumericLiteral "5")
                , Located { end = { col = 1, row = 3 }, start = { col = 16, row = 2 } } (Newlines [] 0)
                , Located { end = { col = 4, row = 3 }, start = { col = 1, row = 3 } } (Identifier { name = "a11", qualifiers = [] })
                , Located { end = { col = 5, row = 3 }, start = { col = 4, row = 3 } } (Whitespace 1)
                , Located { end = { col = 6, row = 3 }, start = { col = 5, row = 3 } } (Sigil Assign)
                , Located { end = { col = 8, row = 3 }, start = { col = 6, row = 3 } } (Whitespace 2)
                , Located { end = { col = 9, row = 3 }, start = { col = 8, row = 3 } } (NumericLiteral "7")
                , Located { end = { col = 10, row = 3 }, start = { col = 9, row = 3 } } (Whitespace 1)
                , Located { end = { col = 11, row = 3 }, start = { col = 10, row = 3 } } (Sigil (Operator Add))
                , Located { end = { col = 12, row = 3 }, start = { col = 11, row = 3 } } (Whitespace 1)
                , Located { end = { col = 13, row = 3 }, start = { col = 12, row = 3 } } (NumericLiteral "5")
                , Located { end = { col = 14, row = 3 }, start = { col = 13, row = 3 } } (Whitespace 1)
                , Located { end = { col = 15, row = 3 }, start = { col = 14, row = 3 } } (Sigil (Operator Multiply))
                , Located { end = { col = 16, row = 3 }, start = { col = 15, row = 3 } } (Whitespace 1)
                , Located { end = { col = 17, row = 3 }, start = { col = 16, row = 3 } } (NumericLiteral "5")
                , Located { end = { col = 18, row = 3 }, start = { col = 17, row = 3 } } (Whitespace 1)
                , Located { end = { col = 19, row = 3 }, start = { col = 18, row = 3 } } (Sigil (Operator Add))
                , Located { end = { col = 20, row = 3 }, start = { col = 19, row = 3 } } (Whitespace 1)
                , Located { end = { col = 21, row = 3 }, start = { col = 20, row = 3 } } (NumericLiteral "6")
                , Located { end = { col = 1, row = 4 }, start = { col = 21, row = 3 } } (Newlines [] 0)
                , Located { end = { col = 3, row = 4 }, start = { col = 1, row = 4 } } (Identifier { name = "a2", qualifiers = [] })
                , Located { end = { col = 4, row = 4 }, start = { col = 3, row = 4 } } (Whitespace 1)
                , Located { end = { col = 5, row = 4 }, start = { col = 4, row = 4 } } (Sigil Assign)
                , Located { end = { col = 6, row = 4 }, start = { col = 5, row = 4 } } (Whitespace 1)
                , Located { end = { col = 9, row = 4 }, start = { col = 6, row = 4 } } (NumericLiteral "100")
                , Located { end = { col = 11, row = 4 }, start = { col = 9, row = 4 } } (Whitespace 2)
                , Located { end = { col = 12, row = 4 }, start = { col = 11, row = 4 } } (Sigil (Operator Add))
                , Located { end = { col = 13, row = 4 }, start = { col = 12, row = 4 } } (Whitespace 1)
                , Located { end = { col = 14, row = 4 }, start = { col = 13, row = 4 } } (NumericLiteral "5")
                , Located { end = { col = 15, row = 4 }, start = { col = 14, row = 4 } } (Whitespace 1)
                , Located { end = { col = 16, row = 4 }, start = { col = 15, row = 4 } } (Sigil (Operator Multiply))
                , Located { end = { col = 17, row = 4 }, start = { col = 16, row = 4 } } (Whitespace 1)
                , Located { end = { col = 18, row = 4 }, start = { col = 17, row = 4 } } (NumericLiteral "5")
                , Located { end = { col = 1, row = 5 }, start = { col = 18, row = 4 } } (Newlines [] 0)
                , Located { end = { col = 3, row = 5 }, start = { col = 1, row = 5 } } (Identifier { name = "a3", qualifiers = [] })
                , Located { end = { col = 4, row = 5 }, start = { col = 3, row = 5 } } (Whitespace 1)
                , Located { end = { col = 5, row = 5 }, start = { col = 4, row = 5 } } (Sigil Assign)
                , Located { end = { col = 6, row = 5 }, start = { col = 5, row = 5 } } (Whitespace 1)
                , Located { end = { col = 9, row = 5 }, start = { col = 6, row = 5 } } (NumericLiteral "345")
                , Located { end = { col = 10, row = 5 }, start = { col = 9, row = 5 } } (Whitespace 1)
                , Located { end = { col = 11, row = 5 }, start = { col = 10, row = 5 } } (Sigil (Operator Multiply))
                , Located { end = { col = 12, row = 5 }, start = { col = 11, row = 5 } } (Whitespace 1)
                , Located { end = { col = 16, row = 5 }, start = { col = 12, row = 5 } } (NumericLiteral "2234")
                , Located { end = { col = 17, row = 5 }, start = { col = 16, row = 5 } } (Whitespace 1)
                , Located { end = { col = 18, row = 5 }, start = { col = 17, row = 5 } } (Sigil (Operator Add))
                , Located { end = { col = 19, row = 5 }, start = { col = 18, row = 5 } } (Whitespace 1)
                , Located { end = { col = 23, row = 5 }, start = { col = 19, row = 5 } } (NumericLiteral "2342")
                , Located { end = { col = 24, row = 5 }, start = { col = 23, row = 5 } } (Whitespace 1)
                , Located { end = { col = 25, row = 5 }, start = { col = 24, row = 5 } } (Sigil (Operator Multiply))
                , Located { end = { col = 26, row = 5 }, start = { col = 25, row = 5 } } (Whitespace 1)
                , Located { end = { col = 30, row = 5 }, start = { col = 26, row = 5 } } (NumericLiteral "1010")
                , Located { end = { col = 1, row = 8 }, start = { col = 30, row = 5 } }
                    (Newlines
                        [ 0
                        , 0
                        ]
                        0
                    )
                , Located { end = { col = 2, row = 8 }, start = { col = 1, row = 8 } } (Identifier { name = "b", qualifiers = [] })
                , Located { end = { col = 3, row = 8 }, start = { col = 2, row = 8 } } (Whitespace 1)
                , Located { end = { col = 4, row = 8 }, start = { col = 3, row = 8 } } (Sigil Assign)
                , Located { end = { col = 5, row = 8 }, start = { col = 4, row = 8 } } (Whitespace 1)
                , Located { end = { col = 7, row = 8 }, start = { col = 5, row = 8 } } (NumericLiteral "78")
                , Located { end = { col = 8, row = 8 }, start = { col = 7, row = 8 } } (Whitespace 1)
                , Located { end = { col = 9, row = 8 }, start = { col = 8, row = 8 } } (Sigil (Operator Add))
                , Located { end = { col = 10, row = 8 }, start = { col = 9, row = 8 } } (Whitespace 1)
                , Located { end = { col = 11, row = 8 }, start = { col = 10, row = 8 } } (NumericLiteral "5")
                , Located { end = { col = 12, row = 8 }, start = { col = 11, row = 8 } } (Whitespace 1)
                , Located { end = { col = 13, row = 8 }, start = { col = 12, row = 8 } } (Sigil (Operator Multiply))
                , Located { end = { col = 14, row = 8 }, start = { col = 13, row = 8 } } (Whitespace 1)
                , Located { end = { col = 15, row = 8 }, start = { col = 14, row = 8 } } (NumericLiteral "2")
                , Located { end = { col = 16, row = 8 }, start = { col = 15, row = 8 } } (Whitespace 1)
                , Located { end = { col = 17, row = 8 }, start = { col = 16, row = 8 } } (Sigil (Operator Divide))
                , Located { end = { col = 18, row = 8 }, start = { col = 17, row = 8 } } (Whitespace 1)
                , Located { end = { col = 19, row = 8 }, start = { col = 18, row = 8 } } (NumericLiteral "4")
                , Located { end = { col = 20, row = 8 }, start = { col = 19, row = 8 } } (Whitespace 1)
                , Located { end = { col = 21, row = 8 }, start = { col = 20, row = 8 } } (Sigil (Operator Multiply))
                , Located { end = { col = 22, row = 8 }, start = { col = 21, row = 8 } } (Whitespace 1)
                , Located { end = { col = 23, row = 8 }, start = { col = 22, row = 8 } } (NumericLiteral "5")
                , Located { end = { col = 1, row = 9 }, start = { col = 23, row = 8 } } (Newlines [] 0)
                , Located { end = { col = 3, row = 9 }, start = { col = 1, row = 9 } } (Identifier { name = "b1", qualifiers = [] })
                , Located { end = { col = 4, row = 9 }, start = { col = 3, row = 9 } } (Whitespace 1)
                , Located { end = { col = 5, row = 9 }, start = { col = 4, row = 9 } } (Sigil Assign)
                , Located { end = { col = 6, row = 9 }, start = { col = 5, row = 9 } } (Whitespace 1)
                , Located { end = { col = 8, row = 9 }, start = { col = 6, row = 9 } } (NumericLiteral "78")
                , Located { end = { col = 9, row = 9 }, start = { col = 8, row = 9 } } (Whitespace 1)
                , Located { end = { col = 10, row = 9 }, start = { col = 9, row = 9 } } (Sigil (Operator Multiply))
                , Located { end = { col = 11, row = 9 }, start = { col = 10, row = 9 } } (Whitespace 1)
                , Located { end = { col = 12, row = 9 }, start = { col = 11, row = 9 } } (NumericLiteral "5")
                , Located { end = { col = 13, row = 9 }, start = { col = 12, row = 9 } } (Whitespace 1)
                , Located { end = { col = 14, row = 9 }, start = { col = 13, row = 9 } } (Sigil (Operator Multiply))
                , Located { end = { col = 15, row = 9 }, start = { col = 14, row = 9 } } (Whitespace 1)
                , Located { end = { col = 16, row = 9 }, start = { col = 15, row = 9 } } (NumericLiteral "2")
                , Located { end = { col = 17, row = 9 }, start = { col = 16, row = 9 } } (Whitespace 1)
                , Located { end = { col = 18, row = 9 }, start = { col = 17, row = 9 } } (Sigil (Operator Divide))
                , Located { end = { col = 19, row = 9 }, start = { col = 18, row = 9 } } (Whitespace 1)
                , Located { end = { col = 20, row = 9 }, start = { col = 19, row = 9 } } (NumericLiteral "4")
                , Located { end = { col = 21, row = 9 }, start = { col = 20, row = 9 } } (Whitespace 1)
                , Located { end = { col = 22, row = 9 }, start = { col = 21, row = 9 } } (Sigil (Operator Subtract))
                , Located { end = { col = 23, row = 9 }, start = { col = 22, row = 9 } } (Whitespace 1)
                , Located { end = { col = 24, row = 9 }, start = { col = 23, row = 9 } } (NumericLiteral "5")
                , Located { end = { col = 1, row = 10 }, start = { col = 24, row = 9 } } (Newlines [] 0)
                , Located { end = { col = 3, row = 10 }, start = { col = 1, row = 10 } } (Identifier { name = "b2", qualifiers = [] })
                , Located { end = { col = 4, row = 10 }, start = { col = 3, row = 10 } } (Whitespace 1)
                , Located { end = { col = 5, row = 10 }, start = { col = 4, row = 10 } } (Sigil Assign)
                , Located { end = { col = 6, row = 10 }, start = { col = 5, row = 10 } } (Whitespace 1)
                , Located { end = { col = 8, row = 10 }, start = { col = 6, row = 10 } } (NumericLiteral "78")
                , Located { end = { col = 9, row = 10 }, start = { col = 8, row = 10 } } (Whitespace 1)
                , Located { end = { col = 10, row = 10 }, start = { col = 9, row = 10 } } (Sigil (Operator Multiply))
                , Located { end = { col = 11, row = 10 }, start = { col = 10, row = 10 } } (Whitespace 1)
                , Located { end = { col = 12, row = 10 }, start = { col = 11, row = 10 } } (NumericLiteral "5")
                , Located { end = { col = 13, row = 10 }, start = { col = 12, row = 10 } } (Whitespace 1)
                , Located { end = { col = 14, row = 10 }, start = { col = 13, row = 10 } } (Sigil (Operator Subtract))
                , Located { end = { col = 15, row = 10 }, start = { col = 14, row = 10 } } (Whitespace 1)
                , Located { end = { col = 16, row = 10 }, start = { col = 15, row = 10 } } (NumericLiteral "2")
                , Located { end = { col = 17, row = 10 }, start = { col = 16, row = 10 } } (Whitespace 1)
                , Located { end = { col = 18, row = 10 }, start = { col = 17, row = 10 } } (Sigil (Operator Divide))
                , Located { end = { col = 19, row = 10 }, start = { col = 18, row = 10 } } (Whitespace 1)
                , Located { end = { col = 20, row = 10 }, start = { col = 19, row = 10 } } (NumericLiteral "4")
                , Located { end = { col = 21, row = 10 }, start = { col = 20, row = 10 } } (Whitespace 1)
                , Located { end = { col = 22, row = 10 }, start = { col = 21, row = 10 } } (Sigil (Operator Multiply))
                , Located { end = { col = 23, row = 10 }, start = { col = 22, row = 10 } } (Whitespace 1)
                , Located { end = { col = 24, row = 10 }, start = { col = 23, row = 10 } } (NumericLiteral "5")
                , Located { end = { col = 1, row = 11 }, start = { col = 24, row = 10 } } (Newlines [] 0)
                , Located { end = { col = 3, row = 11 }, start = { col = 1, row = 11 } } (Identifier { name = "b3", qualifiers = [] })
                , Located { end = { col = 4, row = 11 }, start = { col = 3, row = 11 } } (Whitespace 1)
                , Located { end = { col = 5, row = 11 }, start = { col = 4, row = 11 } } (Sigil Assign)
                , Located { end = { col = 6, row = 11 }, start = { col = 5, row = 11 } } (Whitespace 1)
                , Located { end = { col = 8, row = 11 }, start = { col = 6, row = 11 } } (NumericLiteral "78")
                , Located { end = { col = 9, row = 11 }, start = { col = 8, row = 11 } } (Whitespace 1)
                , Located { end = { col = 10, row = 11 }, start = { col = 9, row = 11 } } (Sigil (Operator Subtract))
                , Located { end = { col = 11, row = 11 }, start = { col = 10, row = 11 } } (Whitespace 1)
                , Located { end = { col = 12, row = 11 }, start = { col = 11, row = 11 } } (NumericLiteral "5")
                , Located { end = { col = 13, row = 11 }, start = { col = 12, row = 11 } } (Whitespace 1)
                , Located { end = { col = 14, row = 11 }, start = { col = 13, row = 11 } } (Sigil (Operator Add))
                , Located { end = { col = 15, row = 11 }, start = { col = 14, row = 11 } } (Whitespace 1)
                , Located { end = { col = 16, row = 11 }, start = { col = 15, row = 11 } } (NumericLiteral "2")
                , Located { end = { col = 17, row = 11 }, start = { col = 16, row = 11 } } (Whitespace 1)
                , Located { end = { col = 18, row = 11 }, start = { col = 17, row = 11 } } (Sigil (Operator Divide))
                , Located { end = { col = 19, row = 11 }, start = { col = 18, row = 11 } } (Whitespace 1)
                , Located { end = { col = 20, row = 11 }, start = { col = 19, row = 11 } } (NumericLiteral "4")
                , Located { end = { col = 21, row = 11 }, start = { col = 20, row = 11 } } (Whitespace 1)
                , Located { end = { col = 22, row = 11 }, start = { col = 21, row = 11 } } (Sigil (Operator Multiply))
                , Located { end = { col = 23, row = 11 }, start = { col = 22, row = 11 } } (Whitespace 1)
                , Located { end = { col = 24, row = 11 }, start = { col = 23, row = 11 } } (NumericLiteral "5")
                , Located { end = { col = 1, row = 12 }, start = { col = 24, row = 11 } } (Newlines [] 0)
                , Located { end = { col = 3, row = 12 }, start = { col = 1, row = 12 } } (Identifier { name = "b4", qualifiers = [] })
                , Located { end = { col = 4, row = 12 }, start = { col = 3, row = 12 } } (Whitespace 1)
                , Located { end = { col = 5, row = 12 }, start = { col = 4, row = 12 } } (Sigil Assign)
                , Located { end = { col = 6, row = 12 }, start = { col = 5, row = 12 } } (Whitespace 1)
                , Located { end = { col = 8, row = 12 }, start = { col = 6, row = 12 } } (NumericLiteral "78")
                , Located { end = { col = 9, row = 12 }, start = { col = 8, row = 12 } } (Whitespace 1)
                , Located { end = { col = 10, row = 12 }, start = { col = 9, row = 12 } } (Sigil (Operator Divide))
                , Located { end = { col = 11, row = 12 }, start = { col = 10, row = 12 } } (Whitespace 1)
                , Located { end = { col = 12, row = 12 }, start = { col = 11, row = 12 } } (NumericLiteral "5")
                , Located { end = { col = 13, row = 12 }, start = { col = 12, row = 12 } } (Whitespace 1)
                , Located { end = { col = 14, row = 12 }, start = { col = 13, row = 12 } } (Sigil (Operator Divide))
                , Located { end = { col = 15, row = 12 }, start = { col = 14, row = 12 } } (Whitespace 1)
                , Located { end = { col = 16, row = 12 }, start = { col = 15, row = 12 } } (NumericLiteral "2")
                , Located { end = { col = 17, row = 12 }, start = { col = 16, row = 12 } } (Whitespace 1)
                , Located { end = { col = 18, row = 12 }, start = { col = 17, row = 12 } } (Sigil (Operator Divide))
                , Located { end = { col = 19, row = 12 }, start = { col = 18, row = 12 } } (Whitespace 1)
                , Located { end = { col = 20, row = 12 }, start = { col = 19, row = 12 } } (NumericLiteral "4")
                , Located { end = { col = 21, row = 12 }, start = { col = 20, row = 12 } } (Whitespace 1)
                , Located { end = { col = 22, row = 12 }, start = { col = 21, row = 12 } } (Sigil (Operator Add))
                , Located { end = { col = 23, row = 12 }, start = { col = 22, row = 12 } } (Whitespace 1)
                , Located { end = { col = 24, row = 12 }, start = { col = 23, row = 12 } } (NumericLiteral "5")
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
                [ Ok (ValueDeclaration { args = [], name = Located { end = { col = 2, row = 1 }, start = { col = 1, row = 1 } } "a", valueExpr__ = Located { end = { col = 10, row = 1 }, start = { col = 5, row = 1 } } (Frontend.Operator (Located { end = { col = 8, row = 1 }, start = { col = 7, row = 1 } } Subtract) (Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Frontend.Int 5)) (Located { end = { col = 10, row = 1 }, start = { col = 9, row = 1 } } (Frontend.Int 5))) })
                , Ok (ValueDeclaration { args = [], name = Located { end = { col = 2, row = 3 }, start = { col = 1, row = 3 } } "b", valueExpr__ = Located { end = { col = 23, row = 3 }, start = { col = 5, row = 3 } } (Frontend.Operator (Located { end = { col = 21, row = 3 }, start = { col = 20, row = 3 } } Add) (Located { end = { col = 19, row = 3 }, start = { col = 5, row = 3 } } (Frontend.Operator (Located { end = { col = 17, row = 3 }, start = { col = 16, row = 3 } } Subtract) (Located { end = { col = 15, row = 3 }, start = { col = 5, row = 3 } } (Frontend.Operator (Located { end = { col = 13, row = 3 }, start = { col = 12, row = 3 } } Add) (Located { end = { col = 11, row = 3 }, start = { col = 5, row = 3 } } (Frontend.Operator (Located { end = { col = 9, row = 3 }, start = { col = 8, row = 3 } } Add) (Located { end = { col = 7, row = 3 }, start = { col = 5, row = 3 } } (Frontend.Int 78)) (Located { end = { col = 11, row = 3 }, start = { col = 10, row = 3 } } (Frontend.Int 5)))) (Located { end = { col = 15, row = 3 }, start = { col = 14, row = 3 } } (Frontend.Int 2)))) (Located { end = { col = 19, row = 3 }, start = { col = 18, row = 3 } } (Frontend.Int 4)))) (Located { end = { col = 23, row = 3 }, start = { col = 22, row = 3 } } (Frontend.Int 5))) })
                ]
      , lexed =
            Ok
                [ Located { end = { col = 2, row = 1 }, start = { col = 1, row = 1 } } (Identifier { name = "a", qualifiers = [] })
                , Located { end = { col = 3, row = 1 }, start = { col = 2, row = 1 } } (Whitespace 1)
                , Located { end = { col = 4, row = 1 }, start = { col = 3, row = 1 } } (Sigil Assign)
                , Located { end = { col = 5, row = 1 }, start = { col = 4, row = 1 } } (Whitespace 1)
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (NumericLiteral "5")
                , Located { end = { col = 7, row = 1 }, start = { col = 6, row = 1 } } (Whitespace 1)
                , Located { end = { col = 8, row = 1 }, start = { col = 7, row = 1 } } (Sigil (Operator Subtract))
                , Located { end = { col = 9, row = 1 }, start = { col = 8, row = 1 } } (Whitespace 1)
                , Located { end = { col = 10, row = 1 }, start = { col = 9, row = 1 } } (NumericLiteral "5")
                , Located { end = { col = 1, row = 3 }, start = { col = 10, row = 1 } }
                    (Newlines
                        [ 0
                        ]
                        0
                    )
                , Located { end = { col = 2, row = 3 }, start = { col = 1, row = 3 } } (Identifier { name = "b", qualifiers = [] })
                , Located { end = { col = 3, row = 3 }, start = { col = 2, row = 3 } } (Whitespace 1)
                , Located { end = { col = 4, row = 3 }, start = { col = 3, row = 3 } } (Sigil Assign)
                , Located { end = { col = 5, row = 3 }, start = { col = 4, row = 3 } } (Whitespace 1)
                , Located { end = { col = 7, row = 3 }, start = { col = 5, row = 3 } } (NumericLiteral "78")
                , Located { end = { col = 8, row = 3 }, start = { col = 7, row = 3 } } (Whitespace 1)
                , Located { end = { col = 9, row = 3 }, start = { col = 8, row = 3 } } (Sigil (Operator Add))
                , Located { end = { col = 10, row = 3 }, start = { col = 9, row = 3 } } (Whitespace 1)
                , Located { end = { col = 11, row = 3 }, start = { col = 10, row = 3 } } (NumericLiteral "5")
                , Located { end = { col = 12, row = 3 }, start = { col = 11, row = 3 } } (Whitespace 1)
                , Located { end = { col = 13, row = 3 }, start = { col = 12, row = 3 } } (Sigil (Operator Add))
                , Located { end = { col = 14, row = 3 }, start = { col = 13, row = 3 } } (Whitespace 1)
                , Located { end = { col = 15, row = 3 }, start = { col = 14, row = 3 } } (NumericLiteral "2")
                , Located { end = { col = 16, row = 3 }, start = { col = 15, row = 3 } } (Whitespace 1)
                , Located { end = { col = 17, row = 3 }, start = { col = 16, row = 3 } } (Sigil (Operator Subtract))
                , Located { end = { col = 18, row = 3 }, start = { col = 17, row = 3 } } (Whitespace 1)
                , Located { end = { col = 19, row = 3 }, start = { col = 18, row = 3 } } (NumericLiteral "4")
                , Located { end = { col = 20, row = 3 }, start = { col = 19, row = 3 } } (Whitespace 1)
                , Located { end = { col = 21, row = 3 }, start = { col = 20, row = 3 } } (Sigil (Operator Add))
                , Located { end = { col = 22, row = 3 }, start = { col = 21, row = 3 } } (Whitespace 1)
                , Located { end = { col = 23, row = 3 }, start = { col = 22, row = 3 } } (NumericLiteral "5")
                , Located { end = { col = 1, row = 4 }, start = { col = 23, row = 3 } } (Newlines [] 0)
                ]
      }
    , { name = "type-alias"
      , source = """type alias Model = List Int
"""
      , pretty = """
        ( ( Ok
          , ( ValueDeclaration
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
                        , ty = "Model"
                        }
                    )
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Keyword Type)
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Keyword Alias)
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Whitespace 1)
                , Located { end = { col = 17, row = 1 }, start = { col = 12, row = 1 } } (Identifier { name = "Model", qualifiers = [] })
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Whitespace 1)
                , Located { end = { col = 19, row = 1 }, start = { col = 18, row = 1 } } (Sigil Assign)
                , Located { end = { col = 20, row = 1 }, start = { col = 19, row = 1 } } (Whitespace 1)
                , Located { end = { col = 24, row = 1 }, start = { col = 20, row = 1 } } (Identifier { name = "List", qualifiers = [] })
                , Located { end = { col = 25, row = 1 }, start = { col = 24, row = 1 } } (Whitespace 1)
                , Located { end = { col = 28, row = 1 }, start = { col = 25, row = 1 } } (Identifier { name = "Int", qualifiers = [] })
                , Located { end = { col = 1, row = 2 }, start = { col = 28, row = 1 } } (Newlines [] 0)
                ]
      }
    , { name = "type-alias-and-expression"
      , source = """type alias Model = List Int

expr hi = 77
"""
      , pretty = """
        ( ( Ok
          , ( ValueDeclaration
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
                        , ty = "Model"
                        }
                    )
                , Ok
                    (ValueDeclaration
                        { args =
                            [ Located { end = { col = 8, row = 3 }, start = { col = 6, row = 3 } } "hi"
                            ]
                        , name = Located { end = { col = 5, row = 3 }, start = { col = 1, row = 3 } } "expr"
                        , valueExpr__ = Located { end = { col = 13, row = 3 }, start = { col = 11, row = 3 } } (Frontend.Int 77)
                        }
                    )
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Keyword Type)
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Keyword Alias)
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Whitespace 1)
                , Located { end = { col = 17, row = 1 }, start = { col = 12, row = 1 } } (Identifier { name = "Model", qualifiers = [] })
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Whitespace 1)
                , Located { end = { col = 19, row = 1 }, start = { col = 18, row = 1 } } (Sigil Assign)
                , Located { end = { col = 20, row = 1 }, start = { col = 19, row = 1 } } (Whitespace 1)
                , Located { end = { col = 24, row = 1 }, start = { col = 20, row = 1 } } (Identifier { name = "List", qualifiers = [] })
                , Located { end = { col = 25, row = 1 }, start = { col = 24, row = 1 } } (Whitespace 1)
                , Located { end = { col = 28, row = 1 }, start = { col = 25, row = 1 } } (Identifier { name = "Int", qualifiers = [] })
                , Located { end = { col = 1, row = 3 }, start = { col = 28, row = 1 } }
                    (Newlines
                        [ 0
                        ]
                        0
                    )
                , Located { end = { col = 5, row = 3 }, start = { col = 1, row = 3 } } (Identifier { name = "expr", qualifiers = [] })
                , Located { end = { col = 6, row = 3 }, start = { col = 5, row = 3 } } (Whitespace 1)
                , Located { end = { col = 8, row = 3 }, start = { col = 6, row = 3 } } (Identifier { name = "hi", qualifiers = [] })
                , Located { end = { col = 9, row = 3 }, start = { col = 8, row = 3 } } (Whitespace 1)
                , Located { end = { col = 10, row = 3 }, start = { col = 9, row = 3 } } (Sigil Assign)
                , Located { end = { col = 11, row = 3 }, start = { col = 10, row = 3 } } (Whitespace 1)
                , Located { end = { col = 13, row = 3 }, start = { col = 11, row = 3 } } (NumericLiteral "77")
                , Located { end = { col = 1, row = 4 }, start = { col = 13, row = 3 } } (Newlines [] 0)
                ]
      }
    , { name = "type-alias-bracket-in-record"
      , source = """type alias Ty = { hi: (Int) }
"""
      , pretty = """
        ( ( Ok
          , ( ValueDeclaration
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
                        , ty = "Ty"
                        }
                    )
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Keyword Type)
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Keyword Alias)
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Whitespace 1)
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Identifier { name = "Ty", qualifiers = [] })
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Whitespace 1)
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Sigil Assign)
                , Located { end = { col = 17, row = 1 }, start = { col = 16, row = 1 } } (Whitespace 1)
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Sigil (Bracket Curly Open))
                , Located { end = { col = 19, row = 1 }, start = { col = 18, row = 1 } } (Whitespace 1)
                , Located { end = { col = 21, row = 1 }, start = { col = 19, row = 1 } } (Identifier { name = "hi", qualifiers = [] })
                , Located { end = { col = 22, row = 1 }, start = { col = 21, row = 1 } } (Sigil Colon)
                , Located { end = { col = 23, row = 1 }, start = { col = 22, row = 1 } } (Whitespace 1)
                , Located { end = { col = 24, row = 1 }, start = { col = 23, row = 1 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 27, row = 1 }, start = { col = 24, row = 1 } } (Identifier { name = "Int", qualifiers = [] })
                , Located { end = { col = 28, row = 1 }, start = { col = 27, row = 1 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 29, row = 1 }, start = { col = 28, row = 1 } } (Whitespace 1)
                , Located { end = { col = 30, row = 1 }, start = { col = 29, row = 1 } } (Sigil (Bracket Curly Close))
                , Located { end = { col = 1, row = 2 }, start = { col = 30, row = 1 } } (Newlines [] 0)
                ]
      }
    , { name = "type-alias-function"
      , source = """type alias Function = List Int -> List (List Int)
"""
      , pretty = """
        ( ( Ok
          , ( ValueDeclaration
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
                        , ty = "Function"
                        }
                    )
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Keyword Type)
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Keyword Alias)
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Whitespace 1)
                , Located { end = { col = 20, row = 1 }, start = { col = 12, row = 1 } } (Identifier { name = "Function", qualifiers = [] })
                , Located { end = { col = 21, row = 1 }, start = { col = 20, row = 1 } } (Whitespace 1)
                , Located { end = { col = 22, row = 1 }, start = { col = 21, row = 1 } } (Sigil Assign)
                , Located { end = { col = 23, row = 1 }, start = { col = 22, row = 1 } } (Whitespace 1)
                , Located { end = { col = 27, row = 1 }, start = { col = 23, row = 1 } } (Identifier { name = "List", qualifiers = [] })
                , Located { end = { col = 28, row = 1 }, start = { col = 27, row = 1 } } (Whitespace 1)
                , Located { end = { col = 31, row = 1 }, start = { col = 28, row = 1 } } (Identifier { name = "Int", qualifiers = [] })
                , Located { end = { col = 32, row = 1 }, start = { col = 31, row = 1 } } (Whitespace 1)
                , Located { end = { col = 34, row = 1 }, start = { col = 32, row = 1 } } (Sigil ThinArrow)
                , Located { end = { col = 35, row = 1 }, start = { col = 34, row = 1 } } (Whitespace 1)
                , Located { end = { col = 39, row = 1 }, start = { col = 35, row = 1 } } (Identifier { name = "List", qualifiers = [] })
                , Located { end = { col = 40, row = 1 }, start = { col = 39, row = 1 } } (Whitespace 1)
                , Located { end = { col = 41, row = 1 }, start = { col = 40, row = 1 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 45, row = 1 }, start = { col = 41, row = 1 } } (Identifier { name = "List", qualifiers = [] })
                , Located { end = { col = 46, row = 1 }, start = { col = 45, row = 1 } } (Whitespace 1)
                , Located { end = { col = 49, row = 1 }, start = { col = 46, row = 1 } } (Identifier { name = "Int", qualifiers = [] })
                , Located { end = { col = 50, row = 1 }, start = { col = 49, row = 1 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 1, row = 2 }, start = { col = 50, row = 1 } } (Newlines [] 0)
                ]
      }
    , { name = "type-alias-function-binding-order"
      , source = """type alias Function = A -> B -> C
type alias Function = A -> B -> C -> D
"""
      , pretty = """
        ( ( Ok
          , ( ValueDeclaration
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
          , ( ValueDeclaration
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
                        , ty = "Function"
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
                        , ty = "Function"
                        }
                    )
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Keyword Type)
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Keyword Alias)
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Whitespace 1)
                , Located { end = { col = 20, row = 1 }, start = { col = 12, row = 1 } } (Identifier { name = "Function", qualifiers = [] })
                , Located { end = { col = 21, row = 1 }, start = { col = 20, row = 1 } } (Whitespace 1)
                , Located { end = { col = 22, row = 1 }, start = { col = 21, row = 1 } } (Sigil Assign)
                , Located { end = { col = 23, row = 1 }, start = { col = 22, row = 1 } } (Whitespace 1)
                , Located { end = { col = 24, row = 1 }, start = { col = 23, row = 1 } } (Identifier { name = "A", qualifiers = [] })
                , Located { end = { col = 25, row = 1 }, start = { col = 24, row = 1 } } (Whitespace 1)
                , Located { end = { col = 27, row = 1 }, start = { col = 25, row = 1 } } (Sigil ThinArrow)
                , Located { end = { col = 28, row = 1 }, start = { col = 27, row = 1 } } (Whitespace 1)
                , Located { end = { col = 29, row = 1 }, start = { col = 28, row = 1 } } (Identifier { name = "B", qualifiers = [] })
                , Located { end = { col = 30, row = 1 }, start = { col = 29, row = 1 } } (Whitespace 1)
                , Located { end = { col = 32, row = 1 }, start = { col = 30, row = 1 } } (Sigil ThinArrow)
                , Located { end = { col = 33, row = 1 }, start = { col = 32, row = 1 } } (Whitespace 1)
                , Located { end = { col = 34, row = 1 }, start = { col = 33, row = 1 } } (Identifier { name = "C", qualifiers = [] })
                , Located { end = { col = 1, row = 2 }, start = { col = 34, row = 1 } } (Newlines [] 0)
                , Located { end = { col = 5, row = 2 }, start = { col = 1, row = 2 } } (Keyword Type)
                , Located { end = { col = 6, row = 2 }, start = { col = 5, row = 2 } } (Whitespace 1)
                , Located { end = { col = 11, row = 2 }, start = { col = 6, row = 2 } } (Keyword Alias)
                , Located { end = { col = 12, row = 2 }, start = { col = 11, row = 2 } } (Whitespace 1)
                , Located { end = { col = 20, row = 2 }, start = { col = 12, row = 2 } } (Identifier { name = "Function", qualifiers = [] })
                , Located { end = { col = 21, row = 2 }, start = { col = 20, row = 2 } } (Whitespace 1)
                , Located { end = { col = 22, row = 2 }, start = { col = 21, row = 2 } } (Sigil Assign)
                , Located { end = { col = 23, row = 2 }, start = { col = 22, row = 2 } } (Whitespace 1)
                , Located { end = { col = 24, row = 2 }, start = { col = 23, row = 2 } } (Identifier { name = "A", qualifiers = [] })
                , Located { end = { col = 25, row = 2 }, start = { col = 24, row = 2 } } (Whitespace 1)
                , Located { end = { col = 27, row = 2 }, start = { col = 25, row = 2 } } (Sigil ThinArrow)
                , Located { end = { col = 28, row = 2 }, start = { col = 27, row = 2 } } (Whitespace 1)
                , Located { end = { col = 29, row = 2 }, start = { col = 28, row = 2 } } (Identifier { name = "B", qualifiers = [] })
                , Located { end = { col = 30, row = 2 }, start = { col = 29, row = 2 } } (Whitespace 1)
                , Located { end = { col = 32, row = 2 }, start = { col = 30, row = 2 } } (Sigil ThinArrow)
                , Located { end = { col = 33, row = 2 }, start = { col = 32, row = 2 } } (Whitespace 1)
                , Located { end = { col = 34, row = 2 }, start = { col = 33, row = 2 } } (Identifier { name = "C", qualifiers = [] })
                , Located { end = { col = 35, row = 2 }, start = { col = 34, row = 2 } } (Whitespace 1)
                , Located { end = { col = 37, row = 2 }, start = { col = 35, row = 2 } } (Sigil ThinArrow)
                , Located { end = { col = 38, row = 2 }, start = { col = 37, row = 2 } } (Whitespace 1)
                , Located { end = { col = 39, row = 2 }, start = { col = 38, row = 2 } } (Identifier { name = "D", qualifiers = [] })
                , Located { end = { col = 1, row = 3 }, start = { col = 39, row = 2 } } (Newlines [] 0)
                ]
      }
    , { name = "type-alias-function-generic"
      , source = """type alias Function a = List Int -> List (List a)
"""
      , pretty = """
        ( ( Ok
          , ( ValueDeclaration
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
                            [ "a"
                            ]
                        , ty = "Function"
                        }
                    )
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Keyword Type)
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Keyword Alias)
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Whitespace 1)
                , Located { end = { col = 20, row = 1 }, start = { col = 12, row = 1 } } (Identifier { name = "Function", qualifiers = [] })
                , Located { end = { col = 21, row = 1 }, start = { col = 20, row = 1 } } (Whitespace 1)
                , Located { end = { col = 22, row = 1 }, start = { col = 21, row = 1 } } (Identifier { name = "a", qualifiers = [] })
                , Located { end = { col = 23, row = 1 }, start = { col = 22, row = 1 } } (Whitespace 1)
                , Located { end = { col = 24, row = 1 }, start = { col = 23, row = 1 } } (Sigil Assign)
                , Located { end = { col = 25, row = 1 }, start = { col = 24, row = 1 } } (Whitespace 1)
                , Located { end = { col = 29, row = 1 }, start = { col = 25, row = 1 } } (Identifier { name = "List", qualifiers = [] })
                , Located { end = { col = 30, row = 1 }, start = { col = 29, row = 1 } } (Whitespace 1)
                , Located { end = { col = 33, row = 1 }, start = { col = 30, row = 1 } } (Identifier { name = "Int", qualifiers = [] })
                , Located { end = { col = 34, row = 1 }, start = { col = 33, row = 1 } } (Whitespace 1)
                , Located { end = { col = 36, row = 1 }, start = { col = 34, row = 1 } } (Sigil ThinArrow)
                , Located { end = { col = 37, row = 1 }, start = { col = 36, row = 1 } } (Whitespace 1)
                , Located { end = { col = 41, row = 1 }, start = { col = 37, row = 1 } } (Identifier { name = "List", qualifiers = [] })
                , Located { end = { col = 42, row = 1 }, start = { col = 41, row = 1 } } (Whitespace 1)
                , Located { end = { col = 43, row = 1 }, start = { col = 42, row = 1 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 47, row = 1 }, start = { col = 43, row = 1 } } (Identifier { name = "List", qualifiers = [] })
                , Located { end = { col = 48, row = 1 }, start = { col = 47, row = 1 } } (Whitespace 1)
                , Located { end = { col = 49, row = 1 }, start = { col = 48, row = 1 } } (Identifier { name = "a", qualifiers = [] })
                , Located { end = { col = 50, row = 1 }, start = { col = 49, row = 1 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 1, row = 2 }, start = { col = 50, row = 1 } } (Newlines [] 0)
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
          , ( ValueDeclaration
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
          , ( ValueDeclaration
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
          , ( ValueDeclaration
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
          , ( ValueDeclaration
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
                        , ty = "Function"
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
                        , ty = "Function2"
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
                        , ty = "Function3"
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
                        , ty = "Function3"
                        }
                    )
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Keyword Type)
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Keyword Alias)
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Whitespace 1)
                , Located { end = { col = 20, row = 1 }, start = { col = 12, row = 1 } } (Identifier { name = "Function", qualifiers = [] })
                , Located { end = { col = 21, row = 1 }, start = { col = 20, row = 1 } } (Whitespace 1)
                , Located { end = { col = 22, row = 1 }, start = { col = 21, row = 1 } } (Sigil Assign)
                , Located { end = { col = 23, row = 1 }, start = { col = 22, row = 1 } } (Whitespace 1)
                , Located { end = { col = 24, row = 1 }, start = { col = 23, row = 1 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 25, row = 1 }, start = { col = 24, row = 1 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 26, row = 1 }, start = { col = 25, row = 1 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 27, row = 1 }, start = { col = 26, row = 1 } } (Whitespace 1)
                , Located { end = { col = 29, row = 1 }, start = { col = 27, row = 1 } } (Sigil ThinArrow)
                , Located { end = { col = 30, row = 1 }, start = { col = 29, row = 1 } } (Whitespace 1)
                , Located { end = { col = 31, row = 1 }, start = { col = 30, row = 1 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 34, row = 1 }, start = { col = 31, row = 1 } } (Identifier { name = "Int", qualifiers = [] })
                , Located { end = { col = 35, row = 1 }, start = { col = 34, row = 1 } } (Sigil Comma)
                , Located { end = { col = 36, row = 1 }, start = { col = 35, row = 1 } } (Whitespace 1)
                , Located { end = { col = 42, row = 1 }, start = { col = 36, row = 1 } } (Identifier { name = "String", qualifiers = [] })
                , Located { end = { col = 43, row = 1 }, start = { col = 42, row = 1 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 44, row = 1 }, start = { col = 43, row = 1 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 1, row = 3 }, start = { col = 44, row = 1 } }
                    (Newlines
                        [ 0
                        ]
                        0
                    )
                , Located { end = { col = 5, row = 3 }, start = { col = 1, row = 3 } } (Keyword Type)
                , Located { end = { col = 6, row = 3 }, start = { col = 5, row = 3 } } (Whitespace 1)
                , Located { end = { col = 11, row = 3 }, start = { col = 6, row = 3 } } (Keyword Alias)
                , Located { end = { col = 12, row = 3 }, start = { col = 11, row = 3 } } (Whitespace 1)
                , Located { end = { col = 21, row = 3 }, start = { col = 12, row = 3 } } (Identifier { name = "Function2", qualifiers = [] })
                , Located { end = { col = 22, row = 3 }, start = { col = 21, row = 3 } } (Whitespace 1)
                , Located { end = { col = 23, row = 3 }, start = { col = 22, row = 3 } } (Sigil Assign)
                , Located { end = { col = 24, row = 3 }, start = { col = 23, row = 3 } } (Whitespace 1)
                , Located { end = { col = 25, row = 3 }, start = { col = 24, row = 3 } } (Sigil (Bracket Curly Open))
                , Located { end = { col = 26, row = 3 }, start = { col = 25, row = 3 } } (Whitespace 1)
                , Located { end = { col = 27, row = 3 }, start = { col = 26, row = 3 } } (Identifier { name = "a", qualifiers = [] })
                , Located { end = { col = 28, row = 3 }, start = { col = 27, row = 3 } } (Sigil Colon)
                , Located { end = { col = 29, row = 3 }, start = { col = 28, row = 3 } } (Whitespace 1)
                , Located { end = { col = 30, row = 3 }, start = { col = 29, row = 3 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 31, row = 3 }, start = { col = 30, row = 3 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 32, row = 3 }, start = { col = 31, row = 3 } } (Whitespace 1)
                , Located { end = { col = 34, row = 3 }, start = { col = 32, row = 3 } } (Sigil ThinArrow)
                , Located { end = { col = 35, row = 3 }, start = { col = 34, row = 3 } } (Whitespace 1)
                , Located { end = { col = 36, row = 3 }, start = { col = 35, row = 3 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 39, row = 3 }, start = { col = 36, row = 3 } } (Identifier { name = "Int", qualifiers = [] })
                , Located { end = { col = 40, row = 3 }, start = { col = 39, row = 3 } } (Sigil Comma)
                , Located { end = { col = 41, row = 3 }, start = { col = 40, row = 3 } } (Whitespace 1)
                , Located { end = { col = 47, row = 3 }, start = { col = 41, row = 3 } } (Identifier { name = "String", qualifiers = [] })
                , Located { end = { col = 48, row = 3 }, start = { col = 47, row = 3 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 49, row = 3 }, start = { col = 48, row = 3 } } (Whitespace 1)
                , Located { end = { col = 50, row = 3 }, start = { col = 49, row = 3 } } (Sigil (Bracket Curly Close))
                , Located { end = { col = 1, row = 5 }, start = { col = 50, row = 3 } }
                    (Newlines
                        [ 0
                        ]
                        0
                    )
                , Located { end = { col = 5, row = 5 }, start = { col = 1, row = 5 } } (Keyword Type)
                , Located { end = { col = 6, row = 5 }, start = { col = 5, row = 5 } } (Whitespace 1)
                , Located { end = { col = 11, row = 5 }, start = { col = 6, row = 5 } } (Keyword Alias)
                , Located { end = { col = 12, row = 5 }, start = { col = 11, row = 5 } } (Whitespace 1)
                , Located { end = { col = 21, row = 5 }, start = { col = 12, row = 5 } } (Identifier { name = "Function3", qualifiers = [] })
                , Located { end = { col = 22, row = 5 }, start = { col = 21, row = 5 } } (Whitespace 1)
                , Located { end = { col = 23, row = 5 }, start = { col = 22, row = 5 } } (Sigil Assign)
                , Located { end = { col = 24, row = 5 }, start = { col = 23, row = 5 } } (Whitespace 1)
                , Located { end = { col = 25, row = 5 }, start = { col = 24, row = 5 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 26, row = 5 }, start = { col = 25, row = 5 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 27, row = 5 }, start = { col = 26, row = 5 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 28, row = 5 }, start = { col = 27, row = 5 } } (Whitespace 1)
                , Located { end = { col = 30, row = 5 }, start = { col = 28, row = 5 } } (Sigil ThinArrow)
                , Located { end = { col = 31, row = 5 }, start = { col = 30, row = 5 } } (Whitespace 1)
                , Located { end = { col = 32, row = 5 }, start = { col = 31, row = 5 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 35, row = 5 }, start = { col = 32, row = 5 } } (Identifier { name = "Int", qualifiers = [] })
                , Located { end = { col = 36, row = 5 }, start = { col = 35, row = 5 } } (Sigil Comma)
                , Located { end = { col = 37, row = 5 }, start = { col = 36, row = 5 } } (Whitespace 1)
                , Located { end = { col = 43, row = 5 }, start = { col = 37, row = 5 } } (Identifier { name = "String", qualifiers = [] })
                , Located { end = { col = 44, row = 5 }, start = { col = 43, row = 5 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 45, row = 5 }, start = { col = 44, row = 5 } } (Sigil Comma)
                , Located { end = { col = 46, row = 5 }, start = { col = 45, row = 5 } } (Whitespace 1)
                , Located { end = { col = 47, row = 5 }, start = { col = 46, row = 5 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 48, row = 5 }, start = { col = 47, row = 5 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 49, row = 5 }, start = { col = 48, row = 5 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 1, row = 7 }, start = { col = 49, row = 5 } }
                    (Newlines
                        [ 0
                        ]
                        0
                    )
                , Located { end = { col = 5, row = 7 }, start = { col = 1, row = 7 } } (Keyword Type)
                , Located { end = { col = 6, row = 7 }, start = { col = 5, row = 7 } } (Whitespace 1)
                , Located { end = { col = 11, row = 7 }, start = { col = 6, row = 7 } } (Keyword Alias)
                , Located { end = { col = 12, row = 7 }, start = { col = 11, row = 7 } } (Whitespace 1)
                , Located { end = { col = 21, row = 7 }, start = { col = 12, row = 7 } } (Identifier { name = "Function3", qualifiers = [] })
                , Located { end = { col = 22, row = 7 }, start = { col = 21, row = 7 } } (Whitespace 1)
                , Located { end = { col = 23, row = 7 }, start = { col = 22, row = 7 } } (Sigil Assign)
                , Located { end = { col = 24, row = 7 }, start = { col = 23, row = 7 } } (Whitespace 1)
                , Located { end = { col = 25, row = 7 }, start = { col = 24, row = 7 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 28, row = 7 }, start = { col = 25, row = 7 } } (Identifier { name = "Int", qualifiers = [] })
                , Located { end = { col = 29, row = 7 }, start = { col = 28, row = 7 } } (Sigil Comma)
                , Located { end = { col = 30, row = 7 }, start = { col = 29, row = 7 } } (Whitespace 1)
                , Located { end = { col = 31, row = 7 }, start = { col = 30, row = 7 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 32, row = 7 }, start = { col = 31, row = 7 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 33, row = 7 }, start = { col = 32, row = 7 } } (Whitespace 1)
                , Located { end = { col = 35, row = 7 }, start = { col = 33, row = 7 } } (Sigil ThinArrow)
                , Located { end = { col = 36, row = 7 }, start = { col = 35, row = 7 } } (Whitespace 1)
                , Located { end = { col = 37, row = 7 }, start = { col = 36, row = 7 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 40, row = 7 }, start = { col = 37, row = 7 } } (Identifier { name = "Int", qualifiers = [] })
                , Located { end = { col = 41, row = 7 }, start = { col = 40, row = 7 } } (Sigil Comma)
                , Located { end = { col = 42, row = 7 }, start = { col = 41, row = 7 } } (Whitespace 1)
                , Located { end = { col = 48, row = 7 }, start = { col = 42, row = 7 } } (Identifier { name = "String", qualifiers = [] })
                , Located { end = { col = 49, row = 7 }, start = { col = 48, row = 7 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 50, row = 7 }, start = { col = 49, row = 7 } } (Sigil Comma)
                , Located { end = { col = 51, row = 7 }, start = { col = 50, row = 7 } } (Whitespace 1)
                , Located { end = { col = 52, row = 7 }, start = { col = 51, row = 7 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 53, row = 7 }, start = { col = 52, row = 7 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 54, row = 7 }, start = { col = 53, row = 7 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 1, row = 8 }, start = { col = 54, row = 7 } } (Newlines [] 0)
                ]
      }
    , { name = "type-alias-function-record"
      , source = """type alias Function = { a: { b: C}, d: E } -> {}
"""
      , pretty = """
        ( ( Ok
          , ( ValueDeclaration
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
                        , ty = "Function"
                        }
                    )
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Keyword Type)
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Keyword Alias)
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Whitespace 1)
                , Located { end = { col = 20, row = 1 }, start = { col = 12, row = 1 } } (Identifier { name = "Function", qualifiers = [] })
                , Located { end = { col = 21, row = 1 }, start = { col = 20, row = 1 } } (Whitespace 1)
                , Located { end = { col = 22, row = 1 }, start = { col = 21, row = 1 } } (Sigil Assign)
                , Located { end = { col = 23, row = 1 }, start = { col = 22, row = 1 } } (Whitespace 1)
                , Located { end = { col = 24, row = 1 }, start = { col = 23, row = 1 } } (Sigil (Bracket Curly Open))
                , Located { end = { col = 25, row = 1 }, start = { col = 24, row = 1 } } (Whitespace 1)
                , Located { end = { col = 26, row = 1 }, start = { col = 25, row = 1 } } (Identifier { name = "a", qualifiers = [] })
                , Located { end = { col = 27, row = 1 }, start = { col = 26, row = 1 } } (Sigil Colon)
                , Located { end = { col = 28, row = 1 }, start = { col = 27, row = 1 } } (Whitespace 1)
                , Located { end = { col = 29, row = 1 }, start = { col = 28, row = 1 } } (Sigil (Bracket Curly Open))
                , Located { end = { col = 30, row = 1 }, start = { col = 29, row = 1 } } (Whitespace 1)
                , Located { end = { col = 31, row = 1 }, start = { col = 30, row = 1 } } (Identifier { name = "b", qualifiers = [] })
                , Located { end = { col = 32, row = 1 }, start = { col = 31, row = 1 } } (Sigil Colon)
                , Located { end = { col = 33, row = 1 }, start = { col = 32, row = 1 } } (Whitespace 1)
                , Located { end = { col = 34, row = 1 }, start = { col = 33, row = 1 } } (Identifier { name = "C", qualifiers = [] })
                , Located { end = { col = 35, row = 1 }, start = { col = 34, row = 1 } } (Sigil (Bracket Curly Close))
                , Located { end = { col = 36, row = 1 }, start = { col = 35, row = 1 } } (Sigil Comma)
                , Located { end = { col = 37, row = 1 }, start = { col = 36, row = 1 } } (Whitespace 1)
                , Located { end = { col = 38, row = 1 }, start = { col = 37, row = 1 } } (Identifier { name = "d", qualifiers = [] })
                , Located { end = { col = 39, row = 1 }, start = { col = 38, row = 1 } } (Sigil Colon)
                , Located { end = { col = 40, row = 1 }, start = { col = 39, row = 1 } } (Whitespace 1)
                , Located { end = { col = 41, row = 1 }, start = { col = 40, row = 1 } } (Identifier { name = "E", qualifiers = [] })
                , Located { end = { col = 42, row = 1 }, start = { col = 41, row = 1 } } (Whitespace 1)
                , Located { end = { col = 43, row = 1 }, start = { col = 42, row = 1 } } (Sigil (Bracket Curly Close))
                , Located { end = { col = 44, row = 1 }, start = { col = 43, row = 1 } } (Whitespace 1)
                , Located { end = { col = 46, row = 1 }, start = { col = 44, row = 1 } } (Sigil ThinArrow)
                , Located { end = { col = 47, row = 1 }, start = { col = 46, row = 1 } } (Whitespace 1)
                , Located { end = { col = 48, row = 1 }, start = { col = 47, row = 1 } } (Sigil (Bracket Curly Open))
                , Located { end = { col = 49, row = 1 }, start = { col = 48, row = 1 } } (Sigil (Bracket Curly Close))
                , Located { end = { col = 1, row = 2 }, start = { col = 49, row = 1 } } (Newlines [] 0)
                ]
      }
    , { name = "type-alias-function-tuple"
      , source = """type alias Function = () -> (Int, String)
"""
      , pretty = """
        ( ( Ok
          , ( ValueDeclaration
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
                        , ty = "Function"
                        }
                    )
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Keyword Type)
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Keyword Alias)
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Whitespace 1)
                , Located { end = { col = 20, row = 1 }, start = { col = 12, row = 1 } } (Identifier { name = "Function", qualifiers = [] })
                , Located { end = { col = 21, row = 1 }, start = { col = 20, row = 1 } } (Whitespace 1)
                , Located { end = { col = 22, row = 1 }, start = { col = 21, row = 1 } } (Sigil Assign)
                , Located { end = { col = 23, row = 1 }, start = { col = 22, row = 1 } } (Whitespace 1)
                , Located { end = { col = 24, row = 1 }, start = { col = 23, row = 1 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 25, row = 1 }, start = { col = 24, row = 1 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 26, row = 1 }, start = { col = 25, row = 1 } } (Whitespace 1)
                , Located { end = { col = 28, row = 1 }, start = { col = 26, row = 1 } } (Sigil ThinArrow)
                , Located { end = { col = 29, row = 1 }, start = { col = 28, row = 1 } } (Whitespace 1)
                , Located { end = { col = 30, row = 1 }, start = { col = 29, row = 1 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 33, row = 1 }, start = { col = 30, row = 1 } } (Identifier { name = "Int", qualifiers = [] })
                , Located { end = { col = 34, row = 1 }, start = { col = 33, row = 1 } } (Sigil Comma)
                , Located { end = { col = 35, row = 1 }, start = { col = 34, row = 1 } } (Whitespace 1)
                , Located { end = { col = 41, row = 1 }, start = { col = 35, row = 1 } } (Identifier { name = "String", qualifiers = [] })
                , Located { end = { col = 42, row = 1 }, start = { col = 41, row = 1 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 1, row = 2 }, start = { col = 42, row = 1 } } (Newlines [] 0)
                ]
      }
    , { name = "type-alias-funky-indentation"
      , source = """type alias
    Model = List Int
"""
      , pretty = """
        ( ( Ok
          , ( ValueDeclaration
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
                        , ty = "Model"
                        }
                    )
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Keyword Type)
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Keyword Alias)
                , Located { end = { col = 5, row = 2 }, start = { col = 11, row = 1 } } (Newlines [] 4)
                , Located { end = { col = 10, row = 2 }, start = { col = 5, row = 2 } } (Identifier { name = "Model", qualifiers = [] })
                , Located { end = { col = 11, row = 2 }, start = { col = 10, row = 2 } } (Whitespace 1)
                , Located { end = { col = 12, row = 2 }, start = { col = 11, row = 2 } } (Sigil Assign)
                , Located { end = { col = 13, row = 2 }, start = { col = 12, row = 2 } } (Whitespace 1)
                , Located { end = { col = 17, row = 2 }, start = { col = 13, row = 2 } } (Identifier { name = "List", qualifiers = [] })
                , Located { end = { col = 18, row = 2 }, start = { col = 17, row = 2 } } (Whitespace 1)
                , Located { end = { col = 21, row = 2 }, start = { col = 18, row = 2 } } (Identifier { name = "Int", qualifiers = [] })
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
          , ( ValueDeclaration
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
                        , ty = "Model"
                        }
                    )
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Keyword Type)
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Keyword Alias)
                , Located { end = { col = 5, row = 2 }, start = { col = 11, row = 1 } } (Newlines [] 4)
                , Located { end = { col = 10, row = 2 }, start = { col = 5, row = 2 } } (Identifier { name = "Model", qualifiers = [] })
                , Located { end = { col = 11, row = 2 }, start = { col = 10, row = 2 } } (Whitespace 1)
                , Located { end = { col = 12, row = 2 }, start = { col = 11, row = 2 } } (Sigil Assign)
                , Located { end = { col = 2, row = 3 }, start = { col = 12, row = 2 } } (Newlines [] 1)
                , Located { end = { col = 6, row = 3 }, start = { col = 2, row = 3 } } (Identifier { name = "List", qualifiers = [] })
                , Located { end = { col = 7, row = 3 }, start = { col = 6, row = 3 } } (Whitespace 1)
                , Located { end = { col = 10, row = 3 }, start = { col = 7, row = 3 } } (Identifier { name = "Int", qualifiers = [] })
                , Located { end = { col = 1, row = 4 }, start = { col = 10, row = 3 } } (Newlines [] 0)
                ]
      }
    , { name = "type-alias-record-3-entries"
      , source = """type alias Ty = { a: A, b: B, c: C }
"""
      , pretty = """
        ( ( Ok
          , ( ValueDeclaration
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
                        , ty = "Ty"
                        }
                    )
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Keyword Type)
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Keyword Alias)
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Whitespace 1)
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Identifier { name = "Ty", qualifiers = [] })
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Whitespace 1)
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Sigil Assign)
                , Located { end = { col = 17, row = 1 }, start = { col = 16, row = 1 } } (Whitespace 1)
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Sigil (Bracket Curly Open))
                , Located { end = { col = 19, row = 1 }, start = { col = 18, row = 1 } } (Whitespace 1)
                , Located { end = { col = 20, row = 1 }, start = { col = 19, row = 1 } } (Identifier { name = "a", qualifiers = [] })
                , Located { end = { col = 21, row = 1 }, start = { col = 20, row = 1 } } (Sigil Colon)
                , Located { end = { col = 22, row = 1 }, start = { col = 21, row = 1 } } (Whitespace 1)
                , Located { end = { col = 23, row = 1 }, start = { col = 22, row = 1 } } (Identifier { name = "A", qualifiers = [] })
                , Located { end = { col = 24, row = 1 }, start = { col = 23, row = 1 } } (Sigil Comma)
                , Located { end = { col = 25, row = 1 }, start = { col = 24, row = 1 } } (Whitespace 1)
                , Located { end = { col = 26, row = 1 }, start = { col = 25, row = 1 } } (Identifier { name = "b", qualifiers = [] })
                , Located { end = { col = 27, row = 1 }, start = { col = 26, row = 1 } } (Sigil Colon)
                , Located { end = { col = 28, row = 1 }, start = { col = 27, row = 1 } } (Whitespace 1)
                , Located { end = { col = 29, row = 1 }, start = { col = 28, row = 1 } } (Identifier { name = "B", qualifiers = [] })
                , Located { end = { col = 30, row = 1 }, start = { col = 29, row = 1 } } (Sigil Comma)
                , Located { end = { col = 31, row = 1 }, start = { col = 30, row = 1 } } (Whitespace 1)
                , Located { end = { col = 32, row = 1 }, start = { col = 31, row = 1 } } (Identifier { name = "c", qualifiers = [] })
                , Located { end = { col = 33, row = 1 }, start = { col = 32, row = 1 } } (Sigil Colon)
                , Located { end = { col = 34, row = 1 }, start = { col = 33, row = 1 } } (Whitespace 1)
                , Located { end = { col = 35, row = 1 }, start = { col = 34, row = 1 } } (Identifier { name = "C", qualifiers = [] })
                , Located { end = { col = 36, row = 1 }, start = { col = 35, row = 1 } } (Whitespace 1)
                , Located { end = { col = 37, row = 1 }, start = { col = 36, row = 1 } } (Sigil (Bracket Curly Close))
                , Located { end = { col = 1, row = 2 }, start = { col = 37, row = 1 } } (Newlines [] 0)
                ]
      }
    , { name = "type-alias-record-empty"
      , source = """type alias Ty = {}
"""
      , pretty = """
        ( ( Ok
          , ( ValueDeclaration
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
                [ Ok (TypeAlias { expr = Record (Dict.fromList []), genericArgs = [], ty = "Ty" })
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Keyword Type)
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Keyword Alias)
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Whitespace 1)
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Identifier { name = "Ty", qualifiers = [] })
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Whitespace 1)
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Sigil Assign)
                , Located { end = { col = 17, row = 1 }, start = { col = 16, row = 1 } } (Whitespace 1)
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Sigil (Bracket Curly Open))
                , Located { end = { col = 19, row = 1 }, start = { col = 18, row = 1 } } (Sigil (Bracket Curly Close))
                , Located { end = { col = 1, row = 2 }, start = { col = 19, row = 1 } } (Newlines [] 0)
                ]
      }
    , { name = "type-alias-record-empty-multiline"
      , source = """type alias Ty = {


    }
"""
      , pretty = """
        ( ( Ok
          , ( ValueDeclaration
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
                [ Ok (TypeAlias { expr = Record (Dict.fromList []), genericArgs = [], ty = "Ty" })
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Keyword Type)
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Keyword Alias)
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Whitespace 1)
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Identifier { name = "Ty", qualifiers = [] })
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
      }
    , { name = "type-alias-record-in-bracket"
      , source = """type alias Ty = ({ hi: Int })
"""
      , pretty = """
        ( ( Ok
          , ( ValueDeclaration
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
                        , ty = "Ty"
                        }
                    )
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Keyword Type)
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Keyword Alias)
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Whitespace 1)
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Identifier { name = "Ty", qualifiers = [] })
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Whitespace 1)
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Sigil Assign)
                , Located { end = { col = 17, row = 1 }, start = { col = 16, row = 1 } } (Whitespace 1)
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 19, row = 1 }, start = { col = 18, row = 1 } } (Sigil (Bracket Curly Open))
                , Located { end = { col = 20, row = 1 }, start = { col = 19, row = 1 } } (Whitespace 1)
                , Located { end = { col = 22, row = 1 }, start = { col = 20, row = 1 } } (Identifier { name = "hi", qualifiers = [] })
                , Located { end = { col = 23, row = 1 }, start = { col = 22, row = 1 } } (Sigil Colon)
                , Located { end = { col = 24, row = 1 }, start = { col = 23, row = 1 } } (Whitespace 1)
                , Located { end = { col = 27, row = 1 }, start = { col = 24, row = 1 } } (Identifier { name = "Int", qualifiers = [] })
                , Located { end = { col = 28, row = 1 }, start = { col = 27, row = 1 } } (Whitespace 1)
                , Located { end = { col = 29, row = 1 }, start = { col = 28, row = 1 } } (Sigil (Bracket Curly Close))
                , Located { end = { col = 30, row = 1 }, start = { col = 29, row = 1 } } (Sigil (Bracket Round Close))
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
          , ( ValueDeclaration
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
                        , ty = "Ty"
                        }
                    )
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Keyword Type)
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Keyword Alias)
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Whitespace 1)
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Identifier { name = "Ty", qualifiers = [] })
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Whitespace 1)
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Sigil Assign)
                , Located { end = { col = 5, row = 2 }, start = { col = 16, row = 1 } } (Newlines [] 4)
                , Located { end = { col = 6, row = 2 }, start = { col = 5, row = 2 } } (Sigil (Bracket Curly Open))
                , Located { end = { col = 7, row = 2 }, start = { col = 6, row = 2 } } (Whitespace 1)
                , Located { end = { col = 9, row = 2 }, start = { col = 7, row = 2 } } (Identifier { name = "hi", qualifiers = [] })
                , Located { end = { col = 10, row = 2 }, start = { col = 9, row = 2 } } (Sigil Colon)
                , Located { end = { col = 12, row = 2 }, start = { col = 10, row = 2 } } (Whitespace 2)
                , Located { end = { col = 13, row = 2 }, start = { col = 12, row = 2 } } (Sigil (Bracket Curly Open))
                , Located { end = { col = 14, row = 2 }, start = { col = 13, row = 2 } } (Whitespace 1)
                , Located { end = { col = 15, row = 2 }, start = { col = 14, row = 2 } } (Identifier { name = "a", qualifiers = [] })
                , Located { end = { col = 16, row = 2 }, start = { col = 15, row = 2 } } (Sigil Colon)
                , Located { end = { col = 17, row = 2 }, start = { col = 16, row = 2 } } (Whitespace 1)
                , Located { end = { col = 20, row = 2 }, start = { col = 17, row = 2 } } (Identifier { name = "Int", qualifiers = [] })
                , Located { end = { col = 21, row = 2 }, start = { col = 20, row = 2 } } (Sigil Comma)
                , Located { end = { col = 22, row = 2 }, start = { col = 21, row = 2 } } (Whitespace 1)
                , Located { end = { col = 23, row = 2 }, start = { col = 22, row = 2 } } (Identifier { name = "b", qualifiers = [] })
                , Located { end = { col = 24, row = 2 }, start = { col = 23, row = 2 } } (Sigil Colon)
                , Located { end = { col = 25, row = 2 }, start = { col = 24, row = 2 } } (Whitespace 1)
                , Located { end = { col = 29, row = 2 }, start = { col = 25, row = 2 } } (Identifier { name = "List", qualifiers = [] })
                , Located { end = { col = 30, row = 2 }, start = { col = 29, row = 2 } } (Whitespace 1)
                , Located { end = { col = 36, row = 2 }, start = { col = 30, row = 2 } } (Identifier { name = "String", qualifiers = [] })
                , Located { end = { col = 37, row = 2 }, start = { col = 36, row = 2 } } (Whitespace 1)
                , Located { end = { col = 38, row = 2 }, start = { col = 37, row = 2 } } (Sigil (Bracket Curly Close))
                , Located { end = { col = 5, row = 3 }, start = { col = 38, row = 2 } } (Newlines [] 4)
                , Located { end = { col = 6, row = 3 }, start = { col = 5, row = 3 } } (Sigil Comma)
                , Located { end = { col = 7, row = 3 }, start = { col = 6, row = 3 } } (Whitespace 1)
                , Located { end = { col = 9, row = 3 }, start = { col = 7, row = 3 } } (Identifier { name = "ih", qualifiers = [] })
                , Located { end = { col = 10, row = 3 }, start = { col = 9, row = 3 } } (Sigil Colon)
                , Located { end = { col = 11, row = 3 }, start = { col = 10, row = 3 } } (Whitespace 1)
                , Located { end = { col = 21, row = 3 }, start = { col = 11, row = 3 } } (Identifier { name = "CustomType", qualifiers = [] })
                , Located { end = { col = 22, row = 3 }, start = { col = 21, row = 3 } } (Whitespace 1)
                , Located { end = { col = 23, row = 3 }, start = { col = 22, row = 3 } } (Identifier { name = "A", qualifiers = [] })
                , Located { end = { col = 24, row = 3 }, start = { col = 23, row = 3 } } (Whitespace 1)
                , Located { end = { col = 25, row = 3 }, start = { col = 24, row = 3 } } (Identifier { name = "B", qualifiers = [] })
                , Located { end = { col = 26, row = 3 }, start = { col = 25, row = 3 } } (Whitespace 1)
                , Located { end = { col = 27, row = 3 }, start = { col = 26, row = 3 } } (Identifier { name = "C", qualifiers = [] })
                , Located { end = { col = 28, row = 3 }, start = { col = 27, row = 3 } } (Whitespace 1)
                , Located { end = { col = 29, row = 3 }, start = { col = 28, row = 3 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 30, row = 3 }, start = { col = 29, row = 3 } } (Identifier { name = "D", qualifiers = [] })
                , Located { end = { col = 31, row = 3 }, start = { col = 30, row = 3 } } (Whitespace 1)
                , Located { end = { col = 32, row = 3 }, start = { col = 31, row = 3 } } (Identifier { name = "E", qualifiers = [] })
                , Located { end = { col = 33, row = 3 }, start = { col = 32, row = 3 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 5, row = 4 }, start = { col = 33, row = 3 } } (Newlines [] 4)
                , Located { end = { col = 6, row = 4 }, start = { col = 5, row = 4 } } (Sigil (Bracket Curly Close))
                , Located { end = { col = 1, row = 5 }, start = { col = 6, row = 4 } } (Newlines [] 0)
                ]
      }
    , { name = "type-alias-record-simple"
      , source = """type alias Ty = { hi: Int }
"""
      , pretty = """
        ( ( Ok
          , ( ValueDeclaration
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
                        , ty = "Ty"
                        }
                    )
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Keyword Type)
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Keyword Alias)
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Whitespace 1)
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Identifier { name = "Ty", qualifiers = [] })
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Whitespace 1)
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Sigil Assign)
                , Located { end = { col = 17, row = 1 }, start = { col = 16, row = 1 } } (Whitespace 1)
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Sigil (Bracket Curly Open))
                , Located { end = { col = 19, row = 1 }, start = { col = 18, row = 1 } } (Whitespace 1)
                , Located { end = { col = 21, row = 1 }, start = { col = 19, row = 1 } } (Identifier { name = "hi", qualifiers = [] })
                , Located { end = { col = 22, row = 1 }, start = { col = 21, row = 1 } } (Sigil Colon)
                , Located { end = { col = 23, row = 1 }, start = { col = 22, row = 1 } } (Whitespace 1)
                , Located { end = { col = 26, row = 1 }, start = { col = 23, row = 1 } } (Identifier { name = "Int", qualifiers = [] })
                , Located { end = { col = 27, row = 1 }, start = { col = 26, row = 1 } } (Whitespace 1)
                , Located { end = { col = 28, row = 1 }, start = { col = 27, row = 1 } } (Sigil (Bracket Curly Close))
                , Located { end = { col = 1, row = 2 }, start = { col = 28, row = 1 } } (Newlines [] 0)
                ]
      }
    , { name = "type-alias-record-two-entries"
      , source = """type alias Ty = { hi: (), buy: String }
"""
      , pretty = """
        ( ( Ok
          , ( ValueDeclaration
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
                        , ty = "Ty"
                        }
                    )
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Keyword Type)
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Keyword Alias)
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Whitespace 1)
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Identifier { name = "Ty", qualifiers = [] })
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Whitespace 1)
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Sigil Assign)
                , Located { end = { col = 17, row = 1 }, start = { col = 16, row = 1 } } (Whitespace 1)
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Sigil (Bracket Curly Open))
                , Located { end = { col = 19, row = 1 }, start = { col = 18, row = 1 } } (Whitespace 1)
                , Located { end = { col = 21, row = 1 }, start = { col = 19, row = 1 } } (Identifier { name = "hi", qualifiers = [] })
                , Located { end = { col = 22, row = 1 }, start = { col = 21, row = 1 } } (Sigil Colon)
                , Located { end = { col = 23, row = 1 }, start = { col = 22, row = 1 } } (Whitespace 1)
                , Located { end = { col = 24, row = 1 }, start = { col = 23, row = 1 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 25, row = 1 }, start = { col = 24, row = 1 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 26, row = 1 }, start = { col = 25, row = 1 } } (Sigil Comma)
                , Located { end = { col = 27, row = 1 }, start = { col = 26, row = 1 } } (Whitespace 1)
                , Located { end = { col = 30, row = 1 }, start = { col = 27, row = 1 } } (Identifier { name = "buy", qualifiers = [] })
                , Located { end = { col = 31, row = 1 }, start = { col = 30, row = 1 } } (Sigil Colon)
                , Located { end = { col = 32, row = 1 }, start = { col = 31, row = 1 } } (Whitespace 1)
                , Located { end = { col = 38, row = 1 }, start = { col = 32, row = 1 } } (Identifier { name = "String", qualifiers = [] })
                , Located { end = { col = 39, row = 1 }, start = { col = 38, row = 1 } } (Whitespace 1)
                , Located { end = { col = 40, row = 1 }, start = { col = 39, row = 1 } } (Sigil (Bracket Curly Close))
                , Located { end = { col = 1, row = 2 }, start = { col = 40, row = 1 } } (Newlines [] 0)
                ]
      }
    , { name = "type-alias-unit"
      , source = """type alias Hi = ()
"""
      , pretty = """
        ( ( Ok
          , ( ValueDeclaration
            , ( ty, Hi )
            , ( genericArgs, () )
            , ( expr, Unit )
            )
          ) )
"""
      , contextualized =
            Just
                [ Ok (TypeAlias { expr = Unit, genericArgs = [], ty = "Hi" })
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Keyword Type)
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Keyword Alias)
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Whitespace 1)
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Identifier { name = "Hi", qualifiers = [] })
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Whitespace 1)
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Sigil Assign)
                , Located { end = { col = 17, row = 1 }, start = { col = 16, row = 1 } } (Whitespace 1)
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 19, row = 1 }, start = { col = 18, row = 1 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 1, row = 2 }, start = { col = 19, row = 1 } } (Newlines [] 0)
                ]
      }
    , { name = "type-alias-with-bracket"
      , source = """type alias Hi = (Int)
"""
      , pretty = """
        ( ( Ok
          , ( ValueDeclaration
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
                        , ty = "Hi"
                        }
                    )
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Keyword Type)
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Keyword Alias)
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Whitespace 1)
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Identifier { name = "Hi", qualifiers = [] })
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Whitespace 1)
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Sigil Assign)
                , Located { end = { col = 17, row = 1 }, start = { col = 16, row = 1 } } (Whitespace 1)
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 21, row = 1 }, start = { col = 18, row = 1 } } (Identifier { name = "Int", qualifiers = [] })
                , Located { end = { col = 22, row = 1 }, start = { col = 21, row = 1 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 1, row = 2 }, start = { col = 22, row = 1 } } (Newlines [] 0)
                ]
      }
    , { name = "type-alias-with-bracket-2"
      , source = """type alias Hi = (List Int)
"""
      , pretty = """
        ( ( Ok
          , ( ValueDeclaration
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
                        , ty = "Hi"
                        }
                    )
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Keyword Type)
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Keyword Alias)
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Whitespace 1)
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Identifier { name = "Hi", qualifiers = [] })
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Whitespace 1)
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Sigil Assign)
                , Located { end = { col = 17, row = 1 }, start = { col = 16, row = 1 } } (Whitespace 1)
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 22, row = 1 }, start = { col = 18, row = 1 } } (Identifier { name = "List", qualifiers = [] })
                , Located { end = { col = 23, row = 1 }, start = { col = 22, row = 1 } } (Whitespace 1)
                , Located { end = { col = 26, row = 1 }, start = { col = 23, row = 1 } } (Identifier { name = "Int", qualifiers = [] })
                , Located { end = { col = 27, row = 1 }, start = { col = 26, row = 1 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 1, row = 2 }, start = { col = 27, row = 1 } } (Newlines [] 0)
                ]
      }
    , { name = "type-alias-with-pair"
      , source = """type alias Hi = (Int, List String)
type alias Hi = (Int)
"""
      , pretty = """
        ( ( Ok
          , ( ValueDeclaration
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
          , ( ValueDeclaration
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
                        , ty = "Hi"
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
                        , ty = "Hi"
                        }
                    )
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Keyword Type)
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Keyword Alias)
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Whitespace 1)
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Identifier { name = "Hi", qualifiers = [] })
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Whitespace 1)
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Sigil Assign)
                , Located { end = { col = 17, row = 1 }, start = { col = 16, row = 1 } } (Whitespace 1)
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 21, row = 1 }, start = { col = 18, row = 1 } } (Identifier { name = "Int", qualifiers = [] })
                , Located { end = { col = 22, row = 1 }, start = { col = 21, row = 1 } } (Sigil Comma)
                , Located { end = { col = 23, row = 1 }, start = { col = 22, row = 1 } } (Whitespace 1)
                , Located { end = { col = 27, row = 1 }, start = { col = 23, row = 1 } } (Identifier { name = "List", qualifiers = [] })
                , Located { end = { col = 28, row = 1 }, start = { col = 27, row = 1 } } (Whitespace 1)
                , Located { end = { col = 34, row = 1 }, start = { col = 28, row = 1 } } (Identifier { name = "String", qualifiers = [] })
                , Located { end = { col = 35, row = 1 }, start = { col = 34, row = 1 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 1, row = 2 }, start = { col = 35, row = 1 } } (Newlines [] 0)
                , Located { end = { col = 5, row = 2 }, start = { col = 1, row = 2 } } (Keyword Type)
                , Located { end = { col = 6, row = 2 }, start = { col = 5, row = 2 } } (Whitespace 1)
                , Located { end = { col = 11, row = 2 }, start = { col = 6, row = 2 } } (Keyword Alias)
                , Located { end = { col = 12, row = 2 }, start = { col = 11, row = 2 } } (Whitespace 1)
                , Located { end = { col = 14, row = 2 }, start = { col = 12, row = 2 } } (Identifier { name = "Hi", qualifiers = [] })
                , Located { end = { col = 15, row = 2 }, start = { col = 14, row = 2 } } (Whitespace 1)
                , Located { end = { col = 16, row = 2 }, start = { col = 15, row = 2 } } (Sigil Assign)
                , Located { end = { col = 17, row = 2 }, start = { col = 16, row = 2 } } (Whitespace 1)
                , Located { end = { col = 18, row = 2 }, start = { col = 17, row = 2 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 21, row = 2 }, start = { col = 18, row = 2 } } (Identifier { name = "Int", qualifiers = [] })
                , Located { end = { col = 22, row = 2 }, start = { col = 21, row = 2 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 1, row = 3 }, start = { col = 22, row = 2 } } (Newlines [] 0)
                ]
      }
    , { name = "type-alias-with-tripple"
      , source = """type alias Hi = (Int, Two, Three)
type alias Hi = ((), (), ())
"""
      , pretty = """
        ( ( Ok
          , ( ValueDeclaration
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
          , ( ValueDeclaration
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
                        , ty = "Hi"
                        }
                    )
                , Ok (TypeAlias { expr = Tuple3 Unit Unit Unit, genericArgs = [], ty = "Hi" })
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Keyword Type)
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Keyword Alias)
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Whitespace 1)
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Identifier { name = "Hi", qualifiers = [] })
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Whitespace 1)
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Sigil Assign)
                , Located { end = { col = 17, row = 1 }, start = { col = 16, row = 1 } } (Whitespace 1)
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 21, row = 1 }, start = { col = 18, row = 1 } } (Identifier { name = "Int", qualifiers = [] })
                , Located { end = { col = 22, row = 1 }, start = { col = 21, row = 1 } } (Sigil Comma)
                , Located { end = { col = 23, row = 1 }, start = { col = 22, row = 1 } } (Whitespace 1)
                , Located { end = { col = 26, row = 1 }, start = { col = 23, row = 1 } } (Identifier { name = "Two", qualifiers = [] })
                , Located { end = { col = 27, row = 1 }, start = { col = 26, row = 1 } } (Sigil Comma)
                , Located { end = { col = 28, row = 1 }, start = { col = 27, row = 1 } } (Whitespace 1)
                , Located { end = { col = 33, row = 1 }, start = { col = 28, row = 1 } } (Identifier { name = "Three", qualifiers = [] })
                , Located { end = { col = 34, row = 1 }, start = { col = 33, row = 1 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 1, row = 2 }, start = { col = 34, row = 1 } } (Newlines [] 0)
                , Located { end = { col = 5, row = 2 }, start = { col = 1, row = 2 } } (Keyword Type)
                , Located { end = { col = 6, row = 2 }, start = { col = 5, row = 2 } } (Whitespace 1)
                , Located { end = { col = 11, row = 2 }, start = { col = 6, row = 2 } } (Keyword Alias)
                , Located { end = { col = 12, row = 2 }, start = { col = 11, row = 2 } } (Whitespace 1)
                , Located { end = { col = 14, row = 2 }, start = { col = 12, row = 2 } } (Identifier { name = "Hi", qualifiers = [] })
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
      }
    , { name = "type-alias-with-tripple-in-record"
      , source = """type alias Hi =
    { a: (Int, Int, Int)
    , b: ({ good_bye: () })
    }
"""
      , pretty = """
        ( ( Ok
          , ( ValueDeclaration
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
                        , ty = "Hi"
                        }
                    )
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Keyword Type)
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Keyword Alias)
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Whitespace 1)
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Identifier { name = "Hi", qualifiers = [] })
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Whitespace 1)
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Sigil Assign)
                , Located { end = { col = 5, row = 2 }, start = { col = 16, row = 1 } } (Newlines [] 4)
                , Located { end = { col = 6, row = 2 }, start = { col = 5, row = 2 } } (Sigil (Bracket Curly Open))
                , Located { end = { col = 7, row = 2 }, start = { col = 6, row = 2 } } (Whitespace 1)
                , Located { end = { col = 8, row = 2 }, start = { col = 7, row = 2 } } (Identifier { name = "a", qualifiers = [] })
                , Located { end = { col = 9, row = 2 }, start = { col = 8, row = 2 } } (Sigil Colon)
                , Located { end = { col = 10, row = 2 }, start = { col = 9, row = 2 } } (Whitespace 1)
                , Located { end = { col = 11, row = 2 }, start = { col = 10, row = 2 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 14, row = 2 }, start = { col = 11, row = 2 } } (Identifier { name = "Int", qualifiers = [] })
                , Located { end = { col = 15, row = 2 }, start = { col = 14, row = 2 } } (Sigil Comma)
                , Located { end = { col = 16, row = 2 }, start = { col = 15, row = 2 } } (Whitespace 1)
                , Located { end = { col = 19, row = 2 }, start = { col = 16, row = 2 } } (Identifier { name = "Int", qualifiers = [] })
                , Located { end = { col = 20, row = 2 }, start = { col = 19, row = 2 } } (Sigil Comma)
                , Located { end = { col = 21, row = 2 }, start = { col = 20, row = 2 } } (Whitespace 1)
                , Located { end = { col = 24, row = 2 }, start = { col = 21, row = 2 } } (Identifier { name = "Int", qualifiers = [] })
                , Located { end = { col = 25, row = 2 }, start = { col = 24, row = 2 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 5, row = 3 }, start = { col = 25, row = 2 } } (Newlines [] 4)
                , Located { end = { col = 6, row = 3 }, start = { col = 5, row = 3 } } (Sigil Comma)
                , Located { end = { col = 7, row = 3 }, start = { col = 6, row = 3 } } (Whitespace 1)
                , Located { end = { col = 8, row = 3 }, start = { col = 7, row = 3 } } (Identifier { name = "b", qualifiers = [] })
                , Located { end = { col = 9, row = 3 }, start = { col = 8, row = 3 } } (Sigil Colon)
                , Located { end = { col = 10, row = 3 }, start = { col = 9, row = 3 } } (Whitespace 1)
                , Located { end = { col = 11, row = 3 }, start = { col = 10, row = 3 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 12, row = 3 }, start = { col = 11, row = 3 } } (Sigil (Bracket Curly Open))
                , Located { end = { col = 13, row = 3 }, start = { col = 12, row = 3 } } (Whitespace 1)
                , Located { end = { col = 21, row = 3 }, start = { col = 13, row = 3 } } (Identifier { name = "good_bye", qualifiers = [] })
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

(shdf).hellp

(sjhsf) .hello

sfhsdf(.hello)

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
        )
"""
      , contextualized =
            Just
                [ Err
                    { error = Error_InvalidToken Expecting_Block
                    , item = Just (Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (IdentifierWithTrailingDot { name = "Foo", qualifiers = [] }))
                    , state = State_BlockStart
                    }
                , Err
                    { error =
                        Error_BlockStartsWithQualifiedName
                            { name = "Bar"
                            , qualifiers =
                                [ "Foo"
                                ]
                            }
                    , item =
                        Just
                            (Located { end = { col = 8, row = 3 }, start = { col = 1, row = 3 } }
                                (Identifier
                                    { name = "Bar"
                                    , qualifiers =
                                        [ "Foo"
                                        ]
                                    }
                                )
                            )
                    , state = State_BlockStart
                    }
                , Err
                    { error =
                        Error_BlockStartsWithQualifiedName
                            { name = "baz"
                            , qualifiers =
                                [ "Foo"
                                , "Bar"
                                ]
                            }
                    , item =
                        Just
                            (Located { end = { col = 12, row = 5 }, start = { col = 1, row = 5 } }
                                (Identifier
                                    { name = "baz"
                                    , qualifiers =
                                        [ "Foo"
                                        , "Bar"
                                        ]
                                    }
                                )
                            )
                    , state = State_BlockStart
                    }
                , Err
                    { error = Error_InvalidToken Expecting_Block
                    , item =
                        Just
                            (Located { end = { col = 11, row = 7 }, start = { col = 1, row = 7 } }
                                (IdentifierWithTrailingDot
                                    { name = "Bing"
                                    , qualifiers =
                                        [ "Boor"
                                        ]
                                    }
                                )
                            )
                    , state = State_BlockStart
                    }
                , Err
                    { error = Error_InvalidToken Expecting_Block
                    , item = Just (Located { end = { col = 5, row = 9 }, start = { col = 1, row = 9 } } (IdentifierWithTrailingDot { name = "Bor", qualifiers = [] }))
                    , state = State_BlockStart
                    }
                , Err
                    { error = Error_InvalidToken Expecting_Block
                    , item = Just (Located { end = { col = 2, row = 11 }, start = { col = 1, row = 11 } } (Sigil SingleDot))
                    , state = State_BlockStart
                    }
                , Err
                    { error = Error_InvalidToken (Expecting_Sigil Assign)
                    , item = Just (Located { end = { col = 8, row = 13 }, start = { col = 7, row = 13 } } (Sigil SingleDot))
                    , state = State_BlockValueDeclaration (BlockValueDeclaration_Named { args = Stack [], name = Located { end = { col = 6, row = 13 }, start = { col = 1, row = 13 } } "sfsdf" })
                    }
                , Err
                    { error =
                        Error_BlockStartsWithQualifiedName
                            { name = "sdgsghj"
                            , qualifiers =
                                [ "asfasf"
                                ]
                            }
                    , item =
                        Just
                            (Located { end = { col = 15, row = 15 }, start = { col = 1, row = 15 } }
                                (Identifier
                                    { name = "sdgsghj"
                                    , qualifiers =
                                        [ "asfasf"
                                        ]
                                    }
                                )
                            )
                    , state = State_BlockStart
                    }
                , Err
                    { error = Error_InvalidToken Expecting_Block
                    , item = Just (Located { end = { col = 2, row = 17 }, start = { col = 1, row = 17 } } (Sigil (Bracket Round Open)))
                    , state = State_BlockStart
                    }
                , Err
                    { error = Error_InvalidToken Expecting_Block
                    , item = Just (Located { end = { col = 2, row = 19 }, start = { col = 1, row = 19 } } (Sigil (Bracket Round Open)))
                    , state = State_BlockStart
                    }
                , Err
                    { error = Error_InvalidToken (Expecting_Sigil Assign)
                    , item = Just (Located { end = { col = 8, row = 21 }, start = { col = 7, row = 21 } } (Sigil (Bracket Round Open)))
                    , state = State_BlockValueDeclaration (BlockValueDeclaration_Named { args = Stack [], name = Located { end = { col = 7, row = 21 }, start = { col = 1, row = 21 } } "sfhsdf" })
                    }
                , Err
                    { error = Error_MisplacedKeyword Case
                    , item = Just (Located { end = { col = 5, row = 23 }, start = { col = 1, row = 23 } } (Keyword Case))
                    , state = State_BlockStart
                    }
                , Err
                    { error =
                        Error_BlockStartsWithQualifiedName
                            { name = "case"
                            , qualifiers =
                                [ "Hi"
                                ]
                            }
                    , item =
                        Just
                            (Located { end = { col = 8, row = 25 }, start = { col = 1, row = 25 } }
                                (Identifier
                                    { name = "case"
                                    , qualifiers =
                                        [ "Hi"
                                        ]
                                    }
                                )
                            )
                    , state = State_BlockStart
                    }
                , Err
                    { error = Error_MisplacedKeyword Case
                    , item = Just (Located { end = { col = 5, row = 27 }, start = { col = 1, row = 27 } } (Keyword Case))
                    , state = State_BlockStart
                    }
                , Err
                    { error = Error_InvalidToken Expecting_Block
                    , item = Just (Located { end = { col = 4, row = 29 }, start = { col = 1, row = 29 } } (IdentifierWithTrailingDot { name = "Hi", qualifiers = [] }))
                    , state = State_BlockStart
                    }
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (IdentifierWithTrailingDot { name = "Foo", qualifiers = [] })
                , Located { end = { col = 1, row = 3 }, start = { col = 5, row = 1 } }
                    (Newlines
                        [ 0
                        ]
                        0
                    )
                , Located { end = { col = 8, row = 3 }, start = { col = 1, row = 3 } }
                    (Identifier
                        { name = "Bar"
                        , qualifiers =
                            [ "Foo"
                            ]
                        }
                    )
                , Located { end = { col = 1, row = 5 }, start = { col = 8, row = 3 } }
                    (Newlines
                        [ 0
                        ]
                        0
                    )
                , Located { end = { col = 12, row = 5 }, start = { col = 1, row = 5 } }
                    (Identifier
                        { name = "baz"
                        , qualifiers =
                            [ "Foo"
                            , "Bar"
                            ]
                        }
                    )
                , Located { end = { col = 1, row = 7 }, start = { col = 12, row = 5 } }
                    (Newlines
                        [ 0
                        ]
                        0
                    )
                , Located { end = { col = 11, row = 7 }, start = { col = 1, row = 7 } }
                    (IdentifierWithTrailingDot
                        { name = "Bing"
                        , qualifiers =
                            [ "Boor"
                            ]
                        }
                    )
                , Located { end = { col = 1, row = 9 }, start = { col = 11, row = 7 } }
                    (Newlines
                        [ 0
                        ]
                        0
                    )
                , Located { end = { col = 5, row = 9 }, start = { col = 1, row = 9 } } (IdentifierWithTrailingDot { name = "Bor", qualifiers = [] })
                , Located { end = { col = 6, row = 9 }, start = { col = 5, row = 9 } } (Whitespace 1)
                , Located { end = { col = 10, row = 9 }, start = { col = 6, row = 9 } } (IdentifierWithTrailingDot { name = "Big", qualifiers = [] })
                , Located { end = { col = 1, row = 11 }, start = { col = 10, row = 9 } }
                    (Newlines
                        [ 0
                        ]
                        0
                    )
                , Located { end = { col = 2, row = 11 }, start = { col = 1, row = 11 } } (Sigil SingleDot)
                , Located { end = { col = 4, row = 11 }, start = { col = 2, row = 11 } } (Identifier { name = "sf", qualifiers = [] })
                , Located { end = { col = 1, row = 13 }, start = { col = 4, row = 11 } }
                    (Newlines
                        [ 0
                        ]
                        0
                    )
                , Located { end = { col = 6, row = 13 }, start = { col = 1, row = 13 } } (Identifier { name = "sfsdf", qualifiers = [] })
                , Located { end = { col = 7, row = 13 }, start = { col = 6, row = 13 } } (Whitespace 1)
                , Located { end = { col = 8, row = 13 }, start = { col = 7, row = 13 } } (Sigil SingleDot)
                , Located { end = { col = 13, row = 13 }, start = { col = 8, row = 13 } } (Identifier { name = "sdfsd", qualifiers = [] })
                , Located { end = { col = 1, row = 15 }, start = { col = 13, row = 13 } }
                    (Newlines
                        [ 0
                        ]
                        0
                    )
                , Located { end = { col = 15, row = 15 }, start = { col = 1, row = 15 } }
                    (Identifier
                        { name = "sdgsghj"
                        , qualifiers =
                            [ "asfasf"
                            ]
                        }
                    )
                , Located { end = { col = 1, row = 17 }, start = { col = 15, row = 15 } }
                    (Newlines
                        [ 0
                        ]
                        0
                    )
                , Located { end = { col = 2, row = 17 }, start = { col = 1, row = 17 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 6, row = 17 }, start = { col = 2, row = 17 } } (Identifier { name = "shdf", qualifiers = [] })
                , Located { end = { col = 7, row = 17 }, start = { col = 6, row = 17 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 8, row = 17 }, start = { col = 7, row = 17 } } (Sigil SingleDot)
                , Located { end = { col = 13, row = 17 }, start = { col = 8, row = 17 } } (Identifier { name = "hellp", qualifiers = [] })
                , Located { end = { col = 1, row = 19 }, start = { col = 13, row = 17 } }
                    (Newlines
                        [ 0
                        ]
                        0
                    )
                , Located { end = { col = 2, row = 19 }, start = { col = 1, row = 19 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 7, row = 19 }, start = { col = 2, row = 19 } } (Identifier { name = "sjhsf", qualifiers = [] })
                , Located { end = { col = 8, row = 19 }, start = { col = 7, row = 19 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 9, row = 19 }, start = { col = 8, row = 19 } } (Whitespace 1)
                , Located { end = { col = 10, row = 19 }, start = { col = 9, row = 19 } } (Sigil SingleDot)
                , Located { end = { col = 15, row = 19 }, start = { col = 10, row = 19 } } (Identifier { name = "hello", qualifiers = [] })
                , Located { end = { col = 1, row = 21 }, start = { col = 15, row = 19 } }
                    (Newlines
                        [ 0
                        ]
                        0
                    )
                , Located { end = { col = 7, row = 21 }, start = { col = 1, row = 21 } } (Identifier { name = "sfhsdf", qualifiers = [] })
                , Located { end = { col = 8, row = 21 }, start = { col = 7, row = 21 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 9, row = 21 }, start = { col = 8, row = 21 } } (Sigil SingleDot)
                , Located { end = { col = 14, row = 21 }, start = { col = 9, row = 21 } } (Identifier { name = "hello", qualifiers = [] })
                , Located { end = { col = 15, row = 21 }, start = { col = 14, row = 21 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 1, row = 23 }, start = { col = 15, row = 21 } }
                    (Newlines
                        [ 0
                        ]
                        0
                    )
                , Located { end = { col = 5, row = 23 }, start = { col = 1, row = 23 } } (Keyword Case)
                , Located { end = { col = 6, row = 23 }, start = { col = 5, row = 23 } } (Sigil SingleDot)
                , Located { end = { col = 8, row = 23 }, start = { col = 6, row = 23 } } (Identifier { name = "hi", qualifiers = [] })
                , Located { end = { col = 1, row = 25 }, start = { col = 8, row = 23 } }
                    (Newlines
                        [ 0
                        ]
                        0
                    )
                , Located { end = { col = 8, row = 25 }, start = { col = 1, row = 25 } }
                    (Identifier
                        { name = "case"
                        , qualifiers =
                            [ "Hi"
                            ]
                        }
                    )
                , Located { end = { col = 1, row = 27 }, start = { col = 8, row = 25 } }
                    (Newlines
                        [ 0
                        ]
                        0
                    )
                , Located { end = { col = 5, row = 27 }, start = { col = 1, row = 27 } } (Keyword Case)
                , Located { end = { col = 6, row = 27 }, start = { col = 5, row = 27 } } (Whitespace 1)
                , Located { end = { col = 7, row = 27 }, start = { col = 6, row = 27 } } (Sigil SingleDot)
                , Located { end = { col = 9, row = 27 }, start = { col = 7, row = 27 } } (Identifier { name = "hi", qualifiers = [] })
                , Located { end = { col = 1, row = 29 }, start = { col = 9, row = 27 } }
                    (Newlines
                        [ 0
                        ]
                        0
                    )
                , Located { end = { col = 4, row = 29 }, start = { col = 1, row = 29 } } (IdentifierWithTrailingDot { name = "Hi", qualifiers = [] })
                , Located { end = { col = 5, row = 29 }, start = { col = 4, row = 29 } } (Whitespace 1)
                , Located { end = { col = 9, row = 29 }, start = { col = 5, row = 29 } } (Keyword Case)
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
                    , item = Just (Located { end = { col = 31, row = 1 }, start = { col = 30, row = 1 } } (Sigil (Bracket Round Close)))
                    , state =
                        State_BlockTypeAlias
                            (BlockTypeAlias_Completish "Function"
                                []
                                { nesting = NestingLeafType_Function { firstInput = TypeExpression_Unit, otherInputs = Stack [], output = Nothing }
                                , parents =
                                    [ NestingParentType_Bracket (Stack [])
                                    ]
                                }
                            )
                    }
                , Err
                    { error = Error_MissingFunctionReturnType
                    , item = Just (Located { end = { col = 37, row = 3 }, start = { col = 36, row = 3 } } (Sigil (Bracket Curly Close)))
                    , state =
                        State_BlockTypeAlias
                            (BlockTypeAlias_Completish "Function2"
                                []
                                { nesting = NestingLeafType_Function { firstInput = TypeExpression_Unit, otherInputs = Stack [], output = Nothing }
                                , parents =
                                    [ NestingParentType_PartialRecord { firstEntries = Stack [], lastEntryName = "a" }
                                    ]
                                }
                            )
                    }
                , Err
                    { error = Error_InvalidToken Expecting_Unknown
                    , item = Just (Located { end = { col = 32, row = 5 }, start = { col = 31, row = 5 } } (Sigil Comma))
                    , state =
                        State_BlockTypeAlias
                            (BlockTypeAlias_Completish "Function3"
                                []
                                { nesting = NestingLeafType_Function { firstInput = TypeExpression_Unit, otherInputs = Stack [], output = Nothing }
                                , parents =
                                    [ NestingParentType_Bracket (Stack [])
                                    ]
                                }
                            )
                    }
                , Err
                    { error = Error_InvalidToken Expecting_Unknown
                    , item = Just (Located { end = { col = 36, row = 7 }, start = { col = 35, row = 7 } } (Sigil Comma))
                    , state =
                        State_BlockTypeAlias
                            (BlockTypeAlias_Completish "Function3"
                                []
                                { nesting = NestingLeafType_Function { firstInput = TypeExpression_Unit, otherInputs = Stack [], output = Nothing }
                                , parents =
                                    [ NestingParentType_Bracket
                                        (Stack
                                            [ TypeExpression_NamedType { args = Stack [], name = "Int" }
                                            ]
                                        )
                                    ]
                                }
                            )
                    }
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Keyword Type)
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Keyword Alias)
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Whitespace 1)
                , Located { end = { col = 20, row = 1 }, start = { col = 12, row = 1 } } (Identifier { name = "Function", qualifiers = [] })
                , Located { end = { col = 21, row = 1 }, start = { col = 20, row = 1 } } (Whitespace 1)
                , Located { end = { col = 22, row = 1 }, start = { col = 21, row = 1 } } (Sigil Assign)
                , Located { end = { col = 23, row = 1 }, start = { col = 22, row = 1 } } (Whitespace 1)
                , Located { end = { col = 24, row = 1 }, start = { col = 23, row = 1 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 25, row = 1 }, start = { col = 24, row = 1 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 26, row = 1 }, start = { col = 25, row = 1 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 27, row = 1 }, start = { col = 26, row = 1 } } (Whitespace 1)
                , Located { end = { col = 29, row = 1 }, start = { col = 27, row = 1 } } (Sigil ThinArrow)
                , Located { end = { col = 30, row = 1 }, start = { col = 29, row = 1 } } (Whitespace 1)
                , Located { end = { col = 31, row = 1 }, start = { col = 30, row = 1 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 1, row = 3 }, start = { col = 31, row = 1 } }
                    (Newlines
                        [ 0
                        ]
                        0
                    )
                , Located { end = { col = 5, row = 3 }, start = { col = 1, row = 3 } } (Keyword Type)
                , Located { end = { col = 6, row = 3 }, start = { col = 5, row = 3 } } (Whitespace 1)
                , Located { end = { col = 11, row = 3 }, start = { col = 6, row = 3 } } (Keyword Alias)
                , Located { end = { col = 12, row = 3 }, start = { col = 11, row = 3 } } (Whitespace 1)
                , Located { end = { col = 21, row = 3 }, start = { col = 12, row = 3 } } (Identifier { name = "Function2", qualifiers = [] })
                , Located { end = { col = 22, row = 3 }, start = { col = 21, row = 3 } } (Whitespace 1)
                , Located { end = { col = 23, row = 3 }, start = { col = 22, row = 3 } } (Sigil Assign)
                , Located { end = { col = 24, row = 3 }, start = { col = 23, row = 3 } } (Whitespace 1)
                , Located { end = { col = 25, row = 3 }, start = { col = 24, row = 3 } } (Sigil (Bracket Curly Open))
                , Located { end = { col = 26, row = 3 }, start = { col = 25, row = 3 } } (Whitespace 1)
                , Located { end = { col = 27, row = 3 }, start = { col = 26, row = 3 } } (Identifier { name = "a", qualifiers = [] })
                , Located { end = { col = 28, row = 3 }, start = { col = 27, row = 3 } } (Sigil Colon)
                , Located { end = { col = 29, row = 3 }, start = { col = 28, row = 3 } } (Whitespace 1)
                , Located { end = { col = 30, row = 3 }, start = { col = 29, row = 3 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 31, row = 3 }, start = { col = 30, row = 3 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 32, row = 3 }, start = { col = 31, row = 3 } } (Whitespace 1)
                , Located { end = { col = 34, row = 3 }, start = { col = 32, row = 3 } } (Sigil ThinArrow)
                , Located { end = { col = 36, row = 3 }, start = { col = 34, row = 3 } } (Whitespace 2)
                , Located { end = { col = 37, row = 3 }, start = { col = 36, row = 3 } } (Sigil (Bracket Curly Close))
                , Located { end = { col = 1, row = 5 }, start = { col = 37, row = 3 } }
                    (Newlines
                        [ 0
                        ]
                        0
                    )
                , Located { end = { col = 5, row = 5 }, start = { col = 1, row = 5 } } (Keyword Type)
                , Located { end = { col = 6, row = 5 }, start = { col = 5, row = 5 } } (Whitespace 1)
                , Located { end = { col = 11, row = 5 }, start = { col = 6, row = 5 } } (Keyword Alias)
                , Located { end = { col = 12, row = 5 }, start = { col = 11, row = 5 } } (Whitespace 1)
                , Located { end = { col = 21, row = 5 }, start = { col = 12, row = 5 } } (Identifier { name = "Function3", qualifiers = [] })
                , Located { end = { col = 22, row = 5 }, start = { col = 21, row = 5 } } (Whitespace 1)
                , Located { end = { col = 23, row = 5 }, start = { col = 22, row = 5 } } (Sigil Assign)
                , Located { end = { col = 24, row = 5 }, start = { col = 23, row = 5 } } (Whitespace 1)
                , Located { end = { col = 25, row = 5 }, start = { col = 24, row = 5 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 26, row = 5 }, start = { col = 25, row = 5 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 27, row = 5 }, start = { col = 26, row = 5 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 28, row = 5 }, start = { col = 27, row = 5 } } (Whitespace 1)
                , Located { end = { col = 30, row = 5 }, start = { col = 28, row = 5 } } (Sigil ThinArrow)
                , Located { end = { col = 31, row = 5 }, start = { col = 30, row = 5 } } (Whitespace 1)
                , Located { end = { col = 32, row = 5 }, start = { col = 31, row = 5 } } (Sigil Comma)
                , Located { end = { col = 33, row = 5 }, start = { col = 32, row = 5 } } (Whitespace 1)
                , Located { end = { col = 34, row = 5 }, start = { col = 33, row = 5 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 35, row = 5 }, start = { col = 34, row = 5 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 36, row = 5 }, start = { col = 35, row = 5 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 1, row = 7 }, start = { col = 36, row = 5 } }
                    (Newlines
                        [ 0
                        ]
                        0
                    )
                , Located { end = { col = 5, row = 7 }, start = { col = 1, row = 7 } } (Keyword Type)
                , Located { end = { col = 6, row = 7 }, start = { col = 5, row = 7 } } (Whitespace 1)
                , Located { end = { col = 11, row = 7 }, start = { col = 6, row = 7 } } (Keyword Alias)
                , Located { end = { col = 12, row = 7 }, start = { col = 11, row = 7 } } (Whitespace 1)
                , Located { end = { col = 21, row = 7 }, start = { col = 12, row = 7 } } (Identifier { name = "Function3", qualifiers = [] })
                , Located { end = { col = 22, row = 7 }, start = { col = 21, row = 7 } } (Whitespace 1)
                , Located { end = { col = 23, row = 7 }, start = { col = 22, row = 7 } } (Sigil Assign)
                , Located { end = { col = 24, row = 7 }, start = { col = 23, row = 7 } } (Whitespace 1)
                , Located { end = { col = 25, row = 7 }, start = { col = 24, row = 7 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 28, row = 7 }, start = { col = 25, row = 7 } } (Identifier { name = "Int", qualifiers = [] })
                , Located { end = { col = 29, row = 7 }, start = { col = 28, row = 7 } } (Sigil Comma)
                , Located { end = { col = 30, row = 7 }, start = { col = 29, row = 7 } } (Whitespace 1)
                , Located { end = { col = 31, row = 7 }, start = { col = 30, row = 7 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 32, row = 7 }, start = { col = 31, row = 7 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 33, row = 7 }, start = { col = 32, row = 7 } } (Whitespace 1)
                , Located { end = { col = 35, row = 7 }, start = { col = 33, row = 7 } } (Sigil ThinArrow)
                , Located { end = { col = 36, row = 7 }, start = { col = 35, row = 7 } } (Sigil Comma)
                , Located { end = { col = 37, row = 7 }, start = { col = 36, row = 7 } } (Whitespace 1)
                , Located { end = { col = 38, row = 7 }, start = { col = 37, row = 7 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 39, row = 7 }, start = { col = 38, row = 7 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 40, row = 7 }, start = { col = 39, row = 7 } } (Sigil (Bracket Round Close))
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
                    { error = Error_TypeDoesNotTakeArgs2 (TypeExpression_Bracketed (TypeExpression_NamedType { args = Stack [], name = "Int" }))
                    , item = Just (Located { end = { col = 24, row = 1 }, start = { col = 23, row = 1 } } (Sigil (Bracket Round Open)))
                    , state = State_BlockTypeAlias (BlockTypeAlias_Completish "Hi" [] { nesting = NestingLeafType_Expr (TypeExpression_Bracketed (TypeExpression_NamedType { args = Stack [], name = "Int" })), parents = [] })
                    }
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Keyword Type)
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Keyword Alias)
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Whitespace 1)
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Identifier { name = "Hi", qualifiers = [] })
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Whitespace 1)
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Sigil Assign)
                , Located { end = { col = 17, row = 1 }, start = { col = 16, row = 1 } } (Whitespace 1)
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 21, row = 1 }, start = { col = 18, row = 1 } } (Identifier { name = "Int", qualifiers = [] })
                , Located { end = { col = 22, row = 1 }, start = { col = 21, row = 1 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 23, row = 1 }, start = { col = 22, row = 1 } } (Whitespace 1)
                , Located { end = { col = 24, row = 1 }, start = { col = 23, row = 1 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 25, row = 1 }, start = { col = 24, row = 1 } } (Sigil (Bracket Round Close))
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
                    , item = Just (Located { end = { col = 21, row = 1 }, start = { col = 20, row = 1 } } (Sigil (Bracket Round Open)))
                    , state = State_BlockTypeAlias (BlockTypeAlias_Completish "Hi" [] { nesting = NestingLeafType_Expr TypeExpression_Unit, parents = [] })
                    }
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Keyword Type)
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Keyword Alias)
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Whitespace 1)
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Identifier { name = "Hi", qualifiers = [] })
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
                    , item = Just (Located { end = { col = 21, row = 1 }, start = { col = 20, row = 1 } } (Sigil (Bracket Round Open)))
                    , state = State_BlockTypeAlias (BlockTypeAlias_Completish "Hi" [] { nesting = NestingLeafType_Expr TypeExpression_Unit, parents = [] })
                    }
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Keyword Type)
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Keyword Alias)
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Whitespace 1)
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Identifier { name = "Hi", qualifiers = [] })
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Whitespace 1)
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Sigil Assign)
                , Located { end = { col = 17, row = 1 }, start = { col = 16, row = 1 } } (Whitespace 1)
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 19, row = 1 }, start = { col = 18, row = 1 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 20, row = 1 }, start = { col = 19, row = 1 } } (Whitespace 1)
                , Located { end = { col = 21, row = 1 }, start = { col = 20, row = 1 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 24, row = 1 }, start = { col = 21, row = 1 } } (Identifier { name = "Int", qualifiers = [] })
                , Located { end = { col = 25, row = 1 }, start = { col = 24, row = 1 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 1, row = 2 }, start = { col = 25, row = 1 } } (Newlines [] 0)
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
                    { error = Error_PartwayThroughTypeAlias
                    , item = Just (Located { end = { col = 1, row = 2 }, start = { col = 19, row = 1 } } (Newlines [] 0))
                    , state = State_BlockTypeAlias (BlockTypeAlias_NamedAssigns "Model" [])
                    }
                , Err
                    { error = Error_PartwayThroughTypeAlias
                    , item = Just (Located { end = { col = 1, row = 3 }, start = { col = 9, row = 2 } } (Newlines [] 0))
                    , state =
                        State_BlockValueDeclaration
                            (BlockValueDeclaration_Named
                                { args =
                                    Stack
                                        [ Located { end = { col = 9, row = 2 }, start = { col = 6, row = 2 } } "Int"
                                        ]
                                , name = Located { end = { col = 5, row = 2 }, start = { col = 1, row = 2 } } "List"
                                }
                            )
                    }
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Keyword Type)
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Keyword Alias)
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Whitespace 1)
                , Located { end = { col = 17, row = 1 }, start = { col = 12, row = 1 } } (Identifier { name = "Model", qualifiers = [] })
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Whitespace 1)
                , Located { end = { col = 19, row = 1 }, start = { col = 18, row = 1 } } (Sigil Assign)
                , Located { end = { col = 1, row = 2 }, start = { col = 19, row = 1 } } (Newlines [] 0)
                , Located { end = { col = 5, row = 2 }, start = { col = 1, row = 2 } } (Identifier { name = "List", qualifiers = [] })
                , Located { end = { col = 6, row = 2 }, start = { col = 5, row = 2 } } (Whitespace 1)
                , Located { end = { col = 9, row = 2 }, start = { col = 6, row = 2 } } (Identifier { name = "Int", qualifiers = [] })
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
                    { error = Error_PartwayThroughTypeAlias
                    , item = Just (Located { end = { col = 1, row = 2 }, start = { col = 11, row = 1 } } (Newlines [] 0))
                    , state = State_BlockTypeAlias BlockTypeAlias_Keywords
                    }
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Keyword Type)
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Keyword Alias)
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
                    { error = Error_PartwayThroughTypeAlias
                    , item = Just (Located { end = { col = 1, row = 2 }, start = { col = 14, row = 1 } } (Newlines [] 0))
                    , state = State_BlockTypeAlias (BlockTypeAlias_Named "Hi" (Stack []))
                    }
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Keyword Type)
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Keyword Alias)
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Whitespace 1)
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Identifier { name = "Hi", qualifiers = [] })
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
                    { error = Error_PartwayThroughTypeAlias
                    , item = Just (Located { end = { col = 1, row = 2 }, start = { col = 16, row = 1 } } (Newlines [] 0))
                    , state = State_BlockTypeAlias (BlockTypeAlias_NamedAssigns "Hi" [])
                    }
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Keyword Type)
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Keyword Alias)
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Whitespace 1)
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Identifier { name = "Hi", qualifiers = [] })
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Whitespace 1)
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Sigil Assign)
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
                    { error = Error_PartwayThroughTypeAlias
                    , item = Just (Located { end = { col = 1, row = 2 }, start = { col = 18, row = 1 } } (Newlines [] 0))
                    , state = State_BlockTypeAlias (BlockTypeAlias_Completish "Hi" [] { nesting = NestingLeafType_Bracket (Stack []) Nothing, parents = [] })
                    }
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Keyword Type)
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Keyword Alias)
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Whitespace 1)
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Identifier { name = "Hi", qualifiers = [] })
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Whitespace 1)
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Sigil Assign)
                , Located { end = { col = 17, row = 1 }, start = { col = 16, row = 1 } } (Whitespace 1)
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Sigil (Bracket Round Open))
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
                    { error = Error_PartwayThroughTypeAlias
                    , item = Just (Located { end = { col = 1, row = 3 }, start = { col = 12, row = 2 } } (Newlines [] 0))
                    , state =
                        State_BlockTypeAlias
                            (BlockTypeAlias_Completish "Hi"
                                []
                                { nesting = NestingLeafType_TypeWithArgs { args = Stack [], name = "Int" }
                                , parents =
                                    [ NestingParentType_Bracket (Stack [])
                                    ]
                                }
                            )
                    }
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Keyword Type)
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Keyword Alias)
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Whitespace 1)
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Identifier { name = "Hi", qualifiers = [] })
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Whitespace 1)
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Sigil Assign)
                , Located { end = { col = 17, row = 1 }, start = { col = 16, row = 1 } } (Whitespace 1)
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 9, row = 2 }, start = { col = 18, row = 1 } } (Newlines [] 8)
                , Located { end = { col = 12, row = 2 }, start = { col = 9, row = 2 } } (Identifier { name = "Int", qualifiers = [] })
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
                    { error = Error_PartwayThroughTypeAlias
                    , item = Just (Located { end = { col = 1, row = 2 }, start = { col = 18, row = 1 } } (Newlines [] 0))
                    , state = State_BlockTypeAlias (BlockTypeAlias_Completish "Ty" [] { nesting = NestingLeafType_PartialRecord { firstEntries = Stack [], lastEntry = LastEntryOfRecord_Empty }, parents = [] })
                    }
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Keyword Type)
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Keyword Alias)
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Whitespace 1)
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Identifier { name = "Ty", qualifiers = [] })
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Whitespace 1)
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Sigil Assign)
                , Located { end = { col = 17, row = 1 }, start = { col = 16, row = 1 } } (Whitespace 1)
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Sigil (Bracket Curly Open))
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
                    , item = Just (Located { end = { col = 24, row = 1 }, start = { col = 22, row = 1 } } (Identifier { name = "j7", qualifiers = [] }))
                    , state = State_BlockTypeAlias (BlockTypeAlias_Completish "Ty" [] { nesting = NestingLeafType_PartialRecord { firstEntries = Stack [], lastEntry = LastEntryOfRecord_Key "hi" }, parents = [] })
                    }
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Keyword Type)
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Keyword Alias)
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Whitespace 1)
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Identifier { name = "Ty", qualifiers = [] })
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Whitespace 1)
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Sigil Assign)
                , Located { end = { col = 17, row = 1 }, start = { col = 16, row = 1 } } (Whitespace 1)
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Sigil (Bracket Curly Open))
                , Located { end = { col = 19, row = 1 }, start = { col = 18, row = 1 } } (Whitespace 1)
                , Located { end = { col = 21, row = 1 }, start = { col = 19, row = 1 } } (Identifier { name = "hi", qualifiers = [] })
                , Located { end = { col = 22, row = 1 }, start = { col = 21, row = 1 } } (Whitespace 1)
                , Located { end = { col = 24, row = 1 }, start = { col = 22, row = 1 } } (Identifier { name = "j7", qualifiers = [] })
                , Located { end = { col = 25, row = 1 }, start = { col = 24, row = 1 } } (Whitespace 1)
                , Located { end = { col = 26, row = 1 }, start = { col = 25, row = 1 } } (Sigil (Bracket Curly Close))
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
                            (BlockTypeAlias_Completish "Hi"
                                []
                                { nesting =
                                    NestingLeafType_Expr
                                        (TypeExpression_Tuple (TypeExpression_NamedType { args = Stack [], name = "Int" })
                                            (TypeExpression_NamedType { args = Stack [], name = "A" })
                                            [ TypeExpression_NamedType { args = Stack [], name = "B" }
                                            , TypeExpression_NamedType { args = Stack [], name = "C" }
                                            , TypeExpression_NamedType { args = Stack [], name = "D" }
                                            ]
                                        )
                                , parents = []
                                }
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
                            (BlockTypeAlias_Completish "Hi"
                                []
                                { nesting =
                                    NestingLeafType_Expr
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
                                , parents = []
                                }
                            )
                    }
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Keyword Type)
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Keyword Alias)
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Whitespace 1)
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Identifier { name = "Hi", qualifiers = [] })
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Whitespace 1)
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Sigil Assign)
                , Located { end = { col = 17, row = 1 }, start = { col = 16, row = 1 } } (Whitespace 1)
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 21, row = 1 }, start = { col = 18, row = 1 } } (Identifier { name = "Int", qualifiers = [] })
                , Located { end = { col = 22, row = 1 }, start = { col = 21, row = 1 } } (Sigil Comma)
                , Located { end = { col = 23, row = 1 }, start = { col = 22, row = 1 } } (Whitespace 1)
                , Located { end = { col = 24, row = 1 }, start = { col = 23, row = 1 } } (Identifier { name = "A", qualifiers = [] })
                , Located { end = { col = 25, row = 1 }, start = { col = 24, row = 1 } } (Sigil Comma)
                , Located { end = { col = 26, row = 1 }, start = { col = 25, row = 1 } } (Whitespace 1)
                , Located { end = { col = 27, row = 1 }, start = { col = 26, row = 1 } } (Identifier { name = "B", qualifiers = [] })
                , Located { end = { col = 28, row = 1 }, start = { col = 27, row = 1 } } (Sigil Comma)
                , Located { end = { col = 29, row = 1 }, start = { col = 28, row = 1 } } (Whitespace 1)
                , Located { end = { col = 30, row = 1 }, start = { col = 29, row = 1 } } (Identifier { name = "C", qualifiers = [] })
                , Located { end = { col = 31, row = 1 }, start = { col = 30, row = 1 } } (Sigil Comma)
                , Located { end = { col = 32, row = 1 }, start = { col = 31, row = 1 } } (Whitespace 1)
                , Located { end = { col = 33, row = 1 }, start = { col = 32, row = 1 } } (Identifier { name = "D", qualifiers = [] })
                , Located { end = { col = 34, row = 1 }, start = { col = 33, row = 1 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 1, row = 2 }, start = { col = 34, row = 1 } } (Newlines [] 0)
                , Located { end = { col = 5, row = 2 }, start = { col = 1, row = 2 } } (Keyword Type)
                , Located { end = { col = 6, row = 2 }, start = { col = 5, row = 2 } } (Whitespace 1)
                , Located { end = { col = 11, row = 2 }, start = { col = 6, row = 2 } } (Keyword Alias)
                , Located { end = { col = 12, row = 2 }, start = { col = 11, row = 2 } } (Whitespace 1)
                , Located { end = { col = 14, row = 2 }, start = { col = 12, row = 2 } } (Identifier { name = "Hi", qualifiers = [] })
                , Located { end = { col = 15, row = 2 }, start = { col = 14, row = 2 } } (Whitespace 1)
                , Located { end = { col = 16, row = 2 }, start = { col = 15, row = 2 } } (Sigil Assign)
                , Located { end = { col = 17, row = 2 }, start = { col = 16, row = 2 } } (Whitespace 1)
                , Located { end = { col = 18, row = 2 }, start = { col = 17, row = 2 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 19, row = 2 }, start = { col = 18, row = 2 } } (Identifier { name = "A", qualifiers = [] })
                , Located { end = { col = 20, row = 2 }, start = { col = 19, row = 2 } } (Whitespace 1)
                , Located { end = { col = 23, row = 2 }, start = { col = 20, row = 2 } } (Identifier { name = "Int", qualifiers = [] })
                , Located { end = { col = 24, row = 2 }, start = { col = 23, row = 2 } } (Sigil Comma)
                , Located { end = { col = 25, row = 2 }, start = { col = 24, row = 2 } } (Whitespace 1)
                , Located { end = { col = 26, row = 2 }, start = { col = 25, row = 2 } } (Identifier { name = "C", qualifiers = [] })
                , Located { end = { col = 27, row = 2 }, start = { col = 26, row = 2 } } (Whitespace 1)
                , Located { end = { col = 28, row = 2 }, start = { col = 27, row = 2 } } (Identifier { name = "D", qualifiers = [] })
                , Located { end = { col = 29, row = 2 }, start = { col = 28, row = 2 } } (Whitespace 1)
                , Located { end = { col = 30, row = 2 }, start = { col = 29, row = 2 } } (Identifier { name = "E", qualifiers = [] })
                , Located { end = { col = 31, row = 2 }, start = { col = 30, row = 2 } } (Whitespace 1)
                , Located { end = { col = 32, row = 2 }, start = { col = 31, row = 2 } } (Identifier { name = "F", qualifiers = [] })
                , Located { end = { col = 33, row = 2 }, start = { col = 32, row = 2 } } (Sigil Comma)
                , Located { end = { col = 34, row = 2 }, start = { col = 33, row = 2 } } (Whitespace 1)
                , Located { end = { col = 35, row = 2 }, start = { col = 34, row = 2 } } (Identifier { name = "H", qualifiers = [] })
                , Located { end = { col = 36, row = 2 }, start = { col = 35, row = 2 } } (Whitespace 1)
                , Located { end = { col = 37, row = 2 }, start = { col = 36, row = 2 } } (Identifier { name = "I", qualifiers = [] })
                , Located { end = { col = 38, row = 2 }, start = { col = 37, row = 2 } } (Whitespace 1)
                , Located { end = { col = 39, row = 2 }, start = { col = 38, row = 2 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 40, row = 2 }, start = { col = 39, row = 2 } } (Identifier { name = "J", qualifiers = [] })
                , Located { end = { col = 41, row = 2 }, start = { col = 40, row = 2 } } (Whitespace 1)
                , Located { end = { col = 42, row = 2 }, start = { col = 41, row = 2 } } (Identifier { name = "K", qualifiers = [] })
                , Located { end = { col = 43, row = 2 }, start = { col = 42, row = 2 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 44, row = 2 }, start = { col = 43, row = 2 } } (Sigil Comma)
                , Located { end = { col = 45, row = 2 }, start = { col = 44, row = 2 } } (Whitespace 1)
                , Located { end = { col = 46, row = 2 }, start = { col = 45, row = 2 } } (Identifier { name = "L", qualifiers = [] })
                , Located { end = { col = 47, row = 2 }, start = { col = 46, row = 2 } } (Whitespace 1)
                , Located { end = { col = 48, row = 2 }, start = { col = 47, row = 2 } } (Identifier { name = "M", qualifiers = [] })
                , Located { end = { col = 49, row = 2 }, start = { col = 48, row = 2 } } (Whitespace 1)
                , Located { end = { col = 50, row = 2 }, start = { col = 49, row = 2 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 51, row = 2 }, start = { col = 50, row = 2 } } (Sigil (Bracket Round Close))
                , Located { end = { col = 52, row = 2 }, start = { col = 51, row = 2 } } (Whitespace 1)
                , Located { end = { col = 53, row = 2 }, start = { col = 52, row = 2 } } (Identifier { name = "O", qualifiers = [] })
                , Located { end = { col = 54, row = 2 }, start = { col = 53, row = 2 } } (Whitespace 1)
                , Located { end = { col = 55, row = 2 }, start = { col = 54, row = 2 } } (Identifier { name = "P", qualifiers = [] })
                , Located { end = { col = 56, row = 2 }, start = { col = 55, row = 2 } } (Sigil (Bracket Round Close))
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
                    , item = Just (Located { end = { col = 24, row = 1 }, start = { col = 23, row = 1 } } (Sigil Comma))
                    , state =
                        State_BlockTypeAlias
                            (BlockTypeAlias_Completish "Hi"
                                []
                                { nesting =
                                    NestingLeafType_Bracket
                                        (Stack
                                            [ TypeExpression_NamedType { args = Stack [], name = "Int" }
                                            ]
                                        )
                                        Nothing
                                , parents = []
                                }
                            )
                    }
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Keyword Type)
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Keyword Alias)
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Whitespace 1)
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Identifier { name = "Hi", qualifiers = [] })
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Whitespace 1)
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Sigil Assign)
                , Located { end = { col = 17, row = 1 }, start = { col = 16, row = 1 } } (Whitespace 1)
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 21, row = 1 }, start = { col = 18, row = 1 } } (Identifier { name = "Int", qualifiers = [] })
                , Located { end = { col = 22, row = 1 }, start = { col = 21, row = 1 } } (Sigil Comma)
                , Located { end = { col = 23, row = 1 }, start = { col = 22, row = 1 } } (Whitespace 1)
                , Located { end = { col = 24, row = 1 }, start = { col = 23, row = 1 } } (Sigil Comma)
                , Located { end = { col = 25, row = 1 }, start = { col = 24, row = 1 } } (Whitespace 1)
                , Located { end = { col = 26, row = 1 }, start = { col = 25, row = 1 } } (Identifier { name = "A", qualifiers = [] })
                , Located { end = { col = 27, row = 1 }, start = { col = 26, row = 1 } } (Sigil (Bracket Round Close))
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
                    { error = Error_PartwayThroughTypeAlias
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
                            (BlockTypeAlias_Completish "Hi"
                                []
                                { nesting =
                                    NestingLeafType_Bracket
                                        (Stack
                                            [ TypeExpression_NamedType { args = Stack [], name = "Int" }
                                            ]
                                        )
                                        Nothing
                                , parents = []
                                }
                            )
                    }
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Keyword Type)
                , Located { end = { col = 6, row = 1 }, start = { col = 5, row = 1 } } (Whitespace 1)
                , Located { end = { col = 11, row = 1 }, start = { col = 6, row = 1 } } (Keyword Alias)
                , Located { end = { col = 12, row = 1 }, start = { col = 11, row = 1 } } (Whitespace 1)
                , Located { end = { col = 14, row = 1 }, start = { col = 12, row = 1 } } (Identifier { name = "Hi", qualifiers = [] })
                , Located { end = { col = 15, row = 1 }, start = { col = 14, row = 1 } } (Whitespace 1)
                , Located { end = { col = 16, row = 1 }, start = { col = 15, row = 1 } } (Sigil Assign)
                , Located { end = { col = 17, row = 1 }, start = { col = 16, row = 1 } } (Whitespace 1)
                , Located { end = { col = 18, row = 1 }, start = { col = 17, row = 1 } } (Sigil (Bracket Round Open))
                , Located { end = { col = 21, row = 1 }, start = { col = 18, row = 1 } } (Identifier { name = "Int", qualifiers = [] })
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
                    { error = Error_PartwayThroughTypeAlias
                    , item = Just (Located { end = { col = 1, row = 2 }, start = { col = 5, row = 1 } } (Newlines [] 0))
                    , state = State_BlockFirstItem BlockFirstItem_Type
                    }
                ]
      , lexed =
            Ok
                [ Located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } } (Keyword Type)
                , Located { end = { col = 1, row = 2 }, start = { col = 5, row = 1 } } (Newlines [] 0)
                ]
      }
    ]
