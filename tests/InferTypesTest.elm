module InferTypesTest exposing (isParametric, niceVarName, typeInference, typeToString)

import AST.Canonical as Canonical
import AST.Canonical.Unwrapped as CanonicalU
import AST.Common.Literal as Literal
import AST.Common.Located as Located
import AST.Common.Type as Type exposing (Type(..))
import AST.Typed as Typed
import AssocList as Dict
import Data.ModuleName as ModuleName exposing (ModuleName)
import Data.VarName as VarName exposing (VarName)
import Error exposing (TypeError(..))
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Stage.InferTypes
import Test exposing (Test, describe, fuzz, test)
import TestHelpers exposing (dumpType, located, module_, var)


typeInference : Test
typeInference =
    let
        runSection : String -> List ( String, CanonicalU.Expr, Result Error.TypeError Type ) -> Test
        runSection description tests =
            describe description
                (List.map runTest tests)

        runTest : ( String, CanonicalU.Expr, Result Error.TypeError Type ) -> Test
        runTest ( description, input, output ) =
            test description <|
                \() ->
                    input
                        |> Canonical.fromUnwrapped
                        |> Stage.InferTypes.inferExpr
                        |> Result.map Typed.getType
                        |> Expect.equal output
    in
    describe "Stage.InferType"
        [ runSection "list"
            [ ( "empty list"
              , CanonicalU.List []
              , Ok (List (Var 1))
              )
            , ( "one item"
              , CanonicalU.List [ CanonicalU.Literal (Literal.Bool True) ]
              , Ok (List Bool)
              )
            , ( "more items"
              , CanonicalU.List
                    [ CanonicalU.Literal (Literal.Int 1)
                    , CanonicalU.Literal (Literal.Int 2)
                    , CanonicalU.Literal (Literal.Int 3)
                    ]
              , Ok (List Int)
              )
            , ( "different types"
              , CanonicalU.List
                    [ CanonicalU.Literal (Literal.Int 1)
                    , CanonicalU.Literal (Literal.String "two")
                    ]
              , Err (Error.TypeMismatch Type.Int Type.String)
              )
            , ( "more items with different types"
              , CanonicalU.List
                    [ CanonicalU.Literal (Literal.Bool True)
                    , CanonicalU.Literal (Literal.String "two")
                    , CanonicalU.Literal (Literal.Int 3)
                    ]
              , Err (Error.TypeMismatch Type.Bool Type.String)
              )
            , ( "List of List of Int"
              , CanonicalU.List
                    [ CanonicalU.List [ CanonicalU.Literal (Literal.Int 1) ]
                    , CanonicalU.List [ CanonicalU.Literal (Literal.Int 2) ]
                    ]
              , Ok (List (List Int))
              )
            , ( "List of List of different types"
              , CanonicalU.List
                    [ CanonicalU.List [ CanonicalU.Literal (Literal.Int 1) ]
                    , CanonicalU.List [ CanonicalU.Literal (Literal.Bool False) ]
                    ]
              , Err (Error.TypeMismatch Type.Int Type.Bool)
              )
            ]
        , runSection "tuple"
            [ ( "items with the same types"
              , CanonicalU.Tuple
                    (CanonicalU.Literal (Literal.String "Hello"))
                    (CanonicalU.Literal (Literal.String "Elm"))
              , Ok (Tuple String String)
              )
            , ( "items of different types"
              , CanonicalU.Tuple
                    (CanonicalU.Literal (Literal.Bool True))
                    (CanonicalU.Literal (Literal.Int 1))
              , Ok (Tuple Bool Int)
              )
            ]
        , runSection "tuple3"
            [ ( "same types"
              , CanonicalU.Tuple3
                    (CanonicalU.Literal (Literal.String "FP"))
                    (CanonicalU.Literal (Literal.String "is"))
                    (CanonicalU.Literal (Literal.String "good"))
              , Ok (Tuple3 String String String)
              )
            , ( "different types"
              , CanonicalU.Tuple3
                    (CanonicalU.Literal (Literal.Bool True))
                    (CanonicalU.Literal (Literal.Int 1))
                    (CanonicalU.Literal (Literal.Char 'h'))
              , Ok (Tuple3 Bool Int Char)
              )
            ]
        , runSection "plus"
            [ ( "same types"
              , CanonicalU.Plus
                    (CanonicalU.Var { qualifier = module_ "Main", name = var "age" })
                    (CanonicalU.Literal (Literal.Int 1))
              , Ok Int
              )
            ]
        , runSection "cons"
            [ ( "simple case"
              , CanonicalU.Cons
                    (CanonicalU.Literal (Literal.Int 1))
                    (CanonicalU.List [])
              , Ok (List Int)
              )
            , ( "advanced case"
              , CanonicalU.Cons
                    (CanonicalU.Literal (Literal.Int 1))
                    (CanonicalU.Cons
                        (CanonicalU.Literal (Literal.Int 2))
                        (CanonicalU.List
                            [ CanonicalU.Literal (Literal.Int 3)
                            , CanonicalU.Literal (Literal.Int 4)
                            ]
                        )
                    )
              , Ok (List Int)
              )
            , ( "fail with wrong argument types"
              , CanonicalU.Cons
                    (CanonicalU.List
                        [ CanonicalU.Literal (Literal.Int 1)
                        , CanonicalU.Literal (Literal.Int 2)
                        ]
                    )
                    (CanonicalU.List
                        [ CanonicalU.Literal (Literal.Int 3)
                        , CanonicalU.Literal (Literal.Int 4)
                        ]
                    )
              , Err
                    (Error.TypeMismatch
                        (Type.List Type.Int)
                        Type.Int
                    )
              )
            , ( "variable and list"
              , CanonicalU.Cons
                    (CanonicalU.Var { qualifier = module_ "Main", name = var "age" })
                    (CanonicalU.List [ CanonicalU.Literal (Literal.Int 1) ])
              , Ok (List Int)
              )
            ]
        ]


typeToString : Test
typeToString =
    let
        toStringOnce : Type -> String
        toStringOnce type_ =
            type_
                |> Type.toString Type.emptyState
                |> Tuple.first

        runTest ( description, input, output ) =
            test description <|
                \() ->
                    toStringOnce input
                        |> Expect.equal output

        runEqual ( description, input, output ) =
            test description <|
                \() ->
                    Expect.equal input output
    in
    describe "Type.toString"
        [ describe "list"
            [ runTest
                ( "empty list"
                , Type.List (Type.Var 0)
                , "List a"
                )
            , runTest
                ( "one item in list"
                , Type.List Type.Bool
                , "List Bool"
                )
            , runTest
                ( "list of list of String"
                , Type.List (Type.List Type.String)
                , "List (List String)"
                )
            ]
        , describe "lambda"
            [ runTest
                ( "function with one param"
                , Type.Function (Type.Var 99) Type.Int
                , "a -> Int"
                )
            , runTest
                ( "function with two params"
                , Type.Function (Type.Var 0) (Type.Function (Type.Var 1) (Type.Var 1))
                , "a -> b -> b"
                )
            , runTest
                ( "function as param"
                , Type.Function (Type.Function (Type.Var 9) (Type.Var 9)) (Type.Var 0)
                , "(a -> a) -> b"
                )
            , runTest
                ( "list of functions"
                , Type.List (Type.Function (Type.Var 0) (Type.Var 0))
                , "List (a -> a)"
                )
            ]
        , describe "edges"
            [ runEqual
                ( "Var number doesn't count"
                , toStringOnce <| Type.List (Type.Var 0)
                , toStringOnce <| Type.List (Type.Var 1)
                )
            , runEqual
                ( "TypeMismatch types share vars index"
                , Error.toString
                    (Error.TypeError
                        (Error.TypeMismatch
                            (Type.Function (Type.List (Type.Var 0)) (Type.Var 1))
                            (Type.Function (Type.List (Type.Var 1)) (Type.Var 0))
                        )
                    )
                , "The types `(List a) -> b` and `(List b) -> a` don't match."
                )
            ]
        , describe "tuples"
            [ runTest
                ( "tuple with two literals"
                , Type.Tuple Type.Int Type.String
                , "( Int, String )"
                )
            , runTest
                ( "tuple with two params"
                , Type.Tuple (Type.Var 0) (Type.Var 1)
                , "( a, b )"
                )
            , runTest
                ( "tuple with tree params"
                , Type.Tuple3 (Type.Var 0) (Type.Var 1) (Type.Var 2)
                , "( a, b, c )"
                )
            ]
        , describe "user defined type"
            [ runTest
                ( "maybe int"
                , Type.UserDefinedType
                    ( ModuleName.fromString "Maybe", VarName.fromString "Maybe" )
                    [ Type.Int ]
                , "Maybe.Maybe Int"
                )
            , runTest
                ( "maybe a"
                , Type.UserDefinedType
                    ( ModuleName.fromString "Maybe", VarName.fromString "Maybe" )
                    [ Type.Var 0 ]
                , "Maybe.Maybe a"
                )
            ]
        ]


niceVarName : Test
niceVarName =
    let
        runTest ( input, output ) =
            test output <|
                \() ->
                    Type.niceVarName input
                        |> Expect.equal output
    in
    describe "Type.niceVarName" <|
        List.map runTest
            [ ( 0, "a" )
            , ( 1, "b" )
            , ( 2, "c" )
            , ( 25, "z" )

            --
            , ( 26, "a1" )
            , ( 49, "x1" )
            , ( 50, "y1" )
            , ( 51, "z1" )

            --
            , ( 52, "a2" )
            , ( 77, "z2" )

            --
            , ( 259, "z9" )
            , ( 260, "a10" )
            ]


isParametric : Test
isParametric =
    let
        runTest : ( Type, Bool ) -> Test
        runTest ( input, output ) =
            test (dumpType input) <|
                \() ->
                    input
                        |> Type.isParametric
                        |> Expect.equal output
    in
    describe "Type.isParametric" <|
        List.map runTest
            [ ( Unit, False )
            , ( Bool, False )
            , ( Char, False )
            , ( Int, False )
            , ( String, False )
            , ( Var 0, True )
            , ( Function Int String, False )
            , ( Function (Var 0) Int, True )
            , ( Function String (Var 0), True )
            , ( Function Int (Function (Var 0) (Var 0)), True )
            , ( List Int, False )
            , ( List (Var 0), True )
            , ( List (List Int), False )
            , ( List (List (Var 0)), True )
            , ( Tuple Int String, False )
            , ( Tuple (Var 0) Int, True )
            , ( Tuple String (Var 0), True )
            , ( Tuple (List (Var 0)) Int, True )
            , ( Tuple Char (Tuple Int (Var 0)), True )
            , ( Tuple3 Int String Bool, False )
            , ( Tuple3 (Var 0) Int Char, True )
            , ( Tuple3 String (Var 0) Unit, True )
            , ( Tuple3 Bool Unit (Var 0), True )
            , ( Tuple3 (List (Var 0)) Int Char, True )
            , ( Tuple3 String (Function (Var 0) Int) Unit, True )
            , ( Tuple3 Bool Unit (Tuple (Var 0) Int), True )
            ]
