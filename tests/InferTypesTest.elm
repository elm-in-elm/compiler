module InferTypesTest exposing (isParametric, niceVarName, typeInference, typeToString)

import Dict
import Elm.AST.Canonical as Canonical
import Elm.AST.Canonical.Unwrapped as CanonicalU
import Elm.AST.Typed as Typed
import Elm.Compiler.Error as Error exposing (Error(..), TypeError(..))
import Elm.Data.Type as Type exposing (Type(..))
import Elm.Data.Type.ToString as TypeToString
import Expect
import Stage.InferTypes
import Test exposing (Test, describe, test)
import TestHelpers exposing (dumpType)


typeInference : Test
typeInference =
    let
        runSection : String -> List ( String, CanonicalU.Expr, Result TypeError Type ) -> Test
        runSection description tests =
            describe description
                (List.map runTest tests)

        runTest : ( String, CanonicalU.Expr, Result TypeError Type ) -> Test
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
              , CanonicalU.List [ CanonicalU.Bool True ]
              , Ok (List Bool)
              )
            , ( "more items"
              , CanonicalU.List
                    [ CanonicalU.Int 1
                    , CanonicalU.Int 2
                    , CanonicalU.Int 3
                    ]
              , Ok (List Int)
              )
            , ( "different types"
              , CanonicalU.List
                    [ CanonicalU.Int 1
                    , CanonicalU.String "two"
                    ]
              , Err (TypeMismatch Int String)
              )
            , ( "more items with different types"
              , CanonicalU.List
                    [ CanonicalU.Bool True
                    , CanonicalU.String "two"
                    , CanonicalU.Int 3
                    ]
              , Err (TypeMismatch Bool String)
              )
            , ( "List of List of Int"
              , CanonicalU.List
                    [ CanonicalU.List [ CanonicalU.Int 1 ]
                    , CanonicalU.List [ CanonicalU.Int 2 ]
                    ]
              , Ok (List (List Int))
              )
            , ( "List of List of different types"
              , CanonicalU.List
                    [ CanonicalU.List [ CanonicalU.Int 1 ]
                    , CanonicalU.List [ CanonicalU.Bool False ]
                    ]
              , Err (TypeMismatch Int Bool)
              )
            ]
        , runSection "tuple"
            [ ( "items with the same types"
              , CanonicalU.Tuple
                    (CanonicalU.String "Hello")
                    (CanonicalU.String "Elm")
              , Ok (Tuple String String)
              )
            , ( "items of different types"
              , CanonicalU.Tuple
                    (CanonicalU.Bool True)
                    (CanonicalU.Int 1)
              , Ok (Tuple Bool Int)
              )
            ]
        , runSection "tuple3"
            [ ( "same types"
              , CanonicalU.Tuple3
                    (CanonicalU.String "FP")
                    (CanonicalU.String "is")
                    (CanonicalU.String "good")
              , Ok (Tuple3 String String String)
              )
            , ( "different types"
              , CanonicalU.Tuple3
                    (CanonicalU.Bool True)
                    (CanonicalU.Int 1)
                    (CanonicalU.Char 'h')
              , Ok (Tuple3 Bool Int Char)
              )
            ]
        , runSection "plus"
            [ ( "same types"
              , CanonicalU.Plus
                    (CanonicalU.Var { module_ = "Main", name = "age" })
                    (CanonicalU.Int 1)
              , Ok Int
              )
            ]
        , runSection "cons"
            [ ( "simple case"
              , CanonicalU.Cons
                    (CanonicalU.Int 1)
                    (CanonicalU.List [])
              , Ok (List Int)
              )
            , ( "advanced case"
              , CanonicalU.Cons
                    (CanonicalU.Int 1)
                    (CanonicalU.Cons
                        (CanonicalU.Int 2)
                        (CanonicalU.List
                            [ CanonicalU.Int 3
                            , CanonicalU.Int 4
                            ]
                        )
                    )
              , Ok (List Int)
              )
            , ( "fail with wrong argument types"
              , CanonicalU.Cons
                    (CanonicalU.List
                        [ CanonicalU.Int 1
                        , CanonicalU.Int 2
                        ]
                    )
                    (CanonicalU.List
                        [ CanonicalU.Int 3
                        , CanonicalU.Int 4
                        ]
                    )
              , Err
                    (TypeMismatch
                        (List Int)
                        Int
                    )
              )
            , ( "variable and list"
              , CanonicalU.Cons
                    (CanonicalU.Var { module_ = "Main", name = "age" })
                    (CanonicalU.List [ CanonicalU.Int 1 ])
              , Ok (List Int)
              )
            ]
        , runSection "record"
            [ ( "empty case"
              , CanonicalU.Record Dict.empty
              , Ok (Record Dict.empty)
              )
            , ( "one field"
              , CanonicalU.Record (Dict.fromList [ ( "a", { name = "a", body = CanonicalU.Int 42 } ) ])
              , Ok (Record <| Dict.fromList [ ( "a", Int ) ])
              )
            , ( "two fields"
              , CanonicalU.Record
                    (Dict.fromList
                        [ ( "a", { name = "a", body = CanonicalU.Int 42 } )
                        , ( "b", { name = "b", body = CanonicalU.String "hello" } )
                        ]
                    )
              , Ok (Record <| Dict.fromList [ ( "a", Int ), ( "b", String ) ])
              )
            ]
        ]


typeToString : Test
typeToString =
    let
        toStringOnce : Type -> String
        toStringOnce type_ =
            type_
                |> TypeToString.toString TypeToString.emptyState
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
                , List (Var 0)
                , "List a"
                )
            , runTest
                ( "one item in list"
                , List Bool
                , "List Bool"
                )
            , runTest
                ( "list of list of String"
                , List (List String)
                , "List (List String)"
                )
            ]
        , describe "lambda"
            [ runTest
                ( "function with one param"
                , Function (Var 99) Int
                , "a -> Int"
                )
            , runTest
                ( "function with two params"
                , Function (Var 0) (Function (Var 1) (Var 1))
                , "a -> b -> b"
                )
            , runTest
                ( "function as param"
                , Function (Function (Var 9) (Var 9)) (Var 0)
                , "(a -> a) -> b"
                )
            , runTest
                ( "list of functions"
                , List (Function (Var 0) (Var 0))
                , "List (a -> a)"
                )
            ]
        , describe "edges"
            [ runEqual
                ( "Var number doesn't count"
                , toStringOnce <| List (Var 0)
                , toStringOnce <| List (Var 1)
                )
            , runEqual
                ( "TypeMismatch types share vars index"
                , Error.toString
                    (TypeError
                        (TypeMismatch
                            (Function (List (Var 0)) (Var 1))
                            (Function (List (Var 1)) (Var 0))
                        )
                    )
                , "The types `(List a) -> b` and `(List b) -> a` don't match."
                )
            ]
        , describe "tuples"
            [ runTest
                ( "tuple with two literals"
                , Tuple Int String
                , "( Int, String )"
                )
            , runTest
                ( "tuple with two params"
                , Tuple (Var 0) (Var 1)
                , "( a, b )"
                )
            , runTest
                ( "tuple with tree params"
                , Tuple3 (Var 0) (Var 1) (Var 2)
                , "( a, b, c )"
                )
            ]
        , describe "user defined type"
            [ runTest
                ( "type without a param"
                , UserDefinedType
                    { module_ = "MyModule", name = "MyBool" }
                    []
                , "MyModule.MyBool"
                )
            , runTest
                ( "type with a param"
                , UserDefinedType
                    { module_ = "Maybe", name = "Maybe" }
                    [ Int ]
                , "Maybe.Maybe Int"
                )
            , runTest
                ( "type with a param 2"
                , UserDefinedType
                    { module_ = "Maybe", name = "Maybe" }
                    [ Var 0 ]
                , "Maybe.Maybe a"
                )
            ]
        , describe "records"
            [ runTest
                ( "empty record"
                , Record Dict.empty
                , "{}"
                )
            , runTest
                ( "one field record"
                , Record <| Dict.fromList [ ( "a", Int ) ]
                , "{ a : Int }"
                )
            , runTest
                ( "two fields record"
                , Record <| Dict.fromList [ ( "a", Int ), ( "b", String ) ]
                , "{ a : Int, b : String }"
                )
            ]
        ]


niceVarName : Test
niceVarName =
    let
        runTest ( input, output ) =
            test output <|
                \() ->
                    TypeToString.niceVarName input
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
            , ( Record Dict.empty, False )
            , ( Record <| Dict.fromList [ ( "a", Var 0 ), ( "b", String ) ], True )
            , ( Record <| Dict.fromList [ ( "a", Int ), ( "b", String ) ], False )
            ]
