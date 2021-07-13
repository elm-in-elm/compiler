module InferTypesTest exposing (isParametric, niceVarName, typeInference, typeToString)

import Dict
import Elm.AST.Canonical as Canonical
import Elm.AST.Canonical.Unwrapped as CanonicalU
import Elm.AST.Typed as Typed
import Elm.Compiler.Error as Error exposing (Error(..), TypeError(..))
import Elm.Data.Qualifiedness exposing (PossiblyQualified(..), Qualified(..))
import Elm.Data.Type as Type exposing (Type(..), TypeOrId(..))
import Elm.Data.Type.ToString as TypeToString
import Expect
import Stage.InferTypes
import Stage.InferTypes.Environment as Env
import Stage.InferTypes.SubstitutionMap as SubstitutionMap
import Test exposing (Test, describe, test)
import TestHelpers exposing (dumpTypeOrId)


typeInference : Test
typeInference =
    let
        runSection : String -> List ( String, CanonicalU.Expr, Result TypeError (Type Qualified) ) -> Test
        runSection description tests =
            describe description
                (List.map runTest tests)

        runTest : ( String, CanonicalU.Expr, Result TypeError (Type Qualified) ) -> Test
        runTest ( description, input, output ) =
            test description <|
                \() ->
                    input
                        |> Canonical.fromUnwrapped
                        |> Stage.InferTypes.inferExpr
                            Dict.empty
                            0
                            Env.empty
                            SubstitutionMap.empty
                        |> Result.map (Tuple.first >> Typed.getType)
                        |> Result.mapError Tuple.first
                        |> Expect.equal (Result.map Just output)
    in
    describe "Stage.InferTypesTest"
        [ runSection "list"
            [ ( "empty list"
              , CanonicalU.List []
              , Ok (List (Id 1))
              )
            , ( "one item"
              , CanonicalU.List [ CanonicalU.Bool True ]
              , Ok (List (Type Bool))
              )
            , ( "more items"
              , CanonicalU.List
                    [ CanonicalU.Int 1
                    , CanonicalU.Int 2
                    , CanonicalU.Int 3
                    ]
              , Ok (List (Type Int))
              )
            , ( "different types"
              , CanonicalU.List
                    [ CanonicalU.Int 1
                    , CanonicalU.String "two"
                    ]
              , Err (TypeMismatch (Type Int) (Type String))
              )
            , ( "more items with different types"
              , CanonicalU.List
                    [ CanonicalU.Bool True
                    , CanonicalU.String "two"
                    , CanonicalU.Int 3
                    ]
              , Err (TypeMismatch (Type Bool) (Type String))
              )
            , ( "List of List of Int"
              , CanonicalU.List
                    [ CanonicalU.List [ CanonicalU.Int 1 ]
                    , CanonicalU.List [ CanonicalU.Int 2 ]
                    ]
              , Ok (List (Type (List (Type Int))))
              )
            , ( "List of List of different types"
              , CanonicalU.List
                    [ CanonicalU.List [ CanonicalU.Int 1 ]
                    , CanonicalU.List [ CanonicalU.Bool False ]
                    ]
              , Err (TypeMismatch (Type Int) (Type Bool))
              )
            ]
        , runSection "tuple"
            [ ( "items with the same types"
              , CanonicalU.Tuple
                    (CanonicalU.String "Hello")
                    (CanonicalU.String "Elm")
              , Ok (Tuple (Type String) (Type String))
              )
            , ( "items of different types"
              , CanonicalU.Tuple
                    (CanonicalU.Bool True)
                    (CanonicalU.Int 1)
              , Ok (Tuple (Type Bool) (Type Int))
              )
            ]
        , runSection "tuple3"
            [ ( "same types"
              , CanonicalU.Tuple3
                    (CanonicalU.String "FP")
                    (CanonicalU.String "is")
                    (CanonicalU.String "good")
              , Ok (Tuple3 (Type String) (Type String) (Type String))
              )
            , ( "different types"
              , CanonicalU.Tuple3
                    (CanonicalU.Bool True)
                    (CanonicalU.Int 1)
                    (CanonicalU.Char 'h')
              , Ok (Tuple3 (Type Bool) (Type Int) (Type Char))
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
              , Ok (List (Type Int))
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
              , Ok (List (Type Int))
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
                        (Type (List (Type Int)))
                        (Type Int)
                    )
              )
            , ( "variable and list"
              , CanonicalU.Cons
                    (CanonicalU.Var { module_ = "Main", name = "age" })
                    (CanonicalU.List [ CanonicalU.Int 1 ])
              , Ok (List (Type Int))
              )
            ]
        , runSection "record"
            [ ( "empty case"
              , CanonicalU.Record Dict.empty
              , Ok (Record Dict.empty)
              )
            , ( "one field"
              , CanonicalU.Record (Dict.fromList [ ( "a", { name = "a", body = CanonicalU.Int 42 } ) ])
              , Ok (Record <| Dict.fromList [ ( "a", Type Int ) ])
              )
            , ( "two fields"
              , CanonicalU.Record
                    (Dict.fromList
                        [ ( "a", { name = "a", body = CanonicalU.Int 42 } )
                        , ( "b", { name = "b", body = CanonicalU.String "hello" } )
                        ]
                    )
              , Ok
                    (Record <|
                        Dict.fromList
                            [ ( "a", Type Int )
                            , ( "b", Type String )
                            ]
                    )
              )
            ]
        ]


typeToString : Test
typeToString =
    let
        toStringQualified : TypeOrId Qualified -> String
        toStringQualified typeOrId =
            typeOrId
                |> TypeToString.toString (TypeToString.fromTypeOrId typeOrId)
                |> Tuple.first

        toStringPossiblyQualified : TypeOrId PossiblyQualified -> String
        toStringPossiblyQualified typeOrId =
            typeOrId
                |> TypeToString.toStringPossiblyQualified (TypeToString.fromTypeOrId typeOrId)
                |> Tuple.first

        runTest : ( String, TypeOrId Qualified, String ) -> Test
        runTest ( description, input, output ) =
            test description <|
                \() ->
                    toStringQualified input
                        |> Expect.equal output

        runTest_ : ( String, TypeOrId PossiblyQualified, String ) -> Test
        runTest_ ( description, input, output ) =
            test description <|
                \() ->
                    toStringPossiblyQualified input
                        |> Expect.equal output

        runEqual ( description, input, output ) =
            test description <|
                \() ->
                    Expect.equal input output
    in
    describe "Type.toString"
        {- TODO test that strings given to `Id _` are different from strings
           given by `Type (TypeVar _)` (eg. if I have type annotation with `List a`
           and somehow in the larger context there's an `Id 0`, it doesn't
           collide with the `a` given by the `Var "a"` and instead is given `a0`
           or something similar.
        -}
        [ describe "list"
            [ runTest
                ( "empty list"
                , Type (List (Id 0))
                , "List a"
                )
            , runTest
                ( "one item in list"
                , Type (List (Type Bool))
                , "List Bool"
                )
            , runTest
                ( "list of list of String"
                , Type (List (Type (List (Type String))))
                , "List (List String)"
                )
            ]
        , describe "lambda"
            [ runTest
                ( "function with one param"
                , Type
                    (Function
                        { from = Id 99
                        , to = Type Int
                        }
                    )
                , "a -> Int"
                )
            , runTest
                ( "function with two params"
                , Type
                    (Function
                        { from = Id 0
                        , to =
                            Type
                                (Function
                                    { from = Id 1
                                    , to = Id 1
                                    }
                                )
                        }
                    )
                , "a -> b -> b"
                )
            , runTest
                ( "function with two params, but they're vars"
                , Type
                    (Function
                        { from = Type (TypeVar "a")
                        , to =
                            Type
                                (Function
                                    { from = Type (TypeVar "b")
                                    , to = Type (TypeVar "b")
                                    }
                                )
                        }
                    )
                , "a -> b -> b"
                )
            , runTest
                ( "function as param"
                , Type
                    (Function
                        { from =
                            Type
                                (Function
                                    { from = Id 9
                                    , to = Id 9
                                    }
                                )
                        , to = Id 0
                        }
                    )
                , "(a -> a) -> b"
                )
            , runTest
                ( "list of functions"
                , Type
                    (List
                        (Type
                            (Function
                                { from = Id 0
                                , to = Id 0
                                }
                            )
                        )
                    )
                , "List (a -> a)"
                )
            ]
        , describe "edge cases"
            [ runEqual
                ( "Var number doesn't count"
                , toStringQualified <| Type (List (Id 0))
                , toStringQualified <| Type (List (Id 1))
                )
            , runEqual
                ( "TypeMismatch types share vars index"
                , Error.toString
                    (TypeError
                        (TypeMismatch
                            (Type
                                (Function
                                    { from = Type (List (Id 0))
                                    , to = Id 1
                                    }
                                )
                            )
                            (Type
                                (Function
                                    { from = Type (List (Id 1))
                                    , to = Id 0
                                    }
                                )
                            )
                        )
                    )
                , "The types `(List a) -> b` and `(List b) -> a` don't match."
                )
            ]
        , describe "tuples"
            [ runTest
                ( "tuple with two literals"
                , Type (Tuple (Type Int) (Type String))
                , "( Int, String )"
                )
            , runTest
                ( "tuple with two params"
                , Type (Tuple (Id 0) (Id 1))
                , "( a, b )"
                )
            , runTest
                ( "3-tuple with three params"
                , Type (Tuple3 (Id 0) (Id 1) (Id 2))
                , "( a, b, c )"
                )
            ]
        , describe "user defined type"
            [ runTest
                ( "type without a param"
                , Type
                    (UserDefinedType
                        { qualifiedness = Qualified "MyModule"
                        , name = "MyBool"
                        , args = []
                        }
                    )
                , "MyModule.MyBool"
                )
            , runTest
                ( "type with a param"
                , Type
                    (UserDefinedType
                        { qualifiedness = Qualified "Maybe"
                        , name = "Maybe"
                        , args = [ Type Int ]
                        }
                    )
                , "Maybe.Maybe Int"
                )
            , runTest
                ( "type with a param 2"
                , Type
                    (UserDefinedType
                        { qualifiedness = Qualified "Maybe"
                        , name = "Maybe"
                        , args = [ Id 0 ]
                        }
                    )
                , "Maybe.Maybe a"
                )
            , runTest_
                ( "unqualified type"
                , Type
                    (UserDefinedType
                        { qualifiedness = PossiblyQualified Nothing
                        , name = "Maybe"
                        , args = [ Id 0 ]
                        }
                    )
                , "Maybe a"
                )
            ]
        , describe "records"
            [ runTest
                ( "empty record"
                , Type (Record Dict.empty)
                , "{}"
                )
            , runTest
                ( "one field record"
                , Type <| Record <| Dict.fromList [ ( "a", Type Int ) ]
                , "{ a : Int }"
                )
            , runTest
                ( "two fields record"
                , Type <|
                    Record <|
                        Dict.fromList
                            [ ( "a", Type Int )
                            , ( "b", Type String )
                            ]
                , "{ a : Int, b : String }"
                )
            , runTest
                ( "parametric field in record"
                , Type <| Record <| Dict.fromList [ ( "foo", Id 0 ) ]
                , "{ foo : a }"
                )
            ]
        , runTest
            ( "unit"
            , Type Unit
            , "()"
            )
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
        runTest : Int -> ( TypeOrId Qualified, Bool ) -> Test
        runTest i ( input, output ) =
            test (String.fromInt i ++ ": " ++ dumpTypeOrId input) <|
                \() ->
                    input
                        |> Type.isParametric
                        |> Expect.equal output
    in
    describe "Type.isParametric" <|
        List.indexedMap runTest
            [ ( Type Unit, False )
            , ( Type Bool, False )
            , ( Type Char, False )
            , ( Type Int, False )
            , ( Type String, False )
            , ( Type <| TypeVar "a", True )
            , ( Id 0, True )
            , ( Type <| Function { from = Type Int, to = Type String }, False )
            , ( Type <| Function { from = Type (TypeVar "bcd"), to = Type Int }, True )
            , ( Type <| Function { from = Id 102, to = Type Int }, True )
            , ( Type <| Function { from = Type String, to = Type (TypeVar "xx") }, True )
            , ( Type <| Function { from = Type String, to = Id 99 }, True )
            , ( Type <| Function { from = Type Int, to = Type (Function { from = Id 0, to = Id 0 }) }, True )
            , ( Type <| List (Type Int), False )
            , ( Type <| List (Id 0), True )
            , ( Type <| List (Type (TypeVar "ab")), True )
            , ( Type <| List (Type (List (Type Int))), False )
            , ( Type <| List (Type (List (Id 0))), True )
            , ( Type <| List (Type (List (Type (TypeVar "cd")))), True )
            , ( Type <| Tuple (Type Int) (Type String), False )
            , ( Type <| Tuple (Id 0) (Type Int), True )
            , ( Type <| Tuple (Type String) (Id 0), True )
            , ( Type <| Tuple (Type (List (Id 0))) (Type Int), True )
            , ( Type <| Tuple (Type Char) (Type (Tuple (Type Int) (Id 0))), True )
            , ( Type <| Tuple3 (Type Int) (Type String) (Type Bool), False )
            , ( Type <| Tuple3 (Id 0) (Type Int) (Type Char), True )
            , ( Type <| Tuple3 (Type String) (Id 0) (Type Unit), True )
            , ( Type <| Tuple3 (Type Bool) (Type Unit) (Id 0), True )
            , ( Type <| Tuple3 (Type (List (Id 0))) (Type Int) (Type Char), True )
            , ( Type <| Tuple3 (Type String) (Type (Function { from = Id 0, to = Type Int })) (Type Unit), True )
            , ( Type <| Tuple3 (Type Bool) (Type Unit) (Type (Tuple (Id 0) (Type Int))), True )
            , ( Type <| Record Dict.empty, False )
            , ( Type <| Record <| Dict.fromList [ ( "a", Id 0 ), ( "b", Type String ) ], True )
            , ( Type <| Record <| Dict.fromList [ ( "a", Type Int ), ( "b", Type String ) ], False )
            ]
