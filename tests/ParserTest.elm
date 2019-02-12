module ParserTest exposing
    ( dependencies
    , exposingList
    , moduleDeclaration
    )

import Common
import Common.Types
    exposing
        ( ExposedItem(..)
        , Exposing(..)
        , ModuleName(..)
        , ModuleType(..)
        )
import Dict.Any
import Expect
import Parser.Advanced as P
import Stage.Parse.Parser
import Test exposing (Test, describe, test)


moduleDeclaration : Test
moduleDeclaration =
    let
        runTest ( description, input, output ) =
            test description <|
                \() ->
                    input
                        |> P.run Stage.Parse.Parser.moduleDeclaration
                        |> Result.toMaybe
                        |> Expect.equal output
    in
    describe "Stage.Parse.Parser.moduleDeclaration"
        [ describe "general"
            (List.map runTest
                [ ( "works with simple module name"
                  , "module Foo exposing (..)"
                  , Just ( PlainModule, ModuleName "Foo", ExposingAll )
                  )
                , ( "works with nested module name"
                  , "module Foo.Bar exposing (..)"
                  , Just ( PlainModule, ModuleName "Foo.Bar", ExposingAll )
                  )
                , ( "works with even more nested module name"
                  , "module Foo.Bar.Baz.Quux exposing (..)"
                  , Just ( PlainModule, ModuleName "Foo.Bar.Baz.Quux", ExposingAll )
                  )
                , ( "allows multiple spaces between the `module` keyword and the module name"
                  , "module  Foo exposing (..)"
                  , Just ( PlainModule, ModuleName "Foo", ExposingAll )
                  )
                , ( "allows multiple spaces between the module name and the `exposing` keyword"
                  , "module Foo  exposing (..)"
                  , Just ( PlainModule, ModuleName "Foo", ExposingAll )
                  )
                , ( "allows a newline between the module name and the `exposing` keyword"
                  , "module Foo\nexposing (..)"
                  , Just ( PlainModule, ModuleName "Foo", ExposingAll )
                  )
                , ( "allows multiple spaces between the `exposing` keyword and the exposing list"
                  , "module Foo exposing  (..)"
                  , Just ( PlainModule, ModuleName "Foo", ExposingAll )
                  )
                , ( "allows a newline between the `exposing` keyword and the exposing list"
                  , "module Foo exposing\n(..)"
                  , Just ( PlainModule, ModuleName "Foo", ExposingAll )
                  )
                , ( "doesn't work without something after the `exposing` keyword"
                  , "module Foo exposing"
                  , Nothing
                  )
                ]
            )
        , describe "plain module"
            (List.map runTest
                [ ( "simply works"
                  , "module Foo exposing (..)"
                  , Just ( PlainModule, ModuleName "Foo", ExposingAll )
                  )
                ]
            )
        , describe "port module"
            (List.map runTest
                [ ( "simply works"
                  , "port module Foo exposing (..)"
                  , Just ( PortModule, ModuleName "Foo", ExposingAll )
                  )
                ]
            )

        -- TODO test effect module
        ]


exposingList : Test
exposingList =
    let
        runTest ( description, input, output ) =
            test description <|
                \() ->
                    input
                        |> P.run Stage.Parse.Parser.exposingList
                        |> Result.toMaybe
                        |> Expect.equal output
    in
    describe "Stage.Parse.Parser.exposingList"
        [ describe "exposing all"
            (List.map runTest
                [ ( "simply works"
                  , "(..)"
                  , Just ExposingAll
                  )
                , ( "doesn't work with spaces inside the parens"
                  , "( .. )"
                  , Nothing
                  )
                ]
            )
        , describe "exposing some"
            [ describe "general"
                (List.map runTest
                    [ ( "can't be empty"
                      , "()"
                      , Nothing
                      )
                    , ( "works with spaces between items"
                      , "(foo, bar)"
                      , Just
                            (ExposingSome
                                [ ExposedValue "foo"
                                , ExposedValue "bar"
                                ]
                            )
                      )
                    , ( "works with even more spaces between items"
                      , "(foo  ,  bar)"
                      , Just
                            (ExposingSome
                                [ ExposedValue "foo"
                                , ExposedValue "bar"
                                ]
                            )
                      )
                    , ( "works with mixed values"
                      , "(foo, Bar, Baz(..))"
                      , Just
                            (ExposingSome
                                [ ExposedValue "foo"
                                , ExposedType "Bar"
                                , ExposedTypeAndAllConstructors "Baz"
                                ]
                            )
                      )
                    , ( "allows for newline"
                      , "(foo\n,bar)"
                      , Just
                            (ExposingSome
                                [ ExposedValue "foo"
                                , ExposedValue "bar"
                                ]
                            )
                      )
                    ]
                )
            , describe "values"
                (List.map runTest
                    [ ( "works with a value"
                      , "(foo)"
                      , Just (ExposingSome [ ExposedValue "foo" ])
                      )
                    ]
                )
            , describe "types"
                (List.map runTest
                    [ ( "works with exposed type"
                      , "(Foo)"
                      , Just (ExposingSome [ ExposedType "Foo" ])
                      )
                    ]
                )
            , describe "types with all constructors"
                (List.map runTest
                    [ ( "works with exposed type and all constructors"
                      , "(Foo(..))"
                      , Just (ExposingSome [ ExposedTypeAndAllConstructors "Foo" ])
                      )
                    , ( "doesn't allow spaces between the module name and the double period list"
                      , "(Foo (..))"
                      , Nothing
                      )
                    , ( "doesn't allow spaces inside the double period list"
                      , "(Foo( .. ))"
                      , Nothing
                      )
                    , ( "doesn't allow only some constructors exposed"
                      , "(Foo(Bar))"
                      , Nothing
                      )
                    ]
                )
            ]
        ]


dependencies : Test
dependencies =
    let
        runTest ( description, input, output ) =
            test description <|
                \() ->
                    input
                        |> P.run Stage.Parse.Parser.dependencies
                        |> Result.toMaybe
                        |> Expect.equal output
    in
    describe "Stage.Parse.Parser.dependencies"
        [ describe "general"
            (List.map runTest
                [ ( "allows for multiple modifiers"
                  , "import Foo as F exposing (..)"
                  , Just
                        (Dict.Any.fromList Common.moduleNameToString
                            [ ( ModuleName "Foo"
                              , { moduleName = ModuleName "Foo"
                                , as_ = Just (ModuleName "F")
                                , exposing_ = Just ExposingAll
                                }
                              )
                            ]
                        )
                  )
                , ( "allows for multiple spaces"
                  , "import   Foo   as   F   exposing   (..)"
                  , Just
                        (Dict.Any.fromList Common.moduleNameToString
                            [ ( ModuleName "Foo"
                              , { moduleName = ModuleName "Foo"
                                , as_ = Just (ModuleName "F")
                                , exposing_ = Just ExposingAll
                                }
                              )
                            ]
                        )
                  )
                , ( "allows for multiple imports"
                  , "import Foo\nimport Bar"
                  , Just
                        (Dict.Any.fromList Common.moduleNameToString
                            [ ( ModuleName "Foo"
                              , { moduleName = ModuleName "Foo"
                                , as_ = Nothing
                                , exposing_ = Nothing
                                }
                              )
                            , ( ModuleName "Bar"
                              , { moduleName = ModuleName "Bar"
                                , as_ = Nothing
                                , exposing_ = Nothing
                                }
                              )
                            ]
                        )
                  )
                , ( "allows for multiple newlines between imports"
                  , "import Foo\n\nimport Bar"
                  , Just
                        (Dict.Any.fromList Common.moduleNameToString
                            [ ( ModuleName "Foo"
                              , { moduleName = ModuleName "Foo"
                                , as_ = Nothing
                                , exposing_ = Nothing
                                }
                              )
                            , ( ModuleName "Bar"
                              , { moduleName = ModuleName "Bar"
                                , as_ = Nothing
                                , exposing_ = Nothing
                                }
                              )
                            ]
                        )
                  )
                , ( "doesn't allow for lower-case import"
                  , "import foo"
                  , Nothing
                  )
                ]
            )
        , describe "simple"
            (List.map runTest
                [ ( "simply works"
                  , "import Foo"
                  , Just
                        (Dict.Any.fromList Common.moduleNameToString
                            [ ( ModuleName "Foo"
                              , { moduleName = ModuleName "Foo"
                                , as_ = Nothing
                                , exposing_ = Nothing
                                }
                              )
                            ]
                        )
                  )
                ]
            )
        , describe "as"
            (List.map runTest
                [ ( "simply works"
                  , "import Foo as F"
                  , Just
                        (Dict.Any.fromList Common.moduleNameToString
                            [ ( ModuleName "Foo"
                              , { moduleName = ModuleName "Foo"
                                , as_ = Just (ModuleName "F")
                                , exposing_ = Nothing
                                }
                              )
                            ]
                        )
                  )
                , ( "doesn't work with lowercase alias"
                  , "import Foo as f"
                  , Nothing
                  )
                , ( "doesn't work with dot-separated alias"
                  , "import Foo as X.B"
                  , Nothing
                  )
                ]
            )
        , describe "exposing"
            (List.map runTest
                [ ( "simply works"
                  , "import Foo exposing (bar, Baz, Quux(..))"
                  , Just
                        (Dict.Any.fromList Common.moduleNameToString
                            [ ( ModuleName "Foo"
                              , { moduleName = ModuleName "Foo"
                                , as_ = Nothing
                                , exposing_ =
                                    Just
                                        (ExposingSome
                                            [ ExposedValue "bar"
                                            , ExposedType "Baz"
                                            , ExposedTypeAndAllConstructors "Quux"
                                            ]
                                        )
                                }
                              )
                            ]
                        )
                  )
                , ( "doesn't work without something after the `exposing` keyword"
                  , "import Foo exposing"
                  , Nothing
                  )
                ]
            )
        ]



-- TODO test topLevelDeclarations
-- TODO test expr
