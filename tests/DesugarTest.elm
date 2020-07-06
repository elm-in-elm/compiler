module DesugarTest exposing (desugarTest)

import Dict
import Elm.AST.Canonical as Canonical
import Elm.AST.Canonical.Unwrapped as CanonicalU
import Elm.AST.Frontend as Frontend
import Elm.Compiler.Error as Error exposing (DesugarError)
import Elm.Data.Declaration as Declaration exposing (Declaration)
import Elm.Data.Exposing as Exposing
import Elm.Data.Import exposing (Import)
import Elm.Data.Located as Located
import Elm.Data.Module as Module exposing (Module)
import Elm.Data.ModuleName exposing (ModuleName)
import Elm.Data.Qualifiedness exposing (PossiblyQualified(..))
import Elm.Data.TypeAnnotation exposing (TypeAnnotation)
import Elm.Data.VarName exposing (VarName)
import Expect
import Stage.Desugar as Desugar
import Test exposing (Test, describe, test)
import TestHelpers exposing (located)


desugarTest : Test
desugarTest =
    describe "Stage.Desugar"
        [ test "desugar \\a b -> a + b into \\a -> \\b -> a + b " <|
            \_ ->
                frontendLambda "a" "b"
                    |> Desugar.desugarExpr Dict.empty (moduleFromName "A")
                    |> mapUnwrap
                    |> Expect.equal (Ok <| canonicalLambda "a" "b")
        , toTest
            { description = "desugar variable name in module"
            , thisModuleName = "A"
            , thisModuleVars = [ "a" ]
            , thisModuleImports = []
            , availableModules = []
            , inputVar = ( Nothing, "a" )
            , expectedResult = Ok ( "A", "a" )
            }
        , toTest
            { description = "desugar variable name NOT in this module"
            , thisModuleName = "A"
            , thisModuleVars = [ "a" ]
            , thisModuleImports = []
            , availableModules = []
            , inputVar = ( Nothing, "b" )
            , expectedResult =
                Err
                    (Error.VarNameNotFound
                        { insideModule = "A"
                        , var =
                            { qualifiedness = PossiblyQualified Nothing
                            , name = "b"
                            }
                        }
                    )
            }
        , toTest
            { description = "desugar prefixed variable name:  import B ; B.a"
            , thisModuleName = "A"
            , thisModuleVars = [ "a" ]
            , thisModuleImports = [ importFromName "B" ]
            , availableModules = [ { name = "B", exposedVars = [ "a" ] } ]
            , inputVar = ( Just "B", "a" )
            , expectedResult = Ok ( "B", "a" )
            }
        , toTest
            { description = "desugar prefixed variable name with aliased module:  import B as C; C.a"
            , thisModuleName = "A"
            , thisModuleVars = [ "a" ]
            , thisModuleImports = [ importFromName "B" |> as_ "C" ]
            , availableModules = [ { name = "B", exposedVars = [ "a" ] } ]
            , inputVar = ( Just "C", "a" )
            , expectedResult = Ok ( "B", "a" )
            }
        , toTest
            { description = "desugar exposed variable name: import B exposing (b); b"
            , thisModuleName = "A"
            , thisModuleVars = [ "a" ]
            , thisModuleImports = [ importFromName "B" |> exposingValuesInImport [ "b" ] ]
            , availableModules = [ { name = "B", exposedVars = [ "b" ] } ]
            , inputVar = ( Nothing, "b" )
            , expectedResult = Ok ( "B", "b" )
            }
        , toTest
            { description = "desugar variable name when exposed from import and defined in module: import B exposing (a); a = 42; a"
            , thisModuleName = "A"
            , thisModuleVars = [ "a" ]
            , thisModuleImports = [ importFromName "B" |> exposingValuesInImport [ "a" ] ]
            , availableModules = [ { name = "B", exposedVars = [ "a" ] } ]
            , inputVar = ( Nothing, "a" )
            , expectedResult = Ok ( "A", "a" )
            }
        , toTest
            { description = "desugar ambiguous variable names: import A as exposing (a); import B exposing (a) ; a"
            , thisModuleName = "A"
            , thisModuleVars = []
            , thisModuleImports =
                [ importFromName "B" |> exposingValuesInImport [ "a" ]
                , importFromName "C" |> exposingValuesInImport [ "a" ]
                ]
            , availableModules =
                [ { name = "B", exposedVars = [ "a" ] }
                , { name = "C", exposedVars = [ "a" ] }
                ]
            , inputVar = ( Nothing, "a" )
            , expectedResult =
                Err <|
                    Error.AmbiguousName
                        { name = "a"
                        , insideModule = "A"
                        , possibleModules = [ "B", "C" ]
                        }
            }
        , toTest
            { description = "referencing unknown variable"
            , thisModuleName = "A"
            , thisModuleVars = []
            , thisModuleImports = []
            , availableModules = []
            , inputVar = ( Nothing, "a" )
            , expectedResult =
                Err <|
                    Error.VarNameNotFound
                        { var =
                            { qualifiedness = PossiblyQualified Nothing
                            , name = "a"
                            }
                        , insideModule = "A"
                        }
            }
        , test "desugar duplicate record field" <|
            \_ ->
                let
                    aRegion =
                        { start = { row = 1, col = 1 }, end = { row = 2, col = 2 } }

                    bRegion =
                        { start = { row = 3, col = 3 }, end = { row = 4, col = 4 } }
                in
                [ { name = "aaa", body = Located.located aRegion Frontend.Unit }
                , { name = "aaa", body = Located.located bRegion Frontend.Unit }
                ]
                    |> Frontend.Record
                    |> located
                    |> Desugar.desugarExpr Dict.empty (moduleFromName "A")
                    |> mapUnwrap
                    |> Expect.equal
                        ({ name = "aaa"
                         , insideModule = "A"
                         , firstOccurrence = Located.located aRegion ()
                         , secondOccurrence = Located.located bRegion ()
                         }
                            |> Error.DuplicateRecordField
                            |> Err
                        )
        ]


{-| `frontendLambda "a" "b"` builds `\a b -> a + b`.
-}
frontendLambda : String -> String -> Frontend.LocatedExpr
frontendLambda arg1 arg2 =
    located <|
        Frontend.Lambda
            { arguments = [ arg1, arg2 ]
            , body =
                located <|
                    Frontend.Plus
                        (located <| Frontend.Argument arg1)
                        (located <| Frontend.Argument arg2)
            }


{-| `canonicalLambda "a" "b"` builds `\a -> \b -> a + b`.
-}
canonicalLambda : String -> String -> CanonicalU.Expr
canonicalLambda arg1 arg2 =
    CanonicalU.Lambda
        { argument = arg1
        , body =
            CanonicalU.Lambda
                { argument = arg2
                , body =
                    CanonicalU.Plus
                        (CanonicalU.Argument arg1)
                        (CanonicalU.Argument arg2)
                }
        }


type alias NameResolutionTestCase =
    { description : String
    , thisModuleName : ModuleName
    , thisModuleVars : List VarName
    , thisModuleImports : List ( ModuleName, Import )
    , availableModules : List { name : ModuleName, exposedVars : List VarName }
    , inputVar : ( Maybe ModuleName, VarName )
    , expectedResult : Result DesugarError ( ModuleName, VarName )
    }


toTest : NameResolutionTestCase -> Test
toTest r =
    test r.description <|
        \_ ->
            Desugar.desugarExpr
                (r.availableModules
                    |> List.map
                        (\m ->
                            ( m.name
                            , moduleFromName m.name
                                |> addDeclarations m.exposedVars
                                |> exposingValuesInModule m.exposedVars
                            )
                        )
                    |> Dict.fromList
                )
                (moduleFromName r.thisModuleName
                    |> addDeclarations r.thisModuleVars
                    |> addImports r.thisModuleImports
                )
                (var r.inputVar)
                |> mapUnwrap
                |> Expect.equal (r.expectedResult |> Result.map buildExpectedResult)


var : ( Maybe ModuleName, VarName ) -> Frontend.LocatedExpr
var ( maybeModuleName, varName ) =
    located <|
        Frontend.Var
            { qualifiedness = PossiblyQualified maybeModuleName
            , name = varName
            }


buildExpectedResult : ( ModuleName, VarName ) -> CanonicalU.Expr
buildExpectedResult ( moduleName, varName ) =
    CanonicalU.Var
        { module_ = moduleName
        , name = varName
        }



{- |
   module ModuleWithVarA exposing (a)

   a = 42
-}


importFromName : ModuleName -> ( ModuleName, Import )
importFromName moduleName =
    ( moduleName
    , { moduleName = moduleName
      , as_ = Nothing
      , exposing_ = Nothing
      }
    )


exposingValuesInImport : List VarName -> ( ModuleName, Import ) -> ( ModuleName, Import )
exposingValuesInImport vars ( moduleName, import_ ) =
    ( moduleName
    , { import_ | exposing_ = Just <| Exposing.ExposingSome <| List.map Exposing.ExposedValue vars }
    )


as_ : ModuleName -> ( ModuleName, Import ) -> ( ModuleName, Import )
as_ alias_ ( moduleName, import_ ) =
    ( moduleName, { import_ | as_ = Just alias_ } )


moduleFromName : ModuleName -> Module expr ann qual
moduleFromName name =
    { imports = Dict.empty
    , name = name
    , filePath = "/"
    , declarations = Dict.empty
    , type_ = Module.PlainModule
    , exposing_ = Exposing.ExposingSome []
    }



{- | add the following declaration to a module:
   a = 42
-}


addDeclaration :
    String
    -> Module Frontend.LocatedExpr TypeAnnotation PossiblyQualified
    -> Module Frontend.LocatedExpr TypeAnnotation PossiblyQualified
addDeclaration varName module_ =
    let
        decl : Declaration Frontend.LocatedExpr TypeAnnotation PossiblyQualified
        decl =
            { module_ = module_.name
            , name = varName
            , body =
                Declaration.Value
                    { typeAnnotation = Nothing
                    , expression = located <| Frontend.Int 42
                    }
            }
    in
    { module_ | declarations = Dict.insert varName decl module_.declarations }


addDeclarations :
    List String
    -> Module Frontend.LocatedExpr TypeAnnotation PossiblyQualified
    -> Module Frontend.LocatedExpr TypeAnnotation PossiblyQualified
addDeclarations varNames module_ =
    List.foldr addDeclaration module_ varNames



{- | add a list of exposed values to a module -}


exposingValuesInModule : List VarName -> Module expr ann qual -> Module expr ann qual
exposingValuesInModule varNames exposable =
    { exposable | exposing_ = Exposing.ExposingSome (List.map Exposing.ExposedValue varNames) }



{- | add an import to a module -}


addImport : ( ModuleName, Import ) -> Module expr ann qual -> Module expr ann qual
addImport ( moduleName, import_ ) module_ =
    { module_ | imports = Dict.insert moduleName import_ module_.imports }


addImports : List ( ModuleName, Import ) -> Module expr ann qual -> Module expr ann qual
addImports imports module_ =
    List.foldr addImport module_ imports


mapUnwrap : Result x Canonical.LocatedExpr -> Result x CanonicalU.Expr
mapUnwrap =
    Result.map Canonical.unwrap
