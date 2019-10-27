module DesugarTest exposing (desugarTest)

import Dict exposing (Dict)
import Elm.AST.Canonical as Canonical
import Elm.AST.Canonical.Unwrapped as CanonicalU
import Elm.AST.Frontend as Frontend
import Elm.Compiler.Error as CompilerError
import Elm.Data.Declaration as Declaration exposing (Declaration)
import Elm.Data.Exposing as Exposing
import Elm.Data.Import exposing (Import)
import Elm.Data.Module as Module exposing (Module)
import Elm.Data.ModuleName exposing (ModuleName)
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
                    |> Desugar.desugarExpr Dict.empty dummyModule
                    |> mapUnwrap
                    |> Expect.equal (Ok <| canonicalLambda "a" "b")
        , test "desugar variable name in module" <|
            \_ ->
                Desugar.desugarExpr Dict.empty moduleWithVarA varANotPrefixed
                    |> mapUnwrap
                    |> Expect.equal (Ok <| CanonicalU.Var { module_ = moduleWithVarA.name, name = "a" })
        , test "desugar variable  name NOT in this module" <|
            \_ ->
                Desugar.desugarExpr Dict.empty dummyModule varANotPrefixed
                    |> Expect.equal
                        (Err
                            (CompilerError.VarNameNotFound
                                { insideModule = dummyModule.name, var = { module_ = Nothing, name = "a" } }
                            )
                        )
        , test "desugar prefixed variable name: import A ; A.a" <|
            \_ ->
                Desugar.desugarExpr
                    moduleWithVarAInDict
                    (dummyModule |> addImport moduleWithVarA.name importModuleWithVarA)
                    (varAPrefixedBy moduleWithVarA.name)
                    |> mapUnwrap
                    |> Expect.equal
                        (Ok <| CanonicalU.Var { module_ = moduleWithVarA.name, name = "a" })
        , test "desugar prefixed aliased variable name: import A as B; B.a" <|
            \_ ->
                Desugar.desugarExpr
                    moduleWithVarAInDict
                    (dummyModule |> addImport moduleWithVarA.name importModuleWithVarAAsB)
                    (varAPrefixedBy "B")
                    |> mapUnwrap
                    |> Expect.equal
                        (Ok <| CanonicalU.Var { module_ = moduleWithVarA.name, name = "a" })
        , test "desugar exposed variable name: import A exposing (a); a" <|
            \_ ->
                Desugar.desugarExpr
                    moduleWithVarAInDict
                    (dummyModule |> addImport moduleWithVarA.name importModuleWithVarAExposingA)
                    varANotPrefixed
                    |> mapUnwrap
                    |> Expect.equal
                        (Ok <| CanonicalU.Var { module_ = moduleWithVarA.name, name = "a" })
        , test "desugar declare already imported variable: import A exposing (a); a = 42" <|
            \_ ->
                Desugar.desugarExpr
                    moduleWithVarAInDict
                    (dummyModule
                        |> addImport moduleWithVarA.name importModuleWithVarAExposingA
                        |> addDeclarationA
                    )
                    varANotPrefixed
                    |> mapUnwrap
                    |> Expect.equal
                        (Ok <| CanonicalU.Var { module_ = dummyModule.name, name = "a" })
        , test "desugar ambiguous variable names: import A as exposing (a); import B exposing (a) " <|
            \_ ->
                Desugar.desugarExpr
                    modulesAAndABisInDict
                    (dummyModule
                        |> addImport moduleWithVarA.name importModuleWithVarAExposingA
                        |> addImport moduleWithVarABis.name importModuleWithVarABisExposingA
                    )
                    varANotPrefixed
                    |> mapUnwrap
                    |> Expect.equal
                        (Err <|
                            CompilerError.AmbiguousName
                                { name = "a"
                                , insideModule = dummyModule.name
                                , possibleModules = [ moduleWithVarA.name, moduleWithVarABis.name ]
                                }
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


moduleWithVarAInDict : Dict ModuleName (Module Frontend.LocatedExpr)
moduleWithVarAInDict =
    Dict.fromList [ ( moduleWithVarA.name, moduleWithVarA ) ]


modulesAAndABisInDict : Dict ModuleName (Module Frontend.LocatedExpr)
modulesAAndABisInDict =
    Dict.fromList
        [ ( moduleWithVarA.name, moduleWithVarA )
        , ( moduleWithVarABis.name, moduleWithVarABis )
        ]


varANotPrefixed : Frontend.LocatedExpr
varANotPrefixed =
    located <| Frontend.Var { module_ = Nothing, name = "a" }


varAPrefixedBy : String -> Frontend.LocatedExpr
varAPrefixedBy moduleName =
    located <| Frontend.Var { module_ = Just moduleName, name = "a" }



{- |
   module ModuleWithVarA exposing (a)

   a = 42
-}


moduleWithVarA : Module Frontend.LocatedExpr
moduleWithVarA =
    dummyModule
        |> withName "ModuleWithVarA"
        |> addDeclarationA
        |> withExposingSomeValues [ "a" ]



{- |
   module ModuleWithVarABis exposing (a)

   a = 42
-}


moduleWithVarABis : Module Frontend.LocatedExpr
moduleWithVarABis =
    dummyModule
        |> withName "ModuleWithVarABis"
        |> addDeclarationA
        |> withExposingSomeValues [ "a" ]



{- | import ModuleWithVarA -}


importModuleWithVarA : Import
importModuleWithVarA =
    { moduleName = moduleWithVarA.name
    , as_ = Nothing
    , exposing_ = Nothing
    }



{- | import ModuleWithVarA  exposing (a) -}


importModuleWithVarAExposingA : Import
importModuleWithVarAExposingA =
    { importModuleWithVarA
        | exposing_ = Just <| Exposing.ExposingSome [ Exposing.ExposedValue "a" ]
    }



{- | import ModuleWithVarABis  exposing (a) -}


importModuleWithVarABisExposingA : Import
importModuleWithVarABisExposingA =
    { moduleName = moduleWithVarABis.name
    , as_ = Nothing
    , exposing_ = Just <| Exposing.ExposingSome [ Exposing.ExposedValue "a" ]
    }



{- | import ModuleWithVarA as B  exposing (a) -}


importModuleWithVarAAsB : Import
importModuleWithVarAAsB =
    { importModuleWithVarA | as_ = Just "B" }



{- |
   module DummyModule
-}


dummyModule : Module a
dummyModule =
    { imports = Dict.empty
    , name = "DummyModule"
    , filePath = "/"
    , declarations = Dict.empty
    , type_ = Module.PlainModule
    , exposing_ = Exposing.ExposingSome []
    }



{- | change the name of a module -}


withName : String -> Module a -> Module a
withName name module_ =
    { module_ | name = name }



{- | add the following declaration to a module:
   a = 42
-}


addDeclarationA : Module Frontend.LocatedExpr -> Module Frontend.LocatedExpr
addDeclarationA module_ =
    let
        decl : Declaration Frontend.LocatedExpr
        decl =
            { module_ = module_.name
            , name = "a"
            , body = Declaration.Value (located <| Frontend.Int 42)
            }
    in
    { module_ | declarations = Dict.insert "a" decl module_.declarations }



{- | add a list of exposed values to a module -}


withExposingSomeValues : List VarName -> Module a -> Module a
withExposingSomeValues varNames exposable =
    { exposable | exposing_ = Exposing.ExposingSome (List.map Exposing.ExposedValue varNames) }



{- | add an import to a module -}


addImport : ModuleName -> Import -> Module a -> Module a
addImport moduleName import_ module_ =
    { module_ | imports = Dict.insert moduleName import_ module_.imports }


mapUnwrap : Result x Canonical.LocatedExpr -> Result x CanonicalU.Expr
mapUnwrap =
    Result.map Canonical.unwrap
