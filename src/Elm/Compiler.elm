module Elm.Compiler exposing
    ( parseExpr, parseModule, parseModules, parseImport, parseDeclaration
    , desugarExpr, desugarModule, desugarModules, desugarOnlyModule
    , inferExpr, inferModule, inferModules
    , defaultOptimizations
    , optimizeExpr, optimizeExprWith, optimizeModule, optimizeModuleWith, optimizeModules, optimizeModulesWith
    )

{-| Functions for working with Elm source code.

The compiler phases in general look like this:

![Stages of the compiler](https://github.com/elm-in-elm/compiler/raw/master/assets/stages.png)


# Parsing

**Useful eg. for tools like `elm-format`** that convert back to the Elm
representation, trying to do as few changes as possible. Ie. it would be bad
if `elm-format` changed

    \a b c -> a + b + c

to

    \a -> \b -> \c -> a + b + c

That transformation is one of the things the Desugar phase does. So tools like
`elm-format` probably don't want to touch that phase, and will only want to parse!

@docs parseExpr, parseModule, parseModules, parseImport, parseDeclaration


# Desugaring

After we parse the source code from a `String` to the AST, we desugar it -
simplify the AST type as much as possible to make later phases simpler and easier.

The best example to illustrate this (it doesn't actually happen though!) is
`let` vs `where`. Imagine if Elm allowed for `where` constructs in its syntax,
like Haskell does:

    foo = x + y
        where
            x = 123
            y = x + 2

Then we'd like to convert these to `let` constructs (or the other way round)
as soon as possible, so that the other phases don't need to handle two
almost identical scenarios all over the place.

Examples of real desugarings include:

  - making functions only take one argument,
  - fully qualify all variables
  - etc.

@docs desugarExpr, desugarModule, desugarModules, desugarOnlyModule


# Inferring types

These functions compute the [types](Elm.Data.Type) of the given expressions, as well as check them
against the user-defined type annotations.

Note that the more of your code you'll give these functions at once, the better
the type inference will be. So it's advisable to eg. run [`inferModules`](#inferModules)
once instead of running [`inferModule`](#inferModule) on each of your modules.

@docs inferExpr, inferModule, inferModules


# Optimizing

After typechecking the expressions are ready to be optimized. (The inferred types
are available to you inside the optimizations! (Unfortunately, the [location
information](Elm.Data.Located) is also available to you inside the optimization. Sorry.))

@docs defaultOptimizations

The optimizations are of the shape `Expr -> Maybe Expr`, and you **shouldn't
recurse** inside them. That's done for you automatically. You should only
consider one "layer", whatever that means for your optimization.

Eg. the `plus` optimization is (conceptually):

    optimizePlus : Expr -> Maybe Expr
    optimizePlus expr =
        case expr of
            Plus (Int a) (Int b) ->
                Just (Int (a + b))

            _ ->
                Nothing

Where it only is concerned with the addition of two integers. Any other
expression will just get ignored.

Your optimizations are then run from the bottom up, repeated as many times as
needed on the same expression until the optimization produces `Nothing`
("Nothing else I can do here!"). At that point, the engine moves "up" to the
parent expression, and so on until it reaches and optimizes the topmost
expression.

Note the optimizations in the list of optimizations are combined in such a way
that they take turns on the expression.

@docs optimizeExpr, optimizeExprWith, optimizeModule, optimizeModuleWith, optimizeModules, optimizeModulesWith


# Dropping types

If you want to typecheck the code but then don't do anything with the types
afterwards, you can drop them from the expressions you have. This is essentially
a move backwards in the compiler phases:

![Stages of the compiler](https://github.com/elm-in-elm/compiler/raw/master/assets/stages.png)

@docs dropTypesExpr, dropTypesModule, dropTypesModules

-}

import Dict exposing (Dict)
import Elm.AST.Canonical as Canonical
import Elm.AST.Frontend as Frontend
import Elm.AST.Typed as Typed
import Elm.Compiler.Error
    exposing
        ( Error(..)
        , ParseContext
        , ParseError(..)
        , ParseProblem
        )
import Elm.Data.Declaration exposing (Declaration)
import Elm.Data.FileContents exposing (FileContents)
import Elm.Data.FilePath exposing (FilePath)
import Elm.Data.Import exposing (Import)
import Elm.Data.Module exposing (Module)
import Elm.Data.ModuleName exposing (ModuleName)
import Elm.Data.Qualifiedness exposing (PossiblyQualified, Qualified)
import Elm.Data.Type exposing (Id)
import Elm.Data.Type.Concrete exposing (ConcreteType)
import Elm.Data.TypeAnnotation exposing (TypeAnnotation)
import Elm.Data.VarName exposing (VarName)
import OurExtras.Dict as Dict
import OurExtras.Tuple3 as Tuple3
import Parser.Advanced as P
import Result.Extra as Result
import Stage.Desugar
import Stage.Desugar.Boilerplate
import Stage.InferTypes
import Stage.InferTypes.Boilerplate
import Stage.InferTypes.Environment exposing (Environment)
import Stage.InferTypes.SubstitutionMap exposing (SubstitutionMap)
import Stage.Optimize
import Stage.Optimize.Boilerplate
import Stage.Parse.Parser



-- PARSING


{-| A shortcut so that the library users don't have to worry about
what ParseContext or ParseProblem means. Don't expose it.
-}
type alias Parser a =
    P.Parser ParseContext ParseProblem a


{-| A helper for a common pattern with our Elm parsers. Don't expose it.
-}
parse : Parser a -> FileContents -> Result Error a
parse parser sourceCode =
    Result.mapError
        (\errorList -> ParseError (ParseProblem ( errorList, sourceCode )))
        (P.run parser sourceCode)


{-| Parse a single expression like

    ( 12, "Hello" )

into AST like

    Located
        {start = ..., end = ...}
        (Tuple
            (Located ... (Int 12))
            (Located ... (String "Hello"))
        )

If you don't need the location information and want to only keep the expressions,
use [`Elm.AST.Frontend.unwrap`](Elm.AST.Frontend#unwrap) to get something like

    Tuple
        (Int 12)
        (String "Hello")

-}
parseExpr : FileContents -> Result Error Frontend.LocatedExpr
parseExpr sourceCode =
    parse Stage.Parse.Parser.expr sourceCode


{-| Parse a module (one `*.elm` file). Get a [`Module`](Elm.Data.Module#Module) datastructure back, holding
the information about its exposed values, imports, declarations and more.

A file like

    module Main exposing (foo)

    import Bar as B exposing (bar)

    foo =
        123

will get parsed into

    { imports =
        Dict.fromList
            [ ( "Bar"
              , { moduleName = "Bar"
                , as_ = Just "B"
                , exposing_ = Just (ExposingSome [ ExposedValue "bar" ])
                }
              )
            ]
    , name = "Foo"
    , filePath = "src/Foo.elm" -- what you pass into the function
    , declarations =
        Dict.fromList
            [ ( "foo"
              , { module_ = "Foo"
                , name = "foo"
                , body = Value (AST.Frontend.Int 123)
                }
              )
            ]
    , type_ = PlainModule
    , exposing_ = ExposingSome [ ExposedValue "foo" ]
    }

-}
parseModule :
    { filePath : FilePath, sourceCode : FileContents }
    -> Result Error (Module Frontend.LocatedExpr TypeAnnotation PossiblyQualified)
parseModule { filePath, sourceCode } =
    -- TODO maybe we can think of a way to not force the user to give us `filePath`?
    parse (Stage.Parse.Parser.module_ filePath) sourceCode


{-| Parse multiple modules (`*.elm` files) - see [`parseModule`](#parseModule) for details.
-}
parseModules :
    List { filePath : FilePath, sourceCode : FileContents }
    -> Result Error (Dict ModuleName (Module Frontend.LocatedExpr TypeAnnotation PossiblyQualified))
parseModules files =
    {- TODO same as with `parseModule` - maybe we can think of a way to not force
       the user to give us `filePath`?
    -}
    files
        |> List.map parseModule
        |> Result.combine
        |> Result.map (List.map (\module_ -> ( module_.name, module_ )) >> Dict.fromList)


{-| Parse a single import statement, like

    import Foo as F exposing
        ( foo
        , Bar(..)
        , Baz
        )

into

    { moduleName = "Foo"
    , as_ = Just "F"
    , exposing_ =
        Just
            (ExposingSome
                [ ExposedValue "foo"
                , ExposedTypeAndAllConstructors "Bar"
                , ExposedType "Baz"
                ]
            )
    }

-}
parseImport : FileContents -> Result Error Import
parseImport sourceCode =
    parse Stage.Parse.Parser.import_ sourceCode


{-| Parse a single declaration, like

    foo =
        123

into

    { module_ = "Foo" -- what you pass into the function
    , name = "foo"
    , body = Value (AST.Frontend.Int 123)
    }

-}
parseDeclaration :
    { moduleName : ModuleName, declaration : FileContents }
    -> Result Error (Declaration Frontend.LocatedExpr TypeAnnotation PossiblyQualified)
parseDeclaration { moduleName, declaration } =
    parse Stage.Parse.Parser.declaration declaration
        |> Result.map (\toDeclaration -> toDeclaration moduleName)



-- DESUGARING


{-| Desugar a single expression like

    Lambda
        { arguments = [ "x", "y" ]
        , body = AST.Frontend.Int 42
        }

into AST like

    Lambda
        { argument = "x"
        , body =
            Lambda
                { argument = "y"
                , body = AST.Canonical.Int 42
                }
        }

We're hitting limitations of the Elm Packages website, and the type shown isn't
very descriptive. **We're going from Frontend expressions to Canonical
expressions.**

-}
desugarExpr :
    Dict ModuleName (Module Frontend.LocatedExpr TypeAnnotation PossiblyQualified)
    -> Module Frontend.LocatedExpr TypeAnnotation PossiblyQualified
    -> Frontend.LocatedExpr
    -> Result Error Canonical.LocatedExpr
desugarExpr modules thisModule locatedExpr =
    Stage.Desugar.desugarExpr modules thisModule locatedExpr
        |> Result.mapError DesugarError


{-| Desugar a module (one `*.elm` file).

We're hitting limitations of the Elm Packages website, and the type shown isn't
very descriptive. **We're going from Frontend expressions to Canonical
expressions.**

-}
desugarModule :
    Dict ModuleName (Module Frontend.LocatedExpr TypeAnnotation PossiblyQualified)
    -> Module Frontend.LocatedExpr TypeAnnotation PossiblyQualified
    -> Result Error (Module Canonical.LocatedExpr (ConcreteType Qualified) Qualified)
desugarModule modules thisModule =
    Stage.Desugar.Boilerplate.desugarModule
        (Stage.Desugar.desugarExpr modules)
        (Stage.Desugar.desugarQualifiedness modules)
        (Stage.Desugar.desugarTypeAnnotation modules)
        thisModule
        |> Result.mapError DesugarError


{-| Desugar multiple modules (`*.elm` files) - see [`desugarModule`](#desugarModule) for details.

We're hitting limitations of the Elm Packages website, and the type shown isn't
very descriptive. **We're going from Frontend expressions to Canonical
expressions.**

-}
desugarModules :
    Dict ModuleName (Module Frontend.LocatedExpr TypeAnnotation PossiblyQualified)
    -> Result Error (Dict ModuleName (Module Canonical.LocatedExpr (ConcreteType Qualified) Qualified))
desugarModules modules =
    modules
        |> Dict.map
            (always
                (Stage.Desugar.Boilerplate.desugarModule
                    (Stage.Desugar.desugarExpr modules)
                    (Stage.Desugar.desugarQualifiedness modules)
                    (Stage.Desugar.desugarTypeAnnotation modules)
                )
            )
        |> Dict.combine
        |> Result.mapError DesugarError


{-| Desugar a module (one `*.elm` file), without the intention of desugaring
another one.

We're hitting limitations of the Elm Packages website, and the type shown isn't
very descriptive. **We're going from Frontend expressions to Canonical
expressions.**

-}
desugarOnlyModule :
    Module Frontend.LocatedExpr TypeAnnotation PossiblyQualified
    -> Result Error (Module Canonical.LocatedExpr (ConcreteType Qualified) Qualified)
desugarOnlyModule module_ =
    desugarModule
        (Dict.singleton module_.name module_)
        module_



-- TYPE INFERENCE


{-| Infer the types of a single expression.

We're hitting limitations of the Elm Packages website, and the type shown isn't
very descriptive. **The real type of this function is:**

    Canonical.LocatedExpr -> Result Error Typed.LocatedExpr

-}
inferExpr :
    Dict ( ModuleName, VarName ) (ConcreteType Qualified)
    -> Id
    -> Environment
    -> SubstitutionMap
    -> Canonical.LocatedExpr
    -> Result Error ( Typed.LocatedExpr, ( SubstitutionMap, Id, Environment ) )
inferExpr aliases unusedId env substitutionMap locatedExpr =
    Stage.InferTypes.inferExpr
        aliases
        unusedId
        env
        substitutionMap
        locatedExpr
        |> Result.mapError (Tuple.first >> TypeError)


{-| Infer the types of expressions in a module (a single `*.elm` file).

We're hitting limitations of the Elm Packages website, and the type shown isn't
very descriptive. **We're going from Canonical expressions to Typed expressions.**

-}
inferModule :
    Dict ( ModuleName, VarName ) (ConcreteType Qualified)
    -> Id
    -> Environment
    -> SubstitutionMap
    -> Module Canonical.LocatedExpr (ConcreteType Qualified) Qualified
    -> Result Error ( Module Typed.LocatedExpr Never Qualified, ( SubstitutionMap, Id, Environment ) )
inferModule aliases unusedId env substitutionMap thisModule =
    Stage.InferTypes.Boilerplate.inferModule
        Stage.InferTypes.inferExpr
        Stage.InferTypes.unifyWithTypeAnnotation
        aliases
        unusedId
        env
        substitutionMap
        thisModule
        |> Result.mapError (Tuple.first >> TypeError)


{-| Infer the types of expressions in multiple modules (`*.elm` files).

We're hitting limitations of the Elm Packages website, and the type shown isn't
very descriptive. **We're going from Canonical expressions to Typed expressions.**

-}
inferModules :
    Dict ( ModuleName, VarName ) (ConcreteType Qualified)
    -> Id
    -> Environment
    -> SubstitutionMap
    -> Dict ModuleName (Module Canonical.LocatedExpr (ConcreteType Qualified) Qualified)
    -> Result Error ( Dict ModuleName (Module Typed.LocatedExpr Never Qualified), ( SubstitutionMap, Id, Environment ) )
inferModules aliases unusedId env substitutionMap modules =
    modules
        |> Dict.foldl
            (\moduleName module_ acc ->
                acc
                    |> Result.andThen
                        (\( accDict, ( accSubstMap, accUnusedId, accEnv ) ) ->
                            inferModule
                                aliases
                                accUnusedId
                                accEnv
                                accSubstMap
                                module_
                                |> Result.map
                                    (Tuple.mapFirst
                                        (\newModule_ ->
                                            Dict.insert moduleName newModule_ accDict
                                        )
                                    )
                        )
            )
            (Ok ( Dict.empty, ( substitutionMap, unusedId, env ) ))



-- OPTIMIZE


{-| The default optimizations the elm-in-elm compiler uses.

    Try evaluating it in the REPL: you should see that each optimization function
    has a name String next to it.

        > import Elm.Compiler
        > Elm.Compiler.optimizations
        [ ("plus", ...)
        , ("cons", ...)
        , ("if-literal-bool", ...)
        ]

        >

    You can use this to filter optimizations you don't want!

       wantedOptimizations = Set.fromList [ "plus", "if-literal-bool ]

       optimizations
           |> List.filter (\(name, _) -> Set.member name wantedOptimizations)

-}
defaultOptimizations : List ( String, Typed.LocatedExpr -> Maybe Typed.LocatedExpr )
defaultOptimizations =
    Stage.Optimize.defaultOptimizations


{-| Optimize a given (typed) expression using the default set of optimizations.

For using your own optimizations instead of or in addition to the default ones,
look at the [`optimizeExprWith`](#optimizeExprWith) function.

-}
optimizeExpr : Typed.LocatedExpr -> Typed.LocatedExpr
optimizeExpr locatedExpr =
    Stage.Optimize.optimizeExpr locatedExpr


{-| Optimize a given (typed) expression using a custom set of optimizations.
-}
optimizeExprWith :
    List ( String, Typed.LocatedExpr -> Maybe Typed.LocatedExpr )
    -> Typed.LocatedExpr
    -> Typed.LocatedExpr
optimizeExprWith optimizations locatedExpr =
    Stage.Optimize.optimizeExprWith optimizations locatedExpr


{-| Optimize all expressions in a given module using the default set of
optimizations.

Note there is currently no inter-definition optimizations (inlining etc.) -
only the optimizations on each separate expression.

For using your own optimizations instead of or in addition to the default ones,
look at the [`optimizeModuleWith`](#optimizeModuleWith) function.

-}
optimizeModule :
    Module Typed.LocatedExpr Never Qualified
    -> Module Typed.LocatedExpr Never Qualified
optimizeModule thisModule =
    Stage.Optimize.Boilerplate.optimizeModule optimizeExpr thisModule


{-| Optimize all expressions in a given module using a custom set of
optimizations.

Note there is currently no inter-definition optimizations (inlining etc.) -
only the optimizations on each separate expression.

-}
optimizeModuleWith :
    List ( String, Typed.LocatedExpr -> Maybe Typed.LocatedExpr )
    -> Module Typed.LocatedExpr Never Qualified
    -> Module Typed.LocatedExpr Never Qualified
optimizeModuleWith optimizations thisModule =
    Stage.Optimize.Boilerplate.optimizeModule (optimizeExprWith optimizations) thisModule


{-| Optimize all expressions in multiple modules using the default set of
optimizations.

For using your own optimizations instead of or in addition to the default ones,
look at the [`optimizeModulesWith`](#optimizeModulesWith) function.

-}
optimizeModules :
    Dict ModuleName (Module Typed.LocatedExpr Never Qualified)
    -> Dict ModuleName (Module Typed.LocatedExpr Never Qualified)
optimizeModules modules =
    Dict.map (always optimizeModule) modules


{-| Optimize all expressions in multiple modules using a custom set of
optimizations.
-}
optimizeModulesWith :
    List ( String, Typed.LocatedExpr -> Maybe Typed.LocatedExpr )
    -> Dict ModuleName (Module Typed.LocatedExpr Never Qualified)
    -> Dict ModuleName (Module Typed.LocatedExpr Never Qualified)
optimizeModulesWith optimizations modules =
    Dict.map (always (optimizeModuleWith optimizations)) modules
