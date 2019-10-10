module Elm.Compiler exposing
    ( parseExpr, parseModule, parseModules, parseImport, parseDeclaration
    , desugarExpr, desugarModule, desugarModules, desugarOnlyModule
    , inferExpr, inferModule, inferModules
    , defaultOptimizations
    , optimizeExpr, optimizeExprWith, optimizeModule, optimizeModuleWith, optimizeModules, optimizeModulesWith
    , dropTypesExpr, dropTypesModule, dropTypesModules
    )

{-| Functions for working with Elm source code.

> Because of package.elm-lang.org not displaying qualifiers correctly in type
> annotations, we're forced to use aliases for the various `Elm.AST.*` types.
> So when you see `FrontendLocatedExpr`, look at [`Elm.AST.Frontend.LocatedExpr`](Elm.AST.Frontend#LocatedExpr).

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
import Elm.AST.Canonical.Unwrapped as CanonicalUnwrapped
import Elm.AST.Frontend as Frontend
import Elm.AST.Frontend.Unwrapped as FrontendUnwrapped
import Elm.AST.Typed as Typed
import Elm.AST.Typed.Unwrapped as TypedUnwrapped
import Elm.Compiler.Error
    exposing
        ( Error(..)
        , ParseContext
        , ParseError(..)
        , ParseProblem
        )
import Elm.Compiler.Stage.Desugar as Desugar
import Elm.Compiler.Stage.Desugar.Boilerplate as DesugarB
import Elm.Compiler.Stage.InferTypes as InferTypes
import Elm.Compiler.Stage.InferTypes.Boilerplate as InferTypesB
import Elm.Compiler.Stage.Optimize as Optimize
import Elm.Compiler.Stage.Optimize.Boilerplate as OptimizeB
import Elm.Compiler.Stage.Parse.Parser as Parser
import Elm.Data.Declaration exposing (Declaration)
import Elm.Data.FileContents exposing (FileContents)
import Elm.Data.FilePath exposing (FilePath)
import Elm.Data.Import exposing (Import)
import Elm.Data.Module as Module exposing (Module)
import Elm.Data.ModuleName exposing (ModuleName)
import OurExtras.Dict as Dict
import Parser.Advanced as P
import Result.Extra as Result



-- PARSING


{-| A shortcut so that the library users don't have to worry about
what ParseContext or ParseProblem means. Don't expose it.
-}
type alias Parser a =
    P.Parser ParseContext ParseProblem a


{-| A helper for a common pattern with our Elm parsers. Don't expose it.
-}
parse : Parser a -> FileContents -> Result Error a
parse parser string =
    P.run parser string
        |> Result.mapError (ParseError << ParseProblem)


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
parseExpr : FileContents -> Result Error FrontendLocatedExpr
parseExpr sourceCode =
    parse Parser.expr sourceCode


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
    -> Result Error (Module FrontendLocatedExpr)
parseModule { filePath, sourceCode } =
    -- TODO maybe we can think of a way to not force the user to give us `filePath`?
    parse (Parser.module_ filePath) sourceCode


{-| Parse multiple modules (`*.elm` files) - see [`parseModule`](#parseModule) for details.
-}
parseModules :
    List { filePath : FilePath, sourceCode : FileContents }
    -> Result Error (Dict ModuleName (Module FrontendLocatedExpr))
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
    parse Parser.import_ sourceCode


{-| Parse a single declaration, like

    foo =
        123

into

    { module_ = "Foo" -- what you pass into the function
    , name = "foo"
    , body = Value (AST.Frontend.Int 123)
    }

-}
parseDeclaration : { moduleName : ModuleName, declaration : FileContents } -> Result Error (Declaration FrontendLocatedExpr)
parseDeclaration { moduleName, declaration } =
    parse Parser.declaration declaration
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

-}
desugarExpr :
    Dict ModuleName (Module FrontendLocatedExpr)
    -> Module FrontendLocatedExpr
    -> FrontendLocatedExpr
    -> Result Error CanonicalLocatedExpr
desugarExpr modules thisModule locatedExpr =
    Desugar.desugarExpr modules thisModule locatedExpr
        |> Result.mapError DesugarError


{-| Desugar a module (one `*.elm` file).
-}
desugarModule :
    Dict ModuleName (Module FrontendLocatedExpr)
    -> Module FrontendLocatedExpr
    -> Result Error (Module CanonicalLocatedExpr)
desugarModule modules thisModule =
    DesugarB.desugarModule (Desugar.desugarExpr modules) thisModule
        |> Result.mapError DesugarError


{-| Desugar multiple modules (`*.elm` files) - see [`desugarModule`](#desugarModule) for details.
-}
desugarModules :
    Dict ModuleName (Module FrontendLocatedExpr)
    -> Result Error (Dict ModuleName (Module CanonicalLocatedExpr))
desugarModules modules =
    modules
        |> Dict.map (always (DesugarB.desugarModule (Desugar.desugarExpr modules)))
        |> Dict.combine
        |> Result.mapError DesugarError


{-| Desugar a module (one `*.elm` file), without the intention of desugaring
another one.
-}
desugarOnlyModule :
    Module FrontendLocatedExpr
    -> Result Error (Module CanonicalLocatedExpr)
desugarOnlyModule module_ =
    desugarModule
        (Dict.singleton module_.name module_)
        module_



-- TYPE INFERENCE


{-| Infer the types of a single expression.
-}
inferExpr : CanonicalLocatedExpr -> Result Error TypedLocatedExpr
inferExpr locatedExpr =
    InferTypes.inferExpr locatedExpr
        |> Result.mapError TypeError


{-| Infer the types of expressions in a module (a single `*.elm` file).
-}
inferModule :
    Module CanonicalLocatedExpr
    -> Result Error (Module TypedLocatedExpr)
inferModule thisModule =
    InferTypesB.inferModule InferTypes.inferExpr thisModule
        |> Result.mapError TypeError


{-| Infer the types of expressions in multiple modules (`*.elm` files).
-}
inferModules :
    Dict ModuleName (Module CanonicalLocatedExpr)
    -> Result Error (Dict ModuleName (Module TypedLocatedExpr))
inferModules modules =
    modules
        |> Dict.map (always inferModule)
        |> Dict.combine



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
defaultOptimizations : List ( String, TypedLocatedExpr -> Maybe TypedLocatedExpr )
defaultOptimizations =
    Optimize.defaultOptimizations


{-| Optimize a given (typed) expression using the default set of optimizations.

For using your own optimizations instead of or in addition to the default ones,
look at the [`optimizeExprWith`](#optimizeExprWith) function.

-}
optimizeExpr : TypedLocatedExpr -> TypedLocatedExpr
optimizeExpr locatedExpr =
    Optimize.optimizeExpr locatedExpr


{-| Optimize a given (typed) expression using a custom set of optimizations.
-}
optimizeExprWith :
    List ( String, TypedLocatedExpr -> Maybe TypedLocatedExpr )
    -> TypedLocatedExpr
    -> TypedLocatedExpr
optimizeExprWith optimizations locatedExpr =
    Optimize.optimizeExprWith optimizations locatedExpr


{-| Optimize all expressions in a given module using the default set of
optimizations.

Note there is currently no inter-definition optimizations (inlining etc.) -
only the optimizations on each separate expression.

For using your own optimizations instead of or in addition to the default ones,
look at the [`optimizeModuleWith`](#optimizeModuleWith) function.

-}
optimizeModule : Module TypedLocatedExpr -> Module TypedLocatedExpr
optimizeModule thisModule =
    OptimizeB.optimizeModule optimizeExpr thisModule


{-| Optimize all expressions in a given module using a custom set of
optimizations.

Note there is currently no inter-definition optimizations (inlining etc.) -
only the optimizations on each separate expression.

-}
optimizeModuleWith :
    List ( String, TypedLocatedExpr -> Maybe TypedLocatedExpr )
    -> Module TypedLocatedExpr
    -> Module TypedLocatedExpr
optimizeModuleWith optimizations thisModule =
    OptimizeB.optimizeModule (optimizeExprWith optimizations) thisModule


{-| Optimize all expressions in multiple modules using the default set of
optimizations.

For using your own optimizations instead of or in addition to the default ones,
look at the [`optimizeModulesWith`](#optimizeModulesWith) function.

-}
optimizeModules :
    Dict ModuleName (Module TypedLocatedExpr)
    -> Dict ModuleName (Module TypedLocatedExpr)
optimizeModules modules =
    Dict.map (always optimizeModule) modules


{-| Optimize all expressions in multiple modules using a custom set of
optimizations.
-}
optimizeModulesWith :
    List ( String, TypedLocatedExpr -> Maybe TypedLocatedExpr )
    -> Dict ModuleName (Module TypedLocatedExpr)
    -> Dict ModuleName (Module TypedLocatedExpr)
optimizeModulesWith optimizations modules =
    Dict.map (always (optimizeModuleWith optimizations)) modules



-- DROP TYPES


{-| Drop types from a single expression.

Example usage:

    Located
        { start = ..., end = ... }
        ( Tuple
            (Located ... ( Int 12, Type.Int ))
            (Located ... ( String "Hello", Type.String ))
        , Type.Tuple Type.Int Type.String
        )

becomes

    Located
        { start = ..., end = ... }
        (Tuple
            (Located ... (Int 12)
            (Located ... (String "Hello")
        )

If location info is not useful to you either, look for the `unwrap` functions
in the various `Elm.AST.*` modules.

-}
dropTypesExpr : TypedLocatedExpr -> CanonicalLocatedExpr
dropTypesExpr locatedExpr =
    Typed.dropTypes locatedExpr


{-| Drop types from all expressions in the module.
-}
dropTypesModule : Module TypedLocatedExpr -> Module CanonicalLocatedExpr
dropTypesModule module_ =
    Module.map dropTypesExpr module_


{-| Drop types from all expressions in all the modules.
-}
dropTypesModules :
    Dict ModuleName (Module TypedLocatedExpr)
    -> Dict ModuleName (Module CanonicalLocatedExpr)
dropTypesModules modules =
    Dict.map (always dropTypesModule) modules


type alias FrontendLocatedExpr =
    Frontend.LocatedExpr


type alias CanonicalLocatedExpr =
    Canonical.LocatedExpr


type alias TypedLocatedExpr =
    Typed.LocatedExpr
