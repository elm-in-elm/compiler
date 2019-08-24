module Elm.Compiler exposing
    ( parseExpr, parseModule, parseModules, parseImport, parseDeclaration
    , desugarExpr, desugarModule, desugarModules
    , inferExpr, inferModule, inferModules
    , optimizeExpr, optimizeModule, optimizeModules
    , unwrapFrontendExpr, unwrapCanonicalExpr, unwrapTypedExpr
    , dropTypesExpr, dropTypesModule, dropTypesModules
    )

{-| High-level common usecases for using the compiler from Elm programs.

You can implement all of these (and change them up to suit your needs)
with the other functions exposed in this library!


# Parsing

**Useful eg. for tools like `elm-format`** that convert back to the Elm
representation, trying to do as few changes as possible. Ie. it would be bad
if it changed

    \a b c -> a + b + c

to

    \a -> \b -> \c -> a + b + c

That is one of the things the Desugar phase does. So tools like `elm-format`
probably won't want to touch that phase, and will want to only parse!

@docs parseExpr, parseModule, parseModules, parseImport, parseDeclaration


# Desugaring

TODO

@docs desugarExpr, desugarModule, desugarModules


# Inferring types

TODO

Note that the more of your code you'll give these functions at once, the better
the type inference will be. So it's advisable to eg. run `inferModules` once
instead of running `inferModule` on each of your modules.

@docs inferExpr, inferModule, inferModules


# Optimizing

TODO

@docs optimizeExpr, optimizeModule, optimizeModules


# Unwrapping expressions

During parsing all the expressions get the location info for where in the source
string they were parsed, and during later phases the location info sticks around.
This is handy for nice error messages, but not necessarily useful for all
applications.

If you don't need the location info, you can unwrap the underlying expressions
to get rid of it!

@docs unwrapFrontendExpr, unwrapCanonicalExpr, unwrapTypedExpr


# Dropping types

TODO

@docs dropTypesExpr, dropTypesModule, dropTypesModules

-}

import AST.Canonical as Canonical
import AST.Canonical.Unwrapped as CanonicalUnwrapped
import AST.Frontend as Frontend
import AST.Frontend.Unwrapped as FrontendUnwrapped
import AST.Typed as Typed
import AST.Typed.Unwrapped as TypedUnwrapped
import AssocList as Dict exposing (Dict)
import Data.Declaration exposing (Declaration)
import Data.FilePath as FilePath
import Data.Import exposing (Import)
import Data.Module as Module exposing (Module)
import Data.ModuleName exposing (ModuleName)
import Error
    exposing
        ( Error(..)
        , ParseContext
        , ParseError(..)
        , ParseProblem
        )
import OurExtras.AssocList as Dict
import Parser.Advanced as P
import Result.Extra as Result
import Stage.Desugar
import Stage.Desugar.Boilerplate
import Stage.InferTypes
import Stage.InferTypes.Boilerplate
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
parse : Parser a -> String -> Result Error a
parse parser string =
    P.run parser string
        |> Result.mapError (ParseError << ParseProblem)


{-| Parse a single expression like

    ( 12, "Hello" )

into AST like

    Located
        {start = ..., end = ...}
        (Tuple
            (Located ... (Literal (Int 12)))
            (Located ... (Literal (String "Hello")))
        )

If you don't need the location information and want to only keep the expressions,
use `unwrapFrontendExpr`.

-}
parseExpr : String -> Result Error Frontend.LocatedExpr
parseExpr sourceCode =
    parse Stage.Parse.Parser.expr sourceCode


{-| TODO
-}
parseModule : { filePath : String, sourceCode : String } -> Result Error (Module Frontend.LocatedExpr)
parseModule { filePath, sourceCode } =
    -- TODO maybe we can think of a way to not force the user to give us `filePath`?
    parse (Stage.Parse.Parser.module_ (FilePath.fromString filePath)) sourceCode


{-| TODO
-}
parseModules : List { filePath : String, sourceCode : String } -> Result Error (Dict ModuleName (Module Frontend.LocatedExpr))
parseModules files =
    {- TODO same as with `parseModule` - maybe we can think of a way to not force
       the user to give us `filePath`?
    -}
    files
        |> List.map parseModule
        |> Result.combine
        |> Result.map (List.map (\module_ -> ( module_.name, module_ )) >> Dict.fromList)


{-| TODO
-}
parseImport : String -> Result Error Import
parseImport sourceCode =
    parse Stage.Parse.Parser.import_ sourceCode


{-| TODO
-}
parseDeclaration : ModuleName -> String -> Result Error (Declaration Frontend.LocatedExpr)
parseDeclaration thisModule sourceCode =
    parse Stage.Parse.Parser.declaration sourceCode
        |> Result.map (\toDeclaration -> toDeclaration thisModule)



-- DESUGARING


{-| TODO
-}
desugarExpr :
    Dict ModuleName (Module Frontend.LocatedExpr)
    -> Module Frontend.LocatedExpr
    -> Frontend.LocatedExpr
    -> Result Error Canonical.LocatedExpr
desugarExpr modules thisModule locatedExpr =
    Stage.Desugar.desugarExpr modules thisModule locatedExpr
        |> Result.mapError DesugarError


{-| TODO
-}
desugarModule :
    Dict ModuleName (Module Frontend.LocatedExpr)
    -> Module Frontend.LocatedExpr
    -> Result Error (Module Canonical.LocatedExpr)
desugarModule modules thisModule =
    Stage.Desugar.Boilerplate.desugarModule (Stage.Desugar.desugarExpr modules) thisModule
        |> Result.mapError DesugarError


{-| TODO
-}
desugarModules : Dict ModuleName (Module Frontend.LocatedExpr) -> Result Error (Dict ModuleName (Module Canonical.LocatedExpr))
desugarModules modules =
    modules
        |> Dict.map (always (Stage.Desugar.Boilerplate.desugarModule (Stage.Desugar.desugarExpr modules)))
        |> Dict.combine
        |> Result.mapError DesugarError



-- TYPE INFERENCE


{-| TODO
-}
inferExpr : Canonical.LocatedExpr -> Result Error Typed.LocatedExpr
inferExpr locatedExpr =
    Stage.InferTypes.inferExpr locatedExpr
        |> Result.mapError TypeError


{-| TODO
-}
inferModule : Module Canonical.LocatedExpr -> Result Error (Module Typed.LocatedExpr)
inferModule thisModule =
    Stage.InferTypes.Boilerplate.inferModule Stage.InferTypes.inferExpr thisModule
        |> Result.mapError TypeError


{-| TODO
-}
inferModules : Dict ModuleName (Module Canonical.LocatedExpr) -> Result Error (Dict ModuleName (Module Typed.LocatedExpr))
inferModules modules =
    modules
        |> Dict.map (always inferModule)
        |> Dict.combine



-- OPTIMIZE


{-| TODO
-}
optimizeExpr : Typed.LocatedExpr -> Typed.LocatedExpr
optimizeExpr locatedExpr =
    Stage.Optimize.optimizeExpr locatedExpr


{-| TODO
-}
optimizeModule : Module Typed.LocatedExpr -> Module Typed.LocatedExpr
optimizeModule thisModule =
    Stage.Optimize.Boilerplate.optimizeModule optimizeExpr thisModule


{-| TODO
-}
optimizeModules : Dict ModuleName (Module Typed.LocatedExpr) -> Dict ModuleName (Module Typed.LocatedExpr)
optimizeModules modules =
    Dict.map (always optimizeModule) modules



-- UNWRAPPING


{-| Removes all the location info from the Frontend expressions, so eg.

    Located
        {start = ..., end = ...}
        (Tuple
            (Located ... (Literal (Int 12)))
            (Located ... (Literal (String "Hello")))
        )

becomes

    Tuple
        (Literal (Int 12))
        (Literal (String "Hello"))

-}
unwrapFrontendExpr : Frontend.LocatedExpr -> FrontendUnwrapped.Expr
unwrapFrontendExpr locatedExpr =
    Frontend.unwrap locatedExpr


{-| Removes all the location info from the Canonical expressions, so eg.

    Located
        {start = ..., end = ...}
        (Tuple
            (Located ... (Literal (Int 12)))
            (Located ... (Literal (String "Hello")))
        )

becomes

    Tuple
        (Literal (Int 12))
        (Literal (String "Hello"))

-}
unwrapCanonicalExpr : Canonical.LocatedExpr -> CanonicalUnwrapped.Expr
unwrapCanonicalExpr locatedExpr =
    Canonical.unwrap locatedExpr


{-| Removes all the location info from the Typed expressions, so eg.

    Located
        { start = ..., end = ... }
        ( Tuple
            (Located ... ( Literal (Int 12), Type.Int ))
            (Located ... ( Literal (String "Hello"), Type.String ))
        , Type.Tuple Type.Int Type.String
        )

becomes

    ( Tuple
        ( Literal (Int 12), Type.Int )
        ( Literal (String "Hello"), Type.String )
    , Type.Tuple Type.Int Type.String
    )

-}
unwrapTypedExpr : Typed.LocatedExpr -> TypedUnwrapped.Expr
unwrapTypedExpr locatedExpr =
    Typed.unwrap locatedExpr



-- DROP TYPES


{-| TODO
-}
dropTypesExpr : Typed.LocatedExpr -> Canonical.LocatedExpr
dropTypesExpr locatedExpr =
    Typed.dropTypes locatedExpr


{-| TODO
-}
dropTypesModule : Module Typed.LocatedExpr -> Module Canonical.LocatedExpr
dropTypesModule module_ =
    Module.map dropTypesExpr module_


{-| TODO
-}
dropTypesModules : Dict ModuleName (Module Typed.LocatedExpr) -> Dict ModuleName (Module Canonical.LocatedExpr)
dropTypesModules modules =
    Dict.map (always dropTypesModule) modules
