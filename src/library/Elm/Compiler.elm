module Elm.Compiler exposing
    ( parseExpr, parseTopLevelDeclarations, parseModule, parseProject
    , parseToCanonicalExpr, parseToCanonicalTopLevelDeclarations, parseToCanonicalModule, parseToCanonicalProject
    , parseToTypedExpr, parseToTypedTopLevelDeclarations, parseToTypedModule, parseToTypedProject
    , dropTypesExpr, dropTypesTopLevelDeclarations, dropTypesModule, dropTypesProject
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
probably won't want to call it!

@docs parseExpr, parseTopLevelDeclarations, parseModule, parseProject


# Parsing to canonical representation

In addition to parsing also performs desugaring. So the returned values contain
`Canonical.Expr`s instead of `Frontend.Expr`s.

**Useful if you need a cleaned-up version of the AST, eg. for some analysis.**

Note this representation doesn't contain the type information yet. For that,
look at the `parseToTyped*` family of functions.

@docs parseToCanonicalExpr, parseToCanonicalTopLevelDeclarations, parseToCanonicalModule, parseToCanonicalProject


# Parsing to typechecked representation

In addition to parsing and desugaring also infers and remembers the types
the parsed code.

**Useful if you need to do analysis that cares about types, or want to do
anything to the AST after checking the program typechecks.**

Note you can drop the type information with `dropTypes` if you want
typechecked AST without the types.

@docs parseToTypedExpr, parseToTypedTopLevelDeclarations, parseToTypedModule, parseToTypedProject


# Dropping types

Allows you to go from the typechecked representation back to the canonical one.

**Useful if your program needs the user program to be typechecked but you don't
care about the types otherwise.**

@docs dropTypesExpr, dropTypesTopLevelDeclarations, dropTypesModule, dropTypesProject

-}

import Dict.Any exposing (AnyDict)
import Elm.Compiler.AST.Canonical
import Elm.Compiler.AST.Frontend
import Elm.Compiler.AST.Typed
import Elm.Compiler.Error exposing (Error(..))
import Stage.Parser


parseExpr : String -> Result Error Frontend.Expr


parseTopLevelDeclarations : String -> Result Error (AnyDict String VarName (TopLevelDeclaration Frontend.Expr))


parseModule : String -> Result Error (Module Frontend.Expr)


parseProject : String -> Result Error (Project Frontend.Expr)


parseToCanonicalExpr : String -> Result Error Canonical.Expr


parseToCanonicalTopLevelDeclarations : String -> Result Error (AnyDict String VarName (TopLevelDeclaration Canonical.Expr))


parseToCanonicalModule : String -> Result Error (Module Canonical.Expr)


parseToCanonicalProject : String -> Result Error (Project Canonical.Expr)


parseToTypedExpr : String -> Result Error Typed.Expr


parseToTypedTopLevelDeclarations : String -> Result Error (AnyDict String VarName (TopLevelDeclaration Typed.Expr))


parseToTypedModule : String -> Result Error (Module Typed.Expr)


parseToTypedProject : String -> Result Error (Project Typed.Expr)


dropTypesExpr : Typed.Expr -> Canonical.Expr


dropTypesTopLevelDeclarations : AnyDict String VarName (TopLevelDeclaration Typed.Expr) -> AnyDict String VarName (TopLevelDeclaration Canonical.Expr)


dropTypesModule : Module Typed.Expr -> Module Canonical.Expr


dropTypesProject : Project Typed.Expr -> Project Canonical.Expr
