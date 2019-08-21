module UsecaseElixir exposing (main)

{-| Usecase: Elixir

In other words, a compiler from Elm to another language.

Needs to thread the source code through the whole pipeline (parsing, desugaring,
type inference, optimizing) and then to its own emit function.

Gives us constraints on the API:

    parseModule : String -> Result Error (Module Frontend.LocatedExpr)

    desugarModule : Module Frontend.LocatedExpr -> Result Error (Module Canonical.LocatedExpr)

    inferModule : Module Canonical.LocatedExpr -> Result Error (Module Typed.LocatedExpr)

    optimizeModule : Module Typed.LocatedExpr -> Module Typed.LocatedExpr

    TODO refactor PrepareForBackend phase into emit helpers (toGraph, find deps of ...), use in JS emit

Unresolved questions:

    1. Multiple modules - better type errors etc... Probably needs combining
       multiple modules into some kind of environment, typechecking *that* and
       then getting the modules back out of it?
    2. We're letting user emit the Module however they want. Should we expose the
       "preparing for backend" stage? It is only about finding the dependency
       path from `main` to whatever it needs, so maybe we should rename it to say
       that? I suspect some languages would benefit from it and some wouldn't,
       so yeah maybe let's expose it...
    3. Or just expose some kind of helper for getting the path from arbitrary
       value to whatever it needs?

-}


main : String -> Result Error String
main moduleSourceCode =
    moduleSourceCode
        |> Elm.Compiler.parseModule
        |> Result.andThen Elm.Compiler.desugarModule
        |> Result.andThen Elm.Compiler.inferModule
        |> Result.map Elm.Compiler.optimizeModule
        |> Result.map emitElixirModule


emitElixirModule : Module Typed.LocatedExpr -> String
emitElixirModule =
    -- TODO maybe flesh this example out a bit to be sure this is feasible?
    Debug.todo "whatever"
