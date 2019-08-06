module UsecaseElixir exposing (main)

{-| Usecase: Elixir

In other words, a compiler from Elm to another language.

Needs to thread the source code through the whole pipeline (parsing, desugaring,
type inference, optimizing, emit), passing its own emit functions.

Gives us constraints on the API:

    parseModule : String -> Result Error (Module Frontend.Expr)
    desugarModule : Module Frontend.Expr -> Result Error (Module Canonical.Expr)
    inferModule : Module Canonical.Expr -> Result Error (Module Typed.Expr)
    optimizeModule : Module Typed.Expr -> Module Typed.Expr
    emitModuleWith : TODO -> Module Typed.Expr -> Result Error String

Unresolved questions:

    1. Multiple modules - better type errors etc. etc.
    2. Or doing them one-by-one but allowing looking at previously parsed modules?
    3. Is this combining of emit and "prepare for backend" a good idea? Would
       the project graph be useful for somebody?
    4. Do we want to emit types, comments etc.? Is it worth making them available
       to the emit function? (Folks might eventually create their own
       Backend.Expr types etc., so maybe make them available at least in the
       Typed phase?)

-}


main : String -> Result ElixirError String
main moduleSourceCode =
    moduleSourceCode
        |> Elm.Compiler.parseModule
        |> Random.andThen Elm.Compiler.desugarModule
        |> Random.andThen Elm.Compiler.inferModule
        |> Random.map Elm.Compiler.optimizeModule
        |> Random.andThen (Elm.Compiler.emitModuleWith ....TODO....)
