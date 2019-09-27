module Elixir exposing (main)

{-| Usecase: Elixir

In other words, a compiler from Elm to another language.

Needs to thread the source code through the whole pipeline (parsing, desugaring,
type inference, optimizing) and then to its own emit function.

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
