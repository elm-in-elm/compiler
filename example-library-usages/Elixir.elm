module Elixir exposing (main_)

{-| Usecase: Elixir

In other words, a compiler from Elm to another language.

Needs to thread the source code through the whole pipeline (parsing, desugaring,
type inference, optimizing) and then to its own emit function.

-}

import Elm.AST.Typed as Typed
import Elm.AST.Typed.Unwrapped as TypedU
import Elm.Compiler
import Elm.Compiler.Error exposing (Error)
import Elm.Data.Module as Module exposing (Module)


main_ : String -> Result Error String
main_ moduleSourceCode =
    { filePath = "src/Foo.elm"
    , sourceCode = moduleSourceCode
    }
        |> Elm.Compiler.parseModule
        |> Result.andThen Elm.Compiler.desugarOnlyModule
        |> Result.andThen Elm.Compiler.inferModule
        |> Result.map Elm.Compiler.optimizeModule
        |> Result.map (Module.map Typed.unwrap)
        |> Result.map emitElixirModule


emitElixirModule : Module TypedU.Expr -> String
emitElixirModule =
    -- TODO flesh out this example
    Debug.todo "whatever"
