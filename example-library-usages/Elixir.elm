module Elixir exposing (main_)

{-| Usecase: Elixir

In other words, a compiler from Elm to another language, with the catch that
it compiles modules in separation (one module -> one output file). This is in
contrast to the classic Elm-to-JS compiler scenario, where all the modules get
compiled into one file together.

Needs to thread the source code through the whole pipeline (parsing, desugaring,
type inference, optimizing) and then to its own emit function.

-}

import Elm.AST.Typed as Typed
import Elm.AST.Typed.Unwrapped as TypedU
import Elm.Compiler
import Elm.Compiler.Error exposing (Error)
import Elm.Data.FileContents exposing (FileContents)
import Elm.Data.Module as Module exposing (Module)


main_ : FileContents -> Result Error FileContents
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


emitElixirModule : Module TypedU.Expr -> FileContents
emitElixirModule =
    -- TODO flesh out this example
    Debug.todo "whatever"
