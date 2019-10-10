# elm-in-elm

Elm compiler written in Elm!

_For more information on the `elm-in-elm` project in general read the [comprehensive documentation](http://github.com/elm-in-elm/compiler/tree/master/README.md)._

The entrypoint of this library is the [`Elm.Compiler`](/packages/elm-in-elm/compiler/latest/Elm-Compiler) module.

The typical flow of functions inside this library looks like this:

![Stages of the compiler](https://github.com/elm-in-elm/compiler/raw/master/assets/stages.png)

## Example usage - Elm to Elixir

An example of how the core of Elm to Elixir compiler might look; pay attention to the pipeline inside `elmToElixir`:

```elm
module ElmToElixir exposing (elmToElixir)

import Elm.AST.Typed as Typed
import Elm.AST.Typed.Unwrapped as TypedU
import Elm.Compiler
import Elm.Compiler.Error exposing (Error)
import Elm.Data.FileContents exposing (FileContents)
import Elm.Data.FilePath exposing (FilePath)
import Elm.Data.Module as Module exposing (Module)


{-| Usage: 

    elmToElixir
      { filePath = "src/Foo.elm"
      , sourceCode = 
          """
          module Foo exposing (foo)

          foo : Int -> String
          foo x =
            String.repeat x "FOO! "
          """
      }
-}
elmToElixir : { filePath : FilePath, sourceCode : FileContents} -> Result Error String
elmToElixir file =
    file
        |> Elm.Compiler.parseModule
        |> Result.andThen Elm.Compiler.desugarOnlyModule
        |> Result.andThen Elm.Compiler.inferModule
        |> Result.map Elm.Compiler.optimizeModule
        |> Result.map (Module.map Typed.unwrap)
        |> Result.map emitElixirModule


{-| The important part: emit Elixir source code!  -}
emitElixirModule : Module TypedU.Expr -> FileContents
emitElixirModule m =
    [ emitHeader m.name m.exposing_
    , emitImports m.imports
    , emitDeclarations m.declarations
    ]
        |> String.join "\n\n\n"


emitHeader = Debug.todo "emitHeader"
emitImports = Debug.todo "emitImports"
emitDeclarations = Debug.todo "emitDeclarations"
```
