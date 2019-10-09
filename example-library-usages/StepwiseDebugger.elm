module StepwiseDebugger exposing (main_)

{-| Usecase: Stepwise debugger (like you're used to from browser devtools or GDB)

This is a bit of a cheat. This is basically the normal Elm-to-JS compiler,
the only thing we need to do in addition (hopefully) is to emit a source map
along with the JS file ;)

-}

import Dict exposing (Dict)
import Elm.AST.Typed as Typed
import Elm.AST.Typed.Unwrapped as TypedU
import Elm.Compiler
import Elm.Compiler.Error exposing (Error)
import Elm.Data.FileContents exposing (FileContents)
import Elm.Data.FilePath exposing (FilePath)
import Elm.Data.Module as Module exposing (Module)
import Elm.Data.ModuleName exposing (ModuleName)


inputFiles : List { filePath : FilePath, sourceCode : FileContents }
inputFiles =
    Debug.todo "whatever"


main_ : FileContents -> Result Error { js : FileContents, sourceMap : FileContents }
main_ moduleSourceCode =
    inputFiles
        |> Elm.Compiler.parseModules
        |> Result.andThen Elm.Compiler.desugarModules
        |> Result.andThen Elm.Compiler.inferModules
        |> Result.map Elm.Compiler.optimizeModules
        |> Result.map emitJSAndSourceMap


emitJSAndSourceMap :
    Dict ModuleName (Module Typed.LocatedExpr)
    -> { js : FileContents, sourceMap : FileContents }
emitJSAndSourceMap =
    -- TODO flesh out this example
    Debug.todo "whatever"
