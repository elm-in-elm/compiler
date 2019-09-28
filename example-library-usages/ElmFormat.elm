module ElmFormat exposing (main_)

{-| Usecase: elm-format

Needs to parse a module to frontend AST (as close to source as possible), and
non-opaque access to that AST to be able to emit back to Elm.

Also, doesn't unwrap the expressions as the location information is probably
useful (for sorting the top-level declarations etc.).

-}

import Elm.AST.Frontend as Frontend
import Elm.AST.Frontend.Unwrapped as FrontendU
import Elm.Compiler
import Elm.Compiler.Error exposing (Error)
import Elm.Data.FileContents exposing (FileContents)
import Elm.Data.FilePath exposing (FilePath)
import Elm.Data.Module exposing (Module)


main_ : { filePath : FilePath, sourceCode : FileContents } -> Result Error FileContents
main_ file =
    file
        |> Elm.Compiler.parseModule
        |> Result.map emitToElm


emitToElm : Module Frontend.LocatedExpr -> FileContents
emitToElm module_ =
    -- TODO flesh out this example
    Debug.todo "whatever"
