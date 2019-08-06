module UsecaseElmFormat exposing (main)

{-| Usecase: elm-format

Needs to parse a module to frontend AST (as close to source as possible), and
non-opaque access to that AST to be able to emit back to Elm.

Gives us constraints on the API:

    parseModule : String -> Result Error (Module Frontend.Expr)

-}

import Elm.Compiler.AST.Frontend as Frontend
import Elm.Compiler.Error exposing (Error)
import Elm.Compiler.Module exposing (Module)


main : String -> Result Error String
main moduleSourceCode =
    moduleSourceCode
        |> Elm.Compiler.parseModule
        |> Random.map emitToElm


emitToElm : Module Frontend.Expr -> String
emitToElm module_ =
    Debug.todo "whatever"
