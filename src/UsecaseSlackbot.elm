module UsecaseSlackbot exposing (main)

{-| Usecase: Elm-evaluating Slackbot

Needs to parse the given snippet as either an import, top-level definition or
an expression.

Bonus points for eval function, but that doesn't have to live in elm/compiler.
(elm/interpreter? :P)

Gives us constraints on the API:

    parseImport : String -> Result Error Import

    parseDefinition : String -> Result Error (Definition Frontend.LocatedExpr)

    parseExpr : String -> Result Error Frontend.LocatedExpr

Unresolved questions:

    1. Is there a nice way of adding user errors to elm/compiler's Error type?
       Or is it better for them to wrap it themselves?
       (CustomError = ElmError Error | Custom)

-}

import Elm.Compiler.AST.Frontend as Frontend
import Elm.Compiler.Error exposing (Error)
import Elm.Compiler.Import exposing (Import)


main : String -> Result SlackbotError SlackbotValue
main snippet =
    let
        ( importSnippets, definitionSnippets, exprSnippet ) =
            Debug.todo "whatever"

        imports : Result Error (List Import)
        imports =
            importSnippets
                |> List.map Elm.Compiler.parseImport
                |> Result.Extra.combine

        definitions : Result Error (List (Definition Frontend.LocatedExpr))
        definitions =
            definitionSnippets
                |> List.map Elm.Compiler.parseDefinition
                |> Result.Extra.combine

        expr : Result Error Frontend.LocatedExpr
        expr =
            Elm.Compiler.parseExpr exprSnippet
    in
    Debug.todo "whatever"
