module Slackbot exposing (main_)

{-| Usecase: Elm-evaluating Slackbot

Needs to parse the given snippet as either an import, top-level Declaration or
an expression.

Bonus points for eval function, but that doesn't have to live in elm/compiler.
(elm/interpreter? :P)

-}

import Elm.AST.Frontend as Frontend
import Elm.Compiler
import Elm.Compiler.Error exposing (Error)
import Elm.Data.Declaration exposing (Declaration)
import Elm.Data.Import exposing (Import)
import Result.Extra


type SlackbotError
    = ElmCompilerError Error
    | CustomFoo


type SlackbotValue
    = -- TODO
      SlackbotValue


type alias Snippets =
    { imports : List String
    , declarations : List String
    , expr : String
    }


main_ : String -> Result SlackbotError SlackbotValue
main_ snippet =
    let
        snippets : Snippets
        snippets =
            Debug.todo "whatever"

        imports : Result Error (List Import)
        imports =
            snippets.imports
                |> List.map Elm.Compiler.parseImport
                |> Result.Extra.combine

        declarations : Result Error (List (Declaration Frontend.LocatedExpr))
        declarations =
            snippets.declarations
                |> List.map
                    (\declaration ->
                        Elm.Compiler.parseDeclaration
                            { moduleName = Debug.todo "???"
                            , declaration = declaration
                            }
                    )
                |> Result.Extra.combine

        expr : Result Error Frontend.LocatedExpr
        expr =
            Elm.Compiler.parseExpr snippets.expr
    in
    -- TODO flesh out this example
    Debug.todo "whatever"
