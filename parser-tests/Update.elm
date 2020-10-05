port module Update exposing (main)

import Elm.Data.Located as Located exposing (Located)
import Platform
import Stage.Parse.Contextualize as Contextualize
import Stage.Parse.Lexer as Lexer
import Parser.Advanced as P


port output : String -> Cmd never


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    ()


type alias Msg =
    ()


type alias Flags =
    List
        { name : String
        , source : String
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


init : Flags -> ( Model, Cmd never )
init snippets =
    let
        rlexed : List (Result (List (P.DeadEnd Never Lexer.LexProblem)) (List (Located Lexer.LexItem)))
        rlexed =
            snippets
                |> List.map (.source >> P.run Lexer.parser)

        mrcontextualized : List (Maybe (List Contextualize.RunResult))
        mrcontextualized =
            rlexed
                |> List.map
                    (Result.toMaybe
                        >> Maybe.map
                            (List.map Located.unwrap
                                >> Contextualize.run
                            )
                    )
    in
    ( ()
    , output
        ("""    [ """
            ++ (List.map3
                    (\{ name, source } lexed contextualized ->
                        "{ name = \""
                            ++ name
                            ++ """"
      , source = \"\"\""""
                            ++ source
                            ++ """\"\"\"
      , lexed = """
                            ++ (Debug.toString lexed
                                |> preFormatElmCode
                               )
                            ++ """
      , contextualized ="""
                            ++ (Debug.toString contextualized
                                |> preFormatElmCode
                               )
                            ++ """
      }"""
                    )
                    snippets
                    rlexed
                    mrcontextualized
                    |> String.join """
    , """
               )
            ++ """
    ]
    """
        )
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update () model =
    ( model, Cmd.none )


preFormatElmCode : String -> String
preFormatElmCode =
    {-String.replace "}" """
    }"""
    >>-} String.replace "]" """
    ]"""
    >> String.replace """[
    ]""" "[]"
    >> String.replace """Err (""" """Err (
    """
    >> String.replace """Err {""" """Err {
    """
    >> String.replace """UserDefinedType {""" """UserDefinedType {
    """
