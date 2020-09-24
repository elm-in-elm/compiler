module Live exposing (main)

import Browser
import Elm.Data.Located as Located
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Parser.Advanced as P
import Stage.Parse.Contextualize as Contextualize
import Stage.Parse.Lexer as Lexer


{-| We're essentially a Node.JS app (until we get self-hosting :P ).
So, `Platform.worker` is the only option for us.
-}
main : Program () String Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


{-| `Compiling` is the state we'll be most of the time. The other two are
mostly useless; they do effectively stop `subscriptions` and `update` though.
-}
type alias Model =
    String


type Msg
    = NewString String


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


init : () -> ( Model, Cmd never )
init () =
    ( """type alias Model =""", Cmd.none )


view : Model -> Html Msg
view model =
    let
        source =
            model

        lexed =
            P.run Lexer.parser source
    in
    Html.div
        [ Attributes.style "display" "flex"
        , Attributes.style "flex-direction" "row"
        ]
        [ Html.div
            []
            [ Html.textarea
                [ Events.onInput <| NewString
                , Attributes.value source
                , Attributes.rows 20
                , Attributes.cols 80
                ]
                []
            , Html.div
                [ Attributes.style "font-family" "monospace" ]
                -- [ Attributes.style "w" ]
                (case lexed of
                    Ok lexItems ->
                        lexItems
                            |> List.map
                                (\item ->
                                    let
                                        { start, end } =
                                            Located.getRegion item
                                    in
                                    Html.div
                                        [ Attributes.style "white-space" "pre-wrap" ]
                                        [ Html.text
                                            ("("
                                                ++ String.padLeft 3 '0' (String.fromInt start.row)
                                                ++ ", "
                                                ++ String.padLeft 3 '0' (String.fromInt start.col)
                                                ++ ") - ("
                                                ++ String.padLeft 3 '0' (String.fromInt end.row)
                                                ++ ", "
                                                ++ String.padLeft 3 '0' (String.fromInt end.col)
                                                ++ "): \""
                                                ++ (item |> Located.unwrap |> Lexer.toString |> String.replace " " "·")
                                                ++ "\""
                                            )
                                        ]
                                )

                    Err e ->
                        [ Html.text (Debug.toString e) ]
                )
            ]
        , Html.div
            [ Attributes.style "font-family" "monospace" ]
            -- [ Attributes.style "w" ]
            (case lexed of
                Ok lexItems ->
                    [ Html.div
                        [ Attributes.style "white-space" "pre-wrap" ]
                        [ Html.text
                            (Debug.toString (Contextualize.parser (lexItems |> List.map Located.unwrap)))
                        ]
                    ]

                Err e ->
                    []
            )
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewString string ->
            ( string, Cmd.none )
