module Main exposing (..)

import Api.Decoder exposing (decodeExampleType)
import Api.Types exposing (ExampleType)
import Browser
import Html exposing (Html, button, div, h1, img, li, text, ul)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import Http



---- MODEL ----


type alias Model =
    { examples : List ExampleType }


init : ( Model, Cmd Msg )
init =
    ( { examples = [] }, Cmd.none )



---- UPDATE ----


type Msg
    = RequestNext
    | NextReceived (Result Http.Error ExampleType)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RequestNext ->
            ( model, loadNextExample )

        NextReceived res ->
            case res of
                Ok example ->
                    ( { model | examples = model.examples ++ [ example ] }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )


loadNextExample : Cmd Msg
loadNextExample =
    Http.get
        { url = "/api/example"
        , expect = Http.expectJson NextReceived decodeExampleType
        }



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    { title = title model
    , body = body model
    }


title : Model -> String
title _ =
    "Test title"


body : Model -> List (Html Msg)
body model =
    let
        f : ExampleType -> Html Msg
        f example =
            li [] [ text (String.fromInt example.first ++ " with message: " ++ example.second) ]

        exampleList : List (Html Msg)
        exampleList =
            if List.length model.examples == 0 then
                []

            else
                [ ul [] (List.map f model.examples) ]
    in
    [ div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm App is working!" ]
        ]
    , button [ onClick RequestNext ] [ text "Load next element" ]
    ]
        ++ exampleList



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.document
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
