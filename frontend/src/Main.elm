module Main exposing (..)

import Api as Api exposing (ApiResponse(..), Token)
import Browser
import Generated.Decoder
import Generated.Types exposing (AuthError, SingleToken)
import Html exposing (Html, button, div, h1, img, input, li, text, ul)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import Http
import Json.Decode as D exposing (..)
import Json.Decode.Pipeline as D exposing (required)
import Time



---- MODEL ----


type alias Model =
    { data : DataModel, view : ViewModel }


type alias DataModel =
    { refreshToken : Maybe String, sessionToken : Maybe String }


type alias ViewModel =
    {}


init : ( Model, Cmd Msg )
init =
    let
        initData =
            { refreshToken = Nothing, sessionToken = Nothing }

        initView =
            {}
    in
    ( { data = initData, view = initView }, Cmd.none )



---- UPDATE ----


type Msg
    = NewSessionToken (Api.ApiResponse AuthError Token)
    | RefreshToken Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewSessionToken res ->
            case res of
                ApiOk newToken ->
                    let
                        tempData =
                            model.data
                    in
                    ( { model | data = { tempData | sessionToken = Just newToken } }, Cmd.none )

                UserError e ->
                    ( model, Cmd.none )

                OtherError string ->
                    ( model, Cmd.none )

                ConnectionError error ->
                    ( model, Cmd.none )

        -- TODO
        _ ->
            ( model, Cmd.none )


sendRefreshRequest : Model -> Cmd Msg
sendRefreshRequest model =
    case model.data.refreshToken of
        Nothing ->
            Cmd.none

        Just refreshToken ->
            Api.authRefresh NewSessionToken <| SingleToken refreshToken



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
        foo =
            2
    in
    [ div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm App is working!" ]
        ]
    , button [ class "" ] [ text "Load next element" ]
    , input [] []
    ]



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.data.refreshToken of
        Nothing ->
            Sub.none

        Just _ ->
            Time.every (1000 * 60 * 10) RefreshToken



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.document
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
