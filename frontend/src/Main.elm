module Main exposing (..)

import Api as Api exposing (AccountId, UserId)
import ApiImpl exposing (ApiResponse(..), Token)
import Browser
import Css as C exposing (..)
import Css.Transitions as CT
import Dict as Dict exposing (Dict)
import Generated.Types exposing (..)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Time



---- MODEL ----


type alias Model =
    { data : DataModel
    , view : ViewModel
    }


type alias DataModel =
    { refreshToken : Maybe String
    , sessionToken : Maybe String
    , isRefreshing : Bool
    , loggedInData : Maybe LoggedInData
    }


type alias LoggedInData =
    { myAccounts : Dict AccountId AccountData
    , allAccounts : Dict UserId (List AccountId)
    }


type alias ViewModel =
    { visiblePage : VisiblePage
    , loginData : LoginDataModel
    , registerData : LoginDataModel
    , accountView : AccountViewModel
    }


type VisiblePage
    = LoginPage
    | RegisterPage
    | AccountsPage


type alias LoginDataModel =
    { username : String, password : String }


type alias AccountViewModel =
    { opened : Maybe ( AccountData, AccountViewOpenedForm )
    , newAccountName : String
    , withdrawAmount : Maybe Int
    , depositAmount : Maybe Int
    , transferUserSelected : Maybe String
    , transferAmount : Maybe Int
    }


type AccountViewOpenedForm
    = WithdrawForm
    | DepositForm
    | TransferForm


init : ( Model, Cmd Msg )
init =
    ( { data =
            { refreshToken = Nothing
            , sessionToken = Nothing
            , isRefreshing = False
            , loggedInData = Nothing
            }
      , view =
            { visiblePage = LoginPage
            , loginData = { username = "", password = "" }
            , registerData = { username = "", password = "" }
            , accountView =
                { opened = Nothing
                , newAccountName = ""
                , withdrawAmount = Nothing
                , depositAmount = Nothing
                , transferUserSelected = Nothing
                , transferAmount = Nothing
                }
            }
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    { title = title model
    , body = List.map toUnstyled <| body model
    }


title : Model -> String
title _ =
    "Test title"


{-| TODO Refresh & Logout button
-}
header : Model -> Html Msg
header model =
    div [] []


body : Model -> List (Html Msg)
body model =
    [ div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Welcome to the Elm Bank" ]
        ]
    ]



--loginForm : String -> LoginDataModel -> Html LoginMsg
--loginForm sendText model =
--    Html.form [ onSubmit Send ]
--        [ input [ type_ "text", name "username", placeholder "Username", value model.username, onInput Username ] []
--        , br [] []
--        , input [ type_ "password", name "password", placeholder "Password", value model.password, onInput Password ] []
--        , br [] []
--        , centeredButton [ type_ "submit" ] [ text sendText ]
--        ]


input : List (Attribute msg) -> List (Html msg) -> Html msg
input =
    styled Html.input
        [ padding2 (px 5) (px 10)
        , border3 (px 1) solid <| hex "#7f7f7f"
        , borderRadius (px 50)
        , margin (px 2)
        , CT.transition [ CT.borderColor 0.5 ]
        , important <| outline none
        , focus
            [ borderColor (hex "007fff")
            ]
        , pseudoElement "placeholder"
            [ textAlign center
            , fontStyle italic
            , fontSize (C.em 1.2)
            ]
        ]


centeredButton : List (Attribute msg) -> List (Html msg) -> Html msg
centeredButton =
    styled Html.button
        [ backgroundImage <| linearGradient (stop <| hex "#007aaa") (stop <| hex "#007fff") []
        , border3 (px 1) solid <| hex "#007fff"
        , hover
            [ backgroundImage <| linearGradient (stop <| hex "#007888") (stop <| hex "#007ddd") []
            , border3 (px 1) solid <| hex "#007ddd"
            ]
        , marginLeft auto
        , textAlign center
        , padding2 (px 5) (px 20)
        , borderRadius (px 10)
        , color <| hex "dedede"
        , fontFamily monospace
        ]



---- SUBSCRIPTIONS ----
--subscriptions : Model -> Sub Msg
--subscriptions model =
--    case model.data.refreshToken of
--        Nothing ->
--            Sub.none
--
--        Just _ ->
--            Time.every (1000 * 60 * 10) <| always (DataMsg RefreshToken)
---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.document
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none -- subscriptions
        }
