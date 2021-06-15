module Main exposing (..)

import Api as Api exposing (AccountId, UserId)
import ApiImpl exposing (ApiResponse(..), Token)
import Browser
import Css as C exposing (..)
import Css.Transitions as CT
import Dict as Dict exposing (Dict)
import Generated.Types exposing (..)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Att exposing (..)
import Html.Styled.Events exposing (..)
import Http exposing (Error(..))
import Time



---- MODEL ----


type alias Model =
    { data : DataModel
    , view : ViewModel
    }


type alias DataModel =
    { refreshToken : Maybe String
    , sessionToken : Maybe String
    , username : Maybe String
    , isWaiting : Bool
    , loggedInData : LoggedInData
    , messages : Messages
    }


type Messages
    = NoMessages
    | LoginMessages Message
    | AnyError String


type Message
    = ErrorMessage (List String)
    | Message String


type alias PseudoUser =
    ( UserId, String )


type alias LoggedInData =
    { myAccounts : Dict AccountId AccountData
    , allAccounts : Dict PseudoUser (List ( AccountId, String ))
    }


type alias ViewModel =
    { visiblePage : VisiblePage
    , loginData : LoginDataModel
    , accountView : AccountViewModel
    }


type VisiblePage
    = LoginPage
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
            , username = Nothing
            , isWaiting = False
            , loggedInData =
                { myAccounts = Dict.empty
                , allAccounts = Dict.empty
                }
            , messages = NoMessages
            }
      , view =
            { visiblePage = LoginPage
            , loginData = { username = "", password = "" }
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
    = -- Login messages
      LoginUsername String
    | LoginPassword String
    | LoginSend
    | LoginResponse (ApiResponse AuthError JwtTokensPair)
    | RegisterSend
    | RegisterResponse (ApiResponse AuthError ())
      -- Session and logout messages
    | Logout
    | LogoutResponse (ApiResponse AuthError ())
    | RefreshSession
    | RefreshSessionResponse (ApiResponse AuthError Token)
      -- Data messages
    | LoadAllData
    | MyAccountsLoaded (ApiResponse () (List ( AccountId, String )))
    | AllUsersLoaded (ApiResponse AuthError (List ( UserId, String )))
    | MyAccountLoaded (ApiResponse () AccountLoadResult)
    | ForeignAccountsLoaded PseudoUser (ApiResponse () (List ( AccountId, String )))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        waiting m =
            let
                oldData =
                    m.data
            in
            { m | data = { oldData | isWaiting = True } }

        notWaiting m =
            let
                oldData =
                    m.data
            in
            { m | data = { oldData | isWaiting = False } }

        withoutMessages m =
            let
                oldData =
                    m.data
            in
            { m | data = { oldData | messages = NoMessages } }

        withError m em errs =
            let
                oldData =
                    m.data
            in
            ( { m | data = { oldData | messages = em (ErrorMessage errs) } }, Cmd.none )

        withMessage m mm message =
            let
                oldData =
                    m.data
            in
            { m | data = { oldData | messages = mm (Message message) } }
    in
    case msg of
        LoginUsername username ->
            let
                oldView =
                    model.view

                oldLogin =
                    oldView.loginData
            in
            ( { model | view = { oldView | loginData = { oldLogin | username = username } } }, Cmd.none )

        LoginPassword password ->
            let
                oldView =
                    model.view

                oldLogin =
                    oldView.loginData
            in
            ( { model | view = { oldView | loginData = { oldLogin | password = password } } }, Cmd.none )

        LoginSend ->
            ( withoutMessages <| waiting model
            , Api.authLogin LoginResponse
                { username = model.view.loginData.username
                , password = model.view.loginData.password
                }
            )

        LoginResponse res ->
            handleApiResponse res authErrorToText (withError (notWaiting model) LoginMessages) <|
                \tokensPair ->
                    let
                        oldModel =
                            withoutMessages model

                        oldData =
                            oldModel.data

                        oldView =
                            oldModel.view
                    in
                    ( { oldModel
                        | data =
                            { oldData
                                | sessionToken = Just tokensPair.session
                                , refreshToken = Just tokensPair.refresh
                                , username = Just model.view.loginData.username
                            }
                        , view =
                            { oldView
                                | visiblePage = AccountsPage
                            }
                      }
                    , loadAccountData tokensPair.session
                    )

        RegisterSend ->
            ( withoutMessages <| waiting model
            , Api.authCreate RegisterResponse
                { username = model.view.loginData.username
                , password = model.view.loginData.password
                }
            )

        RegisterResponse res ->
            handleApiResponse res authErrorToText (withError (notWaiting model) LoginMessages) <|
                \() ->
                    ( withMessage (notWaiting model) LoginMessages "Registration complete", Cmd.none )

        _ ->
            ( model, Cmd.none )


loadAccountData : Token -> Cmd Msg
loadAccountData sessionToken =
    let
        loadMyAccounts =
            Api.myAccounts MyAccountsLoaded sessionToken

        loadAllUsers =
            Api.authAllUsers AllUsersLoaded sessionToken
    in
    Cmd.batch [ loadMyAccounts, loadAllUsers ]


handleApiResponse :
    ApiResponse e a
    -> (e -> String)
    -> (List String -> answer)
    -> (a -> answer)
    -> answer
handleApiResponse res errToText errH ansH =
    case res of
        ApiOk a ->
            ansH a

        UserError e ->
            errH <| [ errToText e ]

        OtherError es ->
            errH es

        ConnectionError e ->
            errH <|
                (\x -> [ x ]) <|
                    case e of
                        BadUrl s ->
                            "Bad URL: " ++ s

                        Timeout ->
                            "Request timed out - Check your connection?"

                        NetworkError ->
                            "A network error occurred"

                        BadStatus status ->
                            "A bad status response came in (shouldn't happen): " ++ String.fromInt status

                        BadBody msg ->
                            "Bad body: " ++ msg


authErrorToText : AuthError -> String
authErrorToText err =
    case err of
        UnknownUser ->
            "User unknown"

        UsernameTaken ->
            "Username taken"

        WrongPassword ->
            "Wrong password"

        NotLoggedIn ->
            "You're not logged in"

        SessionLoggedOut ->
            "Your session was logged out"

        NotBearerAuthenticated ->
            "No Bearer Authentication (programming error)"

        UserNoLongerExists ->
            "Your user no longer exists"

        LoginExpired ->
            "Your login has expired"

        AuthTokenError (TokenMalformed detail) ->
            "Internal Token Error: Malformed: " ++ detail

        AuthTokenError TokenUnsigned ->
            "Internal Token Error: Unsigned"

        AuthTokenError TokenWrongType ->
            "Internal Token Error: Wrong type"

        AuthTokenError TokenExpired ->
            "Internal Token Error: Expired"



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    { title = title model
    , body = List.map toUnstyled <| body model
    }


title : Model -> String
title model =
    case model.data.username of
        Nothing ->
            "Login | Elm Event Bank"

        Just name ->
            name ++ "'s Accounts | Elm Event Bank"


{-| TODO Refresh & Logout button
-}
header : Model -> Html Msg
header model =
    let
        clickSpan : Bool -> List (Attribute msg) -> List (Html msg) -> Html msg
        clickSpan canClick =
            styled Html.span <|
                [ padding2 (px 5) (px 10)
                , backgroundColor <| hex "#909090"
                ]
                    ++ (if not canClick then
                            []

                        else
                            [ hover
                                [ backgroundColor <| hex "#7c7c7c"
                                ]
                            , cursor <| clickable model
                            ]
                       )
    in
    div
        [ css
            [ left (px 0)
            , right (px 0)
            , fontSize (C.em 1.2)
            , backgroundColor <| hex "#a9a9a9"
            , displayFlex
            , justifyContent spaceBetween
            , alignItems center
            , flexDirection row
            ]
        ]
    <|
        case model.data.username of
            Just username ->
                [ clickSpan True [ onClick LoadAllData ] [ text "Refresh" ]
                , clickSpan False [] [ text <| "Elm Event Bank | " ++ username ]
                , clickSpan True [ onClick Logout ] [ text "Logout" ]
                ]

            Nothing ->
                [ Html.span [] []
                , clickSpan False [] [ text "Elm Event Bank" ]
                , Html.span [] []
                ]


body : Model -> List (Html Msg)
body model =
    header model
        :: (case model.view.visiblePage of
                LoginPage ->
                    loginForm model

                AccountsPage ->
                    accountsPage model
           )


displayMessage : Message -> List (Html Never)
displayMessage message =
    case message of
        ErrorMessage errors ->
            [ ul
                [ css
                    [ color <| hex "#af0000"
                    , display inlineBlock
                    ]
                ]
              <|
                List.map (\err -> li [] [ text err ]) errors
            , br [] []
            ]

        Message msgText ->
            [ text msgText, br [] [] ]


loginForm : Model -> List (Html Msg)
loginForm model =
    let
        displayMessages =
            case model.data.messages of
                LoginMessages msg ->
                    List.map (Html.map never) <| displayMessage msg

                _ ->
                    []
    in
    [ p [] [ text "Welcome to the Elm Event Bank.", br [] [], text "Please register an account or log into your existing one." ]
    , div []
        ([ input model [ type_ "text", name "username", placeholder "Username", value model.view.loginData.username, onInput LoginUsername ]
         , br [] []
         , input model [ type_ "password", name "password", placeholder "Password", value model.view.loginData.password, onInput LoginPassword ]
         , br [] []
         ]
            ++ displayMessages
            ++ [ centeredButton model [ onClick LoginSend ] [ text "Login" ]
               , br [] []
               , centeredButton model [ onClick RegisterSend ] [ text "Register" ]
               ]
        )
    ]


accountsPage : Model -> List (Html Msg)
accountsPage _ =
    []


clickable : Model -> Cursor {}
clickable model =
    if model.data.isWaiting then
        notAllowed

    else
        pointer


input : Model -> List (Attribute msg) -> Html msg
input model attrs =
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
        (if model.data.isWaiting then
            Att.disabled True :: attrs

         else
            attrs
        )
        []


centeredButton : Model -> List (Attribute msg) -> List (Html msg) -> Html msg
centeredButton model attrs =
    styled Html.button
        [ backgroundImage <| linearGradient (stop <| hex "#007aaa") (stop <| hex "#007fff") []
        , border3 (px 1) solid <| hex "#007fff"
        , hover
            [ backgroundImage <| linearGradient (stop <| hex "#007888") (stop <| hex "#007ddd") []
            , border3 (px 1) solid <| hex "#007ddd"
            ]
        , textAlign center
        , padding2 (px 5) (px 20)
        , margin2 (px 5) (px 0)
        , borderRadius (px 10)
        , color <| hex "#dedede"
        , fontFamily monospace
        , cursor <| clickable model
        ]
        (if model.data.isWaiting then
            Att.disabled True :: attrs

         else
            attrs
        )



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.data.refreshToken of
        Nothing ->
            Sub.none

        Just _ ->
            Time.every (1000 * 60 * 10) <| always RefreshSession



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.document
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none -- subscriptions
        }
