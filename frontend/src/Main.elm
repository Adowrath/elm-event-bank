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
import Json.Encode as Json
import Time exposing (Month(..), toDay, toHour, toMinute, toMonth, toSecond, toYear, utc)



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
    | WithdrawError Message
    | DepositError Message
    | TransferToError Message
    | GlobalError (List String)


type Message
    = ErrorMessage (List String)
    | Message String


type alias PseudoUser =
    ( UserId, String )


type alias LoggedInData =
    { myAccounts : Dict AccountId AccountData
    , allAccounts : Dict PseudoUser (List ( AccountId, String ))
    }


emptyLoggedInData : LoggedInData
emptyLoggedInData =
    { myAccounts = Dict.empty, allAccounts = Dict.empty }


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


emptyLoginDataModel : LoginDataModel
emptyLoginDataModel =
    { username = "", password = "" }


type alias AccountViewModel =
    { opened : Maybe AccountData
    , newAccountName : String
    , withdrawAmount : Int
    , depositAmount : Int
    , transferAccount : AccountId
    , transferAmount : Int
    }


emptyAccountViewModel : AccountViewModel
emptyAccountViewModel =
    { opened = Nothing
    , newAccountName = ""
    , withdrawAmount = 0
    , depositAmount = 0
    , transferAccount = ""
    , transferAmount = 0
    }


init : ( Model, Cmd Msg )
init =
    ( { data =
            { refreshToken = Nothing
            , sessionToken = Nothing
            , username = Nothing
            , isWaiting = False
            , loggedInData = emptyLoggedInData
            , messages = NoMessages
            }
      , view =
            { visiblePage = LoginPage
            , loginData = emptyLoginDataModel
            , accountView = emptyAccountViewModel
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
    | MyAccountLoaded (ApiResponse () AccountLoadResult)
    | CurrentAccountReloaded (ApiResponse () AccountLoadResult)
    | AllUsersLoaded (ApiResponse AuthError (List ( UserId, String )))
    | ForeignAccountsLoaded PseudoUser (ApiResponse () (List ( AccountId, String )))
      -- Account forms
    | SelectAccount AccountData
    | NewAccountName String
    | NewAccountSend
    | NewAccountResponse (ApiResponse () AccountId)
    | CloseAccountSend
    | CloseAccountResponse (ApiResponse () Bool)
    | WithdrawAmount String
    | WithdrawSend
    | WithdrawResponse (ApiResponse () AccountProcessResult)
    | DepositAmount String
    | DepositSend
    | DepositResponse (ApiResponse () AccountProcessResult)
    | TransferToAccount AccountId
    | TransferToAmount String
    | TransferToSend
    | TransferToResponse (ApiResponse () AccountProcessResult)


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

        withGlobalError m errs =
            let
                oldData =
                    m.data
            in
            ( case oldData.messages of
                GlobalError prevErrs ->
                    { m | data = { oldData | messages = GlobalError (prevErrs ++ errs) } }

                _ ->
                    { m | data = { oldData | messages = GlobalError errs } }
            , Cmd.none
            )

        withMessage m mm message =
            let
                oldData =
                    m.data
            in
            { m | data = { oldData | messages = mm (Message message) } }
    in
    case msg of
        SelectAccount acc ->
            let
                oldView =
                    model.view

                oldAcc =
                    oldView.accountView
            in
            ( { model | view = { oldView | accountView = { oldAcc | opened = Just acc } } }, Cmd.none )

        Logout ->
            case model.data.sessionToken of
                Nothing ->
                    -- Ignore
                    ( model, Cmd.none )

                Just sessionToken ->
                    ( withoutMessages <| waiting model
                    , Api.authLogout LogoutResponse sessionToken ()
                    )

        LogoutResponse res ->
            -- We ignore any errors and just log out.
            handleApiResponse res authErrorToText (always init) (always init)

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
                                , loginData = emptyLoginDataModel
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

        RefreshSession ->
            case model.data.refreshToken of
                Nothing ->
                    -- Ignore
                    ( model, Cmd.none )

                Just refreshToken ->
                    ( model
                    , Api.authRefresh RefreshSessionResponse (SingleToken refreshToken)
                    )

        RefreshSessionResponse res ->
            handleApiResponse res authErrorToText (Debug.log "Refresh Error!" >> always init) <|
                \newSessionToken ->
                    let
                        oldModel =
                            notWaiting model

                        oldData =
                            oldModel.data
                    in
                    ( { oldModel | data = { oldData | sessionToken = Just newSessionToken } }, Cmd.none )

        LoadAllData ->
            case model.data.sessionToken of
                Nothing ->
                    ( model, Cmd.none )

                Just sessionToken ->
                    let
                        oldModel =
                            waiting model

                        oldData =
                            oldModel.data

                        oldView =
                            oldModel.view
                    in
                    ( { oldModel
                        | data = { oldData | loggedInData = emptyLoggedInData }
                        , view = { oldView | accountView = emptyAccountViewModel }
                      }
                    , loadAccountData sessionToken
                    )

        MyAccountsLoaded res ->
            case model.data.sessionToken of
                Nothing ->
                    ( model, Cmd.none )

                Just sessionToken ->
                    handleApiResponse res (always "An error has occurred") (withGlobalError (notWaiting model)) <|
                        \accounts ->
                            let
                                loadMyAccount ( accId, _ ) =
                                    Api.loadAccount accId MyAccountLoaded sessionToken
                            in
                            ( model
                            , Cmd.batch <| List.map loadMyAccount accounts
                            )

        MyAccountLoaded res ->
            handleApiResponse res (always "An error has occurred") (withGlobalError (notWaiting model)) <|
                \loadRes ->
                    case loadRes of
                        -- ignore those error cases
                        NoAccountFound ->
                            ( model, Cmd.none )

                        NotYourAccount ->
                            ( model, Cmd.none )

                        LoadResult account ->
                            let
                                oldModel =
                                    notWaiting model

                                oldData =
                                    oldModel.data

                                oldLoggedInData =
                                    oldData.loggedInData

                                oldMyAccounts =
                                    oldLoggedInData.myAccounts
                            in
                            ( { oldModel
                                | data =
                                    { oldData
                                        | loggedInData =
                                            { oldLoggedInData
                                                | myAccounts =
                                                    Dict.insert account.id account oldMyAccounts
                                            }
                                    }
                              }
                            , Cmd.none
                            )

        CurrentAccountReloaded res ->
            handleApiResponse res (always "An error has occurred") (withGlobalError (notWaiting model)) <|
                \loadRes ->
                    case loadRes of
                        -- ignore those error cases
                        NoAccountFound ->
                            ( model, Cmd.none )

                        NotYourAccount ->
                            ( model, Cmd.none )

                        LoadResult account ->
                            let
                                oldModel =
                                    notWaiting model

                                oldData =
                                    oldModel.data

                                oldView =
                                    oldModel.view

                                oldLoggedInData =
                                    oldData.loggedInData

                                oldMyAccounts =
                                    oldLoggedInData.myAccounts
                            in
                            ( { oldModel
                                | data =
                                    { oldData
                                        | loggedInData =
                                            { oldLoggedInData
                                                | myAccounts =
                                                    Dict.insert account.id account oldMyAccounts
                                            }
                                    }
                                , view = { oldView | accountView = { emptyAccountViewModel | opened = Just account } }
                              }
                            , Cmd.none
                            )

        AllUsersLoaded res ->
            case model.data.sessionToken of
                Nothing ->
                    ( model, Cmd.none )

                Just sessionToken ->
                    handleApiResponse res (always "An error has occurred") (withGlobalError (notWaiting model)) <|
                        \accounts ->
                            let
                                loadForeignAccounts ( userId, username ) =
                                    Api.accountsBy userId (ForeignAccountsLoaded ( userId, username )) sessionToken
                            in
                            ( model
                            , Cmd.batch <| List.map loadForeignAccounts accounts
                            )

        ForeignAccountsLoaded pseudoUser res ->
            handleApiResponse res (always "An error has occurred") (withGlobalError (notWaiting model)) <|
                \accounts ->
                    let
                        oldModel =
                            notWaiting model

                        oldData =
                            oldModel.data

                        oldLoggedInData =
                            oldData.loggedInData

                        oldAllAccounts =
                            oldLoggedInData.allAccounts

                        updateAccounts ( accountId, accountName ) =
                            Dict.update pseudoUser <|
                                \existing ->
                                    case existing of
                                        Nothing ->
                                            Just [ ( accountId, accountName ) ]

                                        Just existingAccounts ->
                                            Just (( accountId, accountName ) :: existingAccounts)

                        newAccounts =
                            List.foldl updateAccounts oldAllAccounts accounts
                    in
                    -- | ForeignAccountsLoaded PseudoUser (ApiResponse () (List ( AccountId, String )))
                    ( { oldModel
                        | data =
                            { oldData
                                | loggedInData =
                                    { oldLoggedInData
                                        | allAccounts = newAccounts
                                    }
                            }
                      }
                    , Cmd.none
                    )

        NewAccountName name ->
            let
                oldView =
                    model.view

                oldAccView =
                    oldView.accountView
            in
            ( { model | view = { oldView | accountView = { oldAccView | newAccountName = name } } }, Cmd.none )

        WithdrawAmount text ->
            case String.toInt text of
                Nothing ->
                    ( model, Cmd.none )

                Just num ->
                    if num < 0 then
                        ( model, Cmd.none )

                    else
                        let
                            oldView =
                                model.view

                            oldAccView =
                                oldView.accountView
                        in
                        ( { model | view = { oldView | accountView = { oldAccView | withdrawAmount = num } } }, Cmd.none )

        DepositAmount text ->
            case String.toInt text of
                Nothing ->
                    ( model, Cmd.none )

                Just num ->
                    if num < 0 then
                        ( model, Cmd.none )

                    else
                        let
                            oldView =
                                model.view

                            oldAccView =
                                oldView.accountView
                        in
                        ( { model | view = { oldView | accountView = { oldAccView | depositAmount = num } } }, Cmd.none )

        TransferToAccount account ->
            let
                oldView =
                    model.view

                oldAccView =
                    oldView.accountView
            in
            ( { model | view = { oldView | accountView = { oldAccView | transferAccount = account } } }, Cmd.none )

        TransferToAmount text ->
            case String.toInt text of
                Nothing ->
                    ( model, Cmd.none )

                Just num ->
                    if num < 0 then
                        ( model, Cmd.none )

                    else
                        let
                            oldView =
                                model.view

                            oldAccView =
                                oldView.accountView
                        in
                        ( { model | view = { oldView | accountView = { oldAccView | transferAmount = num } } }, Cmd.none )

        NewAccountSend ->
            case model.data.sessionToken of
                Nothing ->
                    ( model, Cmd.none )

                Just sessionToken ->
                    ( waiting model
                    , Api.accountOpen NewAccountResponse sessionToken <| AccountOpen model.view.accountView.newAccountName
                    )

        NewAccountResponse res ->
            case model.data.sessionToken of
                Nothing ->
                    ( model, Cmd.none )

                Just sessionToken ->
                    handleApiResponse res (always "An error has occurred") (withGlobalError (notWaiting model)) <|
                        \accountId ->
                            ( model
                            , Api.loadAccount accountId CurrentAccountReloaded sessionToken
                            )

        CloseAccountSend ->
            case model.data.sessionToken of
                Nothing ->
                    ( model, Cmd.none )

                Just sessionToken ->
                    case model.view.accountView.opened of
                        Nothing ->
                            ( model, Cmd.none )

                        Just account ->
                            ( waiting model
                            , Api.closeAccount account.id CloseAccountResponse sessionToken ()
                            )

        CloseAccountResponse res ->
            case model.data.sessionToken of
                Nothing ->
                    ( model, Cmd.none )

                Just sessionToken ->
                    handleApiResponse res (always "An error has occurred") (withGlobalError (notWaiting model)) <|
                        \_ ->
                            case model.view.accountView.opened of
                                Nothing ->
                                    ( model, Cmd.none )

                                Just account ->
                                    ( waiting model
                                    , Api.loadAccount account.id CurrentAccountReloaded sessionToken
                                    )

        WithdrawSend ->
            case model.data.sessionToken of
                Nothing ->
                    ( model, Cmd.none )

                Just sessionToken ->
                    case model.view.accountView.opened of
                        Nothing ->
                            ( model, Cmd.none )

                        Just accData ->
                            ( waiting model, Api.processEvent accData.id WithdrawResponse sessionToken <| WithdrewIn model.view.accountView.withdrawAmount )

        WithdrawResponse res ->
            case model.data.sessionToken of
                Nothing ->
                    ( model, Cmd.none )

                Just sessionToken ->
                    handleApiResponse res (always "An error has occured") (withGlobalError (notWaiting model)) <|
                        \processResult ->
                            case processResult of
                                NotYourAccountToModify ->
                                    withError (notWaiting model) WithdrawError [ "Not your account!" ]

                                NotEnoughBalance ->
                                    withError (notWaiting model) WithdrawError [ "Not enough balance" ]

                                AccountClosed Yours ->
                                    withError (notWaiting model) WithdrawError [ "Your account was already closed" ]

                                AccountClosed Theirs ->
                                    withError (notWaiting model) WithdrawError [ "Their account was closed" ]

                                AccountDoesNotExist Yours ->
                                    withError (notWaiting model) WithdrawError [ "Your account does not exist" ]

                                AccountDoesNotExist Theirs ->
                                    withError (notWaiting model) WithdrawError [ "Their account does not exist" ]

                                AccountOk ->
                                    case model.view.accountView.opened of
                                        Nothing ->
                                            ( model, Cmd.none )

                                        Just acc ->
                                            ( model, Api.loadAccount acc.id CurrentAccountReloaded sessionToken )

        DepositSend ->
            case model.data.sessionToken of
                Nothing ->
                    ( model, Cmd.none )

                Just sessionToken ->
                    case model.view.accountView.opened of
                        Nothing ->
                            ( model, Cmd.none )

                        Just accData ->
                            ( model
                            , Api.processEvent accData.id DepositResponse sessionToken <| DepositedIn model.view.accountView.depositAmount
                            )

        DepositResponse res ->
            case model.data.sessionToken of
                Nothing ->
                    ( model, Cmd.none )

                Just sessionToken ->
                    handleApiResponse res (always "An error has occured") (withGlobalError (notWaiting model)) <|
                        \processResult ->
                            case processResult of
                                NotYourAccountToModify ->
                                    withError (notWaiting model) DepositError [ "Not your account!" ]

                                NotEnoughBalance ->
                                    withError (notWaiting model) DepositError [ "Not enough balance" ]

                                AccountClosed Yours ->
                                    withError (notWaiting model) DepositError [ "Your account was already closed" ]

                                AccountClosed Theirs ->
                                    withError (notWaiting model) DepositError [ "Their account was closed" ]

                                AccountDoesNotExist Yours ->
                                    withError (notWaiting model) DepositError [ "Your account does not exist" ]

                                AccountDoesNotExist Theirs ->
                                    withError (notWaiting model) DepositError [ "Their account does not exist" ]

                                AccountOk ->
                                    case model.view.accountView.opened of
                                        Nothing ->
                                            ( model, Cmd.none )

                                        Just acc ->
                                            ( model, Api.loadAccount acc.id CurrentAccountReloaded sessionToken )

        TransferToSend ->
            case model.data.sessionToken of
                Nothing ->
                    ( model, Cmd.none )

                Just sessionToken ->
                    case model.view.accountView.opened of
                        Nothing ->
                            ( model, Cmd.none )

                        Just accData ->
                            ( model
                            , Api.processEvent accData.id TransferToResponse sessionToken <| TransferToIn model.view.accountView.transferAccount model.view.accountView.transferAmount
                            )

        TransferToResponse res ->
            case model.data.sessionToken of
                Nothing ->
                    ( model, Cmd.none )

                Just sessionToken ->
                    handleApiResponse res (always "An error has occured") (withGlobalError (notWaiting model)) <|
                        \processResult ->
                            case processResult of
                                NotYourAccountToModify ->
                                    withError (notWaiting model) TransferToError [ "Not your account!" ]

                                NotEnoughBalance ->
                                    withError (notWaiting model) TransferToError [ "Not enough balance" ]

                                AccountClosed Yours ->
                                    withError (notWaiting model) TransferToError [ "Your account was already closed" ]

                                AccountClosed Theirs ->
                                    withError (notWaiting model) TransferToError [ "Their account was closed" ]

                                AccountDoesNotExist Yours ->
                                    withError (notWaiting model) TransferToError [ "Your account does not exist" ]

                                AccountDoesNotExist Theirs ->
                                    withError (notWaiting model) TransferToError [ "Their account does not exist" ]

                                AccountOk ->
                                    case model.view.accountView.opened of
                                        Nothing ->
                                            ( model, Cmd.none )

                                        Just acc ->
                                            ( model, Api.loadAccount acc.id CurrentAccountReloaded sessionToken )


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
            , zIndex (int 10)
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


globalError : Model -> List (Html msg)
globalError model =
    case model.data.messages of
        GlobalError errors ->
            [ div
                [ css
                    [ left (px 0)
                    , right (px 0)
                    , backgroundColor <| hex "#af0000"
                    ]
                ]
                (h1 [] [ text "ERRORS" ] :: (List.intersperse (br [] []) <| List.map text errors))
            ]

        _ ->
            []


body : Model -> List (Html Msg)
body model =
    [ header model
    , div [] <| globalError model
    , Html.main_ []
        (case model.view.visiblePage of
            LoginPage ->
                loginForm model

            AccountsPage ->
                accountsPage model
        )
    ]


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
accountsPage model =
    let
        isSelected account =
            case model.view.accountView.opened of
                Nothing ->
                    False

                Just sel ->
                    account == sel

        isClosed account =
            case List.head account.history of
                Nothing ->
                    False

                Just h ->
                    h.event == Closed

        sidebarEntry : AccountData -> Html Msg
        sidebarEntry account =
            div
                [ css
                    ([ if isSelected account then
                        backgroundColor (hex "#9a9a9a")

                       else if isClosed account then
                        backgroundColor (hex "#4f4f4f")

                       else
                        backgroundColor (hex "#c9c9c9")
                     , cursor pointer
                     , padding2 (px 5) (px 0)
                     ]
                        ++ (if isClosed account then
                                [ color <| hex "#d2d2d2" ]

                            else
                                []
                           )
                    )
                , onClick (SelectAccount account)
                ]
                [ text <|
                    account.name
                        ++ " ("
                        ++ (if isClosed account then
                                "Closed"

                            else
                                String.fromInt account.balance
                           )
                        ++ ")"
                ]

        sidebar =
            nav
                [ css
                    [ C.width (vw 20)
                    , top (px 32)
                    , bottom (px 0)
                    , zIndex (int -1)
                    , position absolute
                    , borderRight3 (px 2) solid (hex "#7f7f7f")
                    ]
                ]
                (if Dict.isEmpty model.data.loggedInData.myAccounts then
                    [ Html.i [] [ text "No accounts..." ] ]

                 else
                    List.map sidebarEntry <| Dict.values model.data.loggedInData.myAccounts
                )

        mainContent =
            div
                [ css
                    [ left (vw 20)
                    , right (px 0)
                    , position absolute
                    ]
                ]
                (createNewAccountForm model
                    :: (case model.view.accountView.opened of
                            Nothing ->
                                [ h2 [] [ text "No account loaded" ] ]

                            Just acc ->
                                if isClosed acc then
                                    [ accountHistory model acc ]

                                else
                                    List.map (\x -> x model acc) [ closeForm, withdrawForm, depositForm, transferForm, accountHistory ]
                       )
                )
    in
    [ sidebar, mainContent ]


createNewAccountForm : Model -> Html Msg
createNewAccountForm model =
    div [ css [ margin2 (px 10) (px 0) ] ]
        [ input model [ type_ "text", name "newAccountName", placeholder "New Account name...", value model.view.accountView.newAccountName, onInput NewAccountName ]
        , centeredButton model [ onClick NewAccountSend ] [ text "Create Account" ]
        ]


closeForm : Model -> AccountData -> Html Msg
closeForm model _ =
    div []
        [ centeredButton model [ onClick CloseAccountSend ] [ text "Close Account" ]
        ]


withdrawForm : Model -> AccountData -> Html Msg
withdrawForm model accountData =
    div []
        [ input model [ type_ "text", placeholder "Amount", value <| String.fromInt model.view.accountView.withdrawAmount, onInput WithdrawAmount ]
        , centeredButton model [ onClick WithdrawSend ] [ text "Withdraw" ]
        ]


depositForm : Model -> AccountData -> Html Msg
depositForm model accountData =
    div []
        [ input model [ type_ "text", placeholder "Amount", value <| String.fromInt model.view.accountView.depositAmount, onInput DepositAmount ]
        , centeredButton model [ onClick DepositSend ] [ text "Deposit" ]
        ]


transferForm : Model -> AccountData -> Html Msg
transferForm model accountData =
    let
        options =
            List.map
                (\( ( _, username ), accs ) ->
                    optgroup [ Att.property "label" (Json.string username) ]
                        (List.map
                            (\( accId, accName ) ->
                                option
                                    [ value accId, selected (accId == model.view.accountView.transferAccount) ]
                                    [ text accName ]
                            )
                            accs
                        )
                )
                (Dict.toList model.data.loggedInData.allAccounts)
    in
    div []
        [ select [ value model.view.accountView.transferAccount, onInput TransferToAccount ]
            (option [ Att.disabled True, value "" ] [ text "Select..." ] :: options)
        , input model [ type_ "text", placeholder "Amount", value <| String.fromInt model.view.accountView.transferAmount, onInput TransferToAmount ]
        , centeredButton model [ onClick TransferToSend ] [ text "Transfer" ]
        ]


accountHistory : Model -> AccountData -> Html Msg
accountHistory model accountData =
    let
        historyRow : TimedEvent -> Html Msg
        historyRow timedEvent =
            tr []
                [ td [] [ text <| toIsoString <| timedEvent.time ]
                , td []
                    [ text <|
                        case timedEvent.event of
                            Opened name ->
                                "Opened " ++ name

                            Deposited amount ->
                                "Deposited " ++ String.fromInt amount

                            Withdrew amount ->
                                "Withdrew " ++ String.fromInt amount

                            TransferTo accountId amount ->
                                "Transferred " ++ String.fromInt amount ++ " to " ++ accountId

                            TransferFrom accountId amount ->
                                "Transferred " ++ String.fromInt amount ++ " from " ++ accountId

                            Closed ->
                                "Closed Account."
                    ]
                ]
    in
    Html.table
        [ css
            [ C.width (pct 80)
            , margin2 (px 0) (pct 8)
            ]
        ]
        (List.map historyRow accountData.history)


toIsoString : Time.Posix -> String
toIsoString time =
    let
        monthToInt month =
            case month of
                Jan ->
                    1

                Feb ->
                    2

                Mar ->
                    3

                Apr ->
                    4

                May ->
                    5

                Jun ->
                    6

                Jul ->
                    7

                Aug ->
                    8

                Sep ->
                    9

                Oct ->
                    10

                Nov ->
                    11

                Dec ->
                    12
    in
    (String.padLeft 4 '0' <| String.fromInt (toYear utc time))
        ++ "-"
        ++ (String.padLeft 2 '0' <| String.fromInt <| monthToInt (toMonth utc time))
        ++ "-"
        ++ (String.padLeft 2 '0' <| String.fromInt (toDay utc time))
        ++ " "
        ++ (String.padRight 2 '0' <| String.fromInt (toHour utc time))
        ++ ":"
        ++ (String.padRight 2 '0' <| String.fromInt (toMinute utc time))
        ++ ":"
        ++ (String.padRight 2 '0' <| String.fromInt (toSecond utc time))


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
        , subscriptions = subscriptions
        }
