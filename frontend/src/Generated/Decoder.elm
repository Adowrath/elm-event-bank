module Generated.Decoder exposing (..)

import Iso8601 as Iso
import Json.Decode as D exposing (..)
import Json.Decode.Pipeline as D exposing (required)

import Generated.ElmStreet exposing (..)
import Generated.Types as T


decodeAccountOpen : Decoder T.AccountOpen
decodeAccountOpen = D.succeed T.AccountOpen
    |> required "accountOpen" D.string



decodeAccountEventIn : Decoder T.AccountEventIn
decodeAccountEventIn =
    let decide : String -> Decoder T.AccountEventIn
        decide x = case x of
            "DepositedIn" -> D.field "contents" <| D.map T.DepositedIn D.int
            "WithdrewIn" -> D.field "contents" <| D.map T.WithdrewIn D.int
            "TransferToIn" -> D.field "contents" <| D.map2 T.TransferToIn (D.index 0 D.string) (D.index 1 D.int)
            c -> D.fail <| "AccountEventIn doesn't have such constructor: " ++ c
    in D.andThen decide (D.field "tag" D.string)

decodeAccountEvent : Decoder T.AccountEvent
decodeAccountEvent =
    let decide : String -> Decoder T.AccountEvent
        decide x = case x of
            "Opened" -> D.field "contents" <| D.map T.Opened D.string
            "Deposited" -> D.field "contents" <| D.map T.Deposited D.int
            "Withdrew" -> D.field "contents" <| D.map T.Withdrew D.int
            "TransferTo" -> D.field "contents" <| D.map2 T.TransferTo (D.index 0 D.string) (D.index 1 D.int)
            "TransferFrom" -> D.field "contents" <| D.map2 T.TransferFrom (D.index 0 D.string) (D.index 1 D.int)
            "Closed" -> D.succeed T.Closed
            c -> D.fail <| "AccountEvent doesn't have such constructor: " ++ c
    in D.andThen decide (D.field "tag" D.string)

decodeTimedEvent : Decoder T.TimedEvent
decodeTimedEvent = D.succeed T.TimedEvent
    |> required "time" Iso.decoder
    |> required "event" decodeAccountEvent



decodeWhose : Decoder T.Whose
decodeWhose = elmStreetDecodeEnum T.readWhose

decodeAccountProcessResult : Decoder T.AccountProcessResult
decodeAccountProcessResult =
    let decide : String -> Decoder T.AccountProcessResult
        decide x = case x of
            "AccountOk" -> D.succeed T.AccountOk
            "NotYourAccountToModify" -> D.succeed T.NotYourAccountToModify
            "NotEnoughBalance" -> D.succeed T.NotEnoughBalance
            "AccountClosed" -> D.field "contents" <| D.map T.AccountClosed decodeWhose
            "AccountDoesNotExist" -> D.field "contents" <| D.map T.AccountDoesNotExist decodeWhose
            c -> D.fail <| "AccountProcessResult doesn't have such constructor: " ++ c
    in D.andThen decide (D.field "tag" D.string)

decodeAccountData : Decoder T.AccountData
decodeAccountData = D.succeed T.AccountData
    |> required "id" D.string
    |> required "name" D.string
    |> required "balance" D.int
    |> required "history" (D.list decodeTimedEvent)

decodeAccountLoadResult : Decoder T.AccountLoadResult
decodeAccountLoadResult =
    let decide : String -> Decoder T.AccountLoadResult
        decide x = case x of
            "NoAccountFound" -> D.succeed T.NoAccountFound
            "NotYourAccount" -> D.succeed T.NotYourAccount
            "LoadResult" -> D.field "contents" <| D.map T.LoadResult decodeAccountData
            c -> D.fail <| "AccountLoadResult doesn't have such constructor: " ++ c
    in D.andThen decide (D.field "tag" D.string)

decodeLoginData : Decoder T.LoginData
decodeLoginData = D.succeed T.LoginData
    |> required "username" D.string
    |> required "password" D.string

decodeAuthError : Decoder T.AuthError
decodeAuthError =
    let decide : String -> Decoder T.AuthError
        decide x = case x of
            "UnknownUser" -> D.succeed T.UnknownUser
            "UsernameTaken" -> D.succeed T.UsernameTaken
            "WrongPassword" -> D.succeed T.WrongPassword
            "NotLoggedIn" -> D.succeed T.NotLoggedIn
            "SessionLoggedOut" -> D.succeed T.SessionLoggedOut
            "NotBearerAuthenticated" -> D.succeed T.NotBearerAuthenticated
            "UserNoLongerExists" -> D.succeed T.UserNoLongerExists
            "LoginExpired" -> D.succeed T.LoginExpired
            "AuthTokenError" -> D.field "contents" <| D.map T.AuthTokenError decodeTokenError
            c -> D.fail <| "AuthError doesn't have such constructor: " ++ c
    in D.andThen decide (D.field "tag" D.string)





decodeJwtTokensPair : Decoder T.JwtTokensPair
decodeJwtTokensPair = D.succeed T.JwtTokensPair
    |> required "session" D.string
    |> required "refresh" D.string

decodeSingleToken : Decoder T.SingleToken
decodeSingleToken = D.succeed T.SingleToken
    |> required "token" D.string

decodeTokenError : Decoder T.TokenError
decodeTokenError =
    let decide : String -> Decoder T.TokenError
        decide x = case x of
            "TokenMalformed" -> D.field "contents" <| D.map T.TokenMalformed D.string
            "TokenUnsigned" -> D.succeed T.TokenUnsigned
            "TokenWrongType" -> D.succeed T.TokenWrongType
            "TokenExpired" -> D.succeed T.TokenExpired
            c -> D.fail <| "TokenError doesn't have such constructor: " ++ c
    in D.andThen decide (D.field "tag" D.string)
