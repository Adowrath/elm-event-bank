module Generated.Encoder exposing (..)

import Iso8601 as Iso
import Json.Encode as E exposing (..)

import Generated.ElmStreet exposing (..)
import Generated.Types as T


encodeAccountOpen : T.AccountOpen -> Value
encodeAccountOpen = E.string << T.unAccountOpen



encodeAccountEventIn : T.AccountEventIn -> Value
encodeAccountEventIn x = E.object <| case x of
    T.DepositedIn x1 -> [("tag", E.string "DepositedIn"), ("contents", E.int x1)]
    T.WithdrewIn x1 -> [("tag", E.string "WithdrewIn"), ("contents", E.int x1)]
    T.TransferToIn x1 x2 -> [("tag", E.string "TransferToIn"), ("contents", E.list identity [E.string x1, E.int x2])]

encodeAccountEvent : T.AccountEvent -> Value
encodeAccountEvent x = E.object <| case x of
    T.Opened x1 -> [("tag", E.string "Opened"), ("contents", E.string x1)]
    T.Deposited x1 -> [("tag", E.string "Deposited"), ("contents", E.int x1)]
    T.Withdrew x1 -> [("tag", E.string "Withdrew"), ("contents", E.int x1)]
    T.TransferTo x1 x2 -> [("tag", E.string "TransferTo"), ("contents", E.list identity [E.string x1, E.int x2])]
    T.TransferFrom x1 x2 -> [("tag", E.string "TransferFrom"), ("contents", E.list identity [E.string x1, E.int x2])]
    T.Closed  -> [("tag", E.string "Closed"), ("contents", E.list identity [])]

encodeTimedEvent : T.TimedEvent -> Value
encodeTimedEvent x = E.object
    [ ("tag", E.string "TimedEvent")
    , ("time", Iso.encode x.time)
    , ("event", encodeAccountEvent x.event)
    ]



encodeWhose : T.Whose -> Value
encodeWhose = E.string << T.showWhose

encodeAccountProcessResult : T.AccountProcessResult -> Value
encodeAccountProcessResult x = E.object <| case x of
    T.AccountOk  -> [("tag", E.string "AccountOk"), ("contents", E.list identity [])]
    T.NotYourAccountToModify  -> [("tag", E.string "NotYourAccountToModify"), ("contents", E.list identity [])]
    T.NotEnoughBalance  -> [("tag", E.string "NotEnoughBalance"), ("contents", E.list identity [])]
    T.AccountClosed x1 -> [("tag", E.string "AccountClosed"), ("contents", encodeWhose x1)]
    T.AccountDoesNotExist x1 -> [("tag", E.string "AccountDoesNotExist"), ("contents", encodeWhose x1)]

encodeAccountData : T.AccountData -> Value
encodeAccountData x = E.object
    [ ("tag", E.string "AccountData")
    , ("id", E.string x.id)
    , ("name", E.string x.name)
    , ("balance", E.int x.balance)
    , ("history", (E.list encodeTimedEvent) x.history)
    ]

encodeAccountLoadResult : T.AccountLoadResult -> Value
encodeAccountLoadResult x = E.object <| case x of
    T.NoAccountFound  -> [("tag", E.string "NoAccountFound"), ("contents", E.list identity [])]
    T.NotYourAccount  -> [("tag", E.string "NotYourAccount"), ("contents", E.list identity [])]
    T.LoadResult x1 -> [("tag", E.string "LoadResult"), ("contents", encodeAccountData x1)]

encodeLoginData : T.LoginData -> Value
encodeLoginData x = E.object
    [ ("tag", E.string "LoginData")
    , ("username", E.string x.username)
    , ("password", E.string x.password)
    ]

encodeAuthError : T.AuthError -> Value
encodeAuthError x = E.object <| case x of
    T.UnknownUser  -> [("tag", E.string "UnknownUser"), ("contents", E.list identity [])]
    T.UsernameTaken  -> [("tag", E.string "UsernameTaken"), ("contents", E.list identity [])]
    T.WrongPassword  -> [("tag", E.string "WrongPassword"), ("contents", E.list identity [])]
    T.NotLoggedIn  -> [("tag", E.string "NotLoggedIn"), ("contents", E.list identity [])]
    T.SessionLoggedOut  -> [("tag", E.string "SessionLoggedOut"), ("contents", E.list identity [])]
    T.NotBearerAuthenticated  -> [("tag", E.string "NotBearerAuthenticated"), ("contents", E.list identity [])]
    T.UserNoLongerExists  -> [("tag", E.string "UserNoLongerExists"), ("contents", E.list identity [])]
    T.LoginExpired  -> [("tag", E.string "LoginExpired"), ("contents", E.list identity [])]
    T.AuthTokenError x1 -> [("tag", E.string "AuthTokenError"), ("contents", encodeTokenError x1)]





encodeJwtTokensPair : T.JwtTokensPair -> Value
encodeJwtTokensPair x = E.object
    [ ("tag", E.string "JwtTokensPair")
    , ("session", E.string x.session)
    , ("refresh", E.string x.refresh)
    ]

encodeSingleToken : T.SingleToken -> Value
encodeSingleToken x = E.object
    [ ("tag", E.string "SingleToken")
    , ("token", E.string x.token)
    ]

encodeTokenError : T.TokenError -> Value
encodeTokenError x = E.object <| case x of
    T.TokenMalformed x1 -> [("tag", E.string "TokenMalformed"), ("contents", E.string x1)]
    T.TokenUnsigned  -> [("tag", E.string "TokenUnsigned"), ("contents", E.list identity [])]
    T.TokenWrongType  -> [("tag", E.string "TokenWrongType"), ("contents", E.list identity [])]
    T.TokenExpired  -> [("tag", E.string "TokenExpired"), ("contents", E.list identity [])]
