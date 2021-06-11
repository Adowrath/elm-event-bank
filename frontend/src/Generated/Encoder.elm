module Generated.Encoder exposing (..)

import Iso8601 as Iso
import Json.Encode as E exposing (..)

import Generated.ElmStreet exposing (..)
import Generated.Types as T


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
