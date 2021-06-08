module Generated.Encoder exposing (..)

import Iso8601 as Iso
import Json.Encode as E exposing (..)

import Generated.ElmStreet exposing (..)
import Generated.Types as T






encodeJwtTokensPair : T.JwtTokensPair -> Value
encodeJwtTokensPair x = E.object
    [ ("tag", E.string "JwtTokensPair")
    , ("session", E.string x.session)
    , ("refresh", E.string x.refresh)
    ]

encodeSingleToken : T.SingleToken -> Value
encodeSingleToken x = E.string x.token

encodeLoginData : T.LoginData -> Value
encodeLoginData x = E.object
    [ ("tag", E.string "LoginData")
    , ("username", E.string x.username)
    , ("password", E.string x.password)
    ]
