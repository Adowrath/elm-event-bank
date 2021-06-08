module Generated.Decoder exposing (..)

import Iso8601 as Iso
import Json.Decode as D exposing (..)
import Json.Decode.Pipeline as D exposing (required)

import Generated.ElmStreet exposing (..)
import Generated.Types as T






decodeJwtTokensPair : Decoder T.JwtTokensPair
decodeJwtTokensPair = D.succeed T.JwtTokensPair
    |> required "session" D.string
    |> required "refresh" D.string

decodeSingleToken : Decoder T.SingleToken
decodeSingleToken = D.map T.SingleToken D.string

decodeLoginData : Decoder T.LoginData
decodeLoginData = D.succeed T.LoginData
    |> required "username" D.string
    |> required "password" D.string
