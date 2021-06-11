module Generated.Decoder exposing (..)

import Iso8601 as Iso
import Json.Decode as D exposing (..)
import Json.Decode.Pipeline as D exposing (required)

import Generated.ElmStreet exposing (..)
import Generated.Types as T


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
