module ApiImpl exposing (..)

import Generated.Decoder exposing (decodeAuthError, decodeJwtTokensPair)
import Generated.Encoder exposing (encodeLoginData, encodeSingleToken)
import Generated.Types exposing (AuthError, JwtTokensPair, LoginData, SingleToken, TokenError)
import Http as Http exposing (Expect, expectStringResponse, header)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)


type ApiResponse e a
    = ApiOk a
    | UserError e
    | OtherError String
    | ConnectionError Http.Error


apiResponseDecoder : Decoder a -> Decoder e -> Decoder (ApiResponse e a)
apiResponseDecoder value error =
    let
        decide : String -> Decoder (ApiResponse e a)
        decide res =
            case res of
                "ok" ->
                    D.field "data" <| D.map ApiOk value

                "error" ->
                    D.field "data" <| D.map UserError error

                "other-error" ->
                    D.field "data" <| D.map OtherError D.string

                _ ->
                    D.fail <| "Unknown result type."
    in
    D.andThen decide (D.field "result" D.string)


expectApi : (ApiResponse e a -> msg) -> Decoder a -> Decoder e -> Expect msg
expectApi handler valueDec errorDec =
    let
        unwrapHttpError : Result Http.Error (ApiResponse e a) -> msg
        unwrapHttpError res =
            case res of
                Ok a ->
                    handler a

                Err e ->
                    handler (ConnectionError e)

        tryParseBody : String -> Decoder b -> Result Http.Error b
        tryParseBody body dec =
            case D.decodeString dec body of
                Ok value ->
                    Ok value

                Err err ->
                    Err <| Http.BadBody (D.errorToString err)
    in
    expectStringResponse unwrapHttpError <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (Http.BadUrl url)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ _ body ->
                    tryParseBody body (apiResponseDecoder valueDec errorDec)

                Http.GoodStatus_ _ body ->
                    tryParseBody body (apiResponseDecoder valueDec errorDec)


apiPost : Decoder e -> String -> Encoder i -> Decoder a -> (ApiResponse e a -> msg) -> i -> Cmd msg
apiPost errDecoder url dataEncoder resDecoder resHandler data =
    Http.request
        { method = "POST"
        , headers = []
        , url = url
        , body = Http.jsonBody (dataEncoder data)
        , expect = expectApi resHandler resDecoder errDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


sapiPost : Decoder e -> String -> Encoder i -> Decoder a -> (ApiResponse e a -> msg) -> Token -> i -> Cmd msg
sapiPost errDecoder url dataEncoder resDecoder resHandler token data =
    Http.request
        { method = "POST"
        , headers = [ header "Authorization" ("Bearer " ++ token) ]
        , url = url
        , body = Http.jsonBody (dataEncoder data)
        , expect = expectApi resHandler resDecoder errDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


apiGet : Decoder e -> String -> Decoder a -> (ApiResponse e a -> msg) -> Cmd msg
apiGet errDecoder url resDecoder resHandler =
    Http.request
        { method = "GET"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , expect = expectApi resHandler resDecoder errDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


sapiGet : Decoder e -> String -> Decoder a -> (ApiResponse e a -> msg) -> Token -> Cmd msg
sapiGet errDecoder url resDecoder resHandler token =
    Http.request
        { method = "GET"
        , headers = [ header "Authorization" ("Bearer " ++ token) ]
        , url = url
        , body = Http.emptyBody
        , expect = expectApi resHandler resDecoder errDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


type alias Encoder a =
    a -> Value


type alias Token =
    String


type alias ApiPost err answer data msg =
    (ApiResponse err answer -> msg) -> data -> Cmd msg


type alias SecurePost err answer data msg =
    (ApiResponse err answer -> msg) -> Token -> data -> Cmd msg


type alias ApiGet err answer msg =
    (ApiResponse err answer -> msg) -> Cmd msg


type alias SecureGet err answer msg =
    (ApiResponse err answer -> msg) -> Token -> Cmd msg
