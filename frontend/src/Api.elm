module Api exposing (..)

import ApiImpl exposing (..)
import Generated.Decoder exposing (decodeAccountLoadResult, decodeAccountProcessResult, decodeAuthError, decodeJwtTokensPair)
import Generated.ElmStreet exposing (elmStreetDecodePair)
import Generated.Encoder exposing (encodeAccountEventIn, encodeAccountOpen, encodeLoginData, encodeSingleToken)
import Generated.Types exposing (AccountEventIn, AccountLoadResult, AccountOpen, AccountProcessResult, AuthError, JwtTokensPair, LoginData, SingleToken, TokenError)
import Http as Http exposing (Expect, expectStringResponse, header)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)



---------- AUTH ROUTES ----------


type alias AuthPost answer data msg =
    ApiPost AuthError answer data msg


type alias SecureAuthPost answer data msg =
    SecurePost AuthError answer data msg


type alias SecureAuthGet answer msg =
    SecureGet AuthError answer msg


authPost : String -> Encoder data -> Decoder answer -> AuthPost answer data msg
authPost =
    apiPost decodeAuthError


sauthPost : String -> Encoder data -> Decoder answer -> SecureAuthPost answer data msg
sauthPost =
    sapiPost decodeAuthError


sauthGet : String -> Decoder answer -> SecureAuthGet answer msg
sauthGet =
    sapiGet decodeAuthError


type alias UserId =
    String


authLogin : AuthPost JwtTokensPair LoginData msg
authLogin =
    authPost "/api/auth/login" encodeLoginData decodeJwtTokensPair


authCreate : AuthPost () LoginData msg
authCreate =
    authPost "/api/auth/create" encodeLoginData <| D.succeed ()


authRefresh : AuthPost Token SingleToken msg
authRefresh =
    authPost "/api/auth/refresh" encodeSingleToken D.string


authLogout : SecureAuthPost () () msg
authLogout =
    sauthPost "/api/auth/logout" (\_ -> E.object []) <| D.succeed ()


authAllUsers : SecureAuthPost (List ( UserId, String )) () msg
authAllUsers =
    sauthPost "/api/users" (\_ -> E.object []) <| D.list <| elmStreetDecodePair D.string D.string



---------- ACCOUNT ROUTES ----------


type alias AccountId =
    String


accountOpen : SecurePost () AccountId AccountOpen msg
accountOpen =
    sapiPost (D.succeed ()) "/api/account/open" encodeAccountOpen D.string


closeAccount : AccountId -> SecurePost () Bool () msg
closeAccount id =
    sapiPost (D.succeed ()) ("/api/account/" ++ id ++ "/close") (\_ -> E.object []) D.bool


myAccounts : SecureGet () (List ( AccountId, String )) msg
myAccounts =
    sapiGet (D.succeed ()) "/api/account" (D.list <| elmStreetDecodePair D.string D.string)


accountsBy : String -> SecureGet () (List ( AccountId, String )) msg
accountsBy id =
    sapiGet (D.succeed ()) ("/api/account/by/" ++ id) (D.list <| elmStreetDecodePair D.string D.string)


loadAccount : String -> SecureGet () AccountLoadResult msg
loadAccount id =
    sapiGet (D.succeed ()) ("/api/account/" ++ id) decodeAccountLoadResult


processEvent : String -> SecurePost () AccountProcessResult AccountEventIn msg
processEvent id =
    sapiPost (D.succeed ()) ("/api/account/" ++ id) encodeAccountEventIn decodeAccountProcessResult
