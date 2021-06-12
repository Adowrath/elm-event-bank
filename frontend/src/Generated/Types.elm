module Generated.Types exposing (..)

import Time exposing (Posix)


type AccountEvent
    = Opened String
    | Deposited Int
    | Withdrew Int
    | TransferFrom String Int
    | TransferTo String Int
    | Closed

type alias TimedEvent =
    { time : Posix
    , event : AccountEvent
    }



type Whose
    = Yours
    | Theirs

showWhose : Whose -> String
showWhose x = case x of
    Yours -> "Yours"
    Theirs -> "Theirs"

readWhose : String -> Maybe Whose
readWhose x = case x of
    "Yours" -> Just Yours
    "Theirs" -> Just Theirs
    _ -> Nothing

universeWhose : List Whose
universeWhose = [Yours, Theirs]

type AccountProcessResult
    = AccountOk
    | NotEnoughBalance
    | AccountClosed Whose
    | AccountDoesNotExist Whose

type alias LoginData =
    { username : String
    , password : String
    }

type AuthError
    = UnknownUser
    | UsernameTaken
    | WrongPassword
    | NotLoggedIn
    | SessionLoggedOut
    | NotBearerAuthenticated
    | UserNoLongerExists
    | LoginExpired
    | AuthTokenError TokenError





type alias JwtTokensPair =
    { session : String
    , refresh : String
    }

type alias SingleToken =
    { token : String
    }

type TokenError
    = TokenMalformed String
    | TokenUnsigned
    | TokenWrongType
    | TokenExpired
