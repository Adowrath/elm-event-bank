module Generated.Types exposing (..)

import Time exposing (Posix)


type alias AccountOpen =
    { accountOpen : String
    }



type AccountEventIn
    = DepositedIn Int
    | WithdrewIn Int
    | TransferToIn String Int

type AccountEvent
    = Opened String
    | Deposited Int
    | Withdrew Int
    | TransferTo String Int
    | TransferFrom String Int
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
    | NotYourAccountToModify
    | NotEnoughBalance
    | AccountClosed Whose
    | AccountDoesNotExist Whose

type alias AccountData =
    { id : String
    , name : String
    , balance : Int
    , history : List TimedEvent
    }

type AccountLoadResult
    = NoAccountFound
    | NotYourAccount
    | LoadResult AccountData

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
