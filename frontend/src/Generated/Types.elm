module Generated.Types exposing (..)

import Time exposing (Posix)


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
