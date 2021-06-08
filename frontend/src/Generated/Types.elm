module Generated.Types exposing (..)

import Time exposing (Posix)






type alias JwtTokensPair =
    { session : String
    , refresh : String
    }

type alias SingleToken =
    { token : String
    }

type alias LoginData =
    { username : String
    , password : String
    }
