module Session exposing (Session(..), Cred, getNavKey)

import Browser.Navigation as Nav


-- TYPES

type alias Cred =
    { access  : String
    , refresh : String   
    }

type Session
    = Authenticated   Nav.Key Cred
    | Unauthenticated Nav.Key

-- INFO

getNavKey : Session -> Nav.Key
getNavKey session =
    case session of
        Authenticated key _ ->
            key

        Unauthenticated key ->
            key