module Session exposing (Session(..), Cred, getNavKey,getCred,isLoggedIn)

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

getCred : Session -> Maybe Cred
getCred session =
    case session of
        Authenticated _ cred ->
            Just cred

        Unauthenticated _ ->
            Nothing

isLoggedIn : Session -> Bool
isLoggedIn session =
    case session of
        Authenticated _ _ -> True

        Unauthenticated _ -> False



-- changes (Session -> msg) -> Nav.Key -> Sub msg
-- changes toMsg key =
