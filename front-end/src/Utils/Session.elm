port module Utils.Session exposing (getNavKey,getCred,isLoggedIn,store,logout,storeV,changes,onStoreChange,decodeCred)

import Browser.Navigation as Nav
import Json.Decode        as Decode exposing (Decoder, Value, decodeString, field, string)
import Utils.Types                  exposing (..)
import Utils.Transcoder             exposing (..)


{- INFO
    These functions are used to retrieve information about the session.
-}

{-| This function gets the navigation key from the current session.
-}
getNavKey : Session -> Nav.Key
getNavKey session =
    case session of
        Authenticated key _ ->
            key

        Unauthenticated key ->
            key

{-| This function gets the credentials from the current session,
    only when the session is authenticated, otherwise it returns Nothing.
-}
getCred : Session -> Maybe Cred
getCred session =
    case session of
        Authenticated _ cred ->
            Just cred

        Unauthenticated _ ->
            Nothing

{-| This function returns whether the current session is authenticated or not.
-}
isLoggedIn : Session -> Bool
isLoggedIn session =
    case session of
        Authenticated _ _ -> True

        Unauthenticated _ -> False


{- STORE DATA
    These function and ports handle storing data in local storages.
-}

{-| This function encodes the credentials and passes it through the store port.
-}
store : Cred -> Cmd msg
store cred =
    let
            json = credEncoder cred
                -- Encode.object
                --     [ ( "user"
                --       , Encode.object
                --             [ ( "acces", Encode.string cred.access )
                --             , ( "refresh", Encode.string cred.refresh )
                --             ]
                --       )
                --     ]
        in
        storeV (Just json)

{-| This function passes nothing through the store port,
    to indicate the current credentials should be removed from local storage.
-}
logout : Cmd msg
logout = 
    storeV Nothing

{-| This port is used to store data into local storage.
-}
port storeV : Maybe Value -> Cmd msg

{-| This port is used to handle on change events from local storage.
-}
port onStoreChange : (String -> msg) -> Sub msg

{-| This function is used to handle local storage session updates.
-}
changes : (Session -> msg) -> Nav.Key -> Sub msg
changes toMsg key =
     onStoreChange (\value -> toMsg (decodeCred key value)  )

{-| This function decodes credentials into an authenticated session,
    when thre are credentials, otherwise it creates an unauthenticated session. 
-}
decodeCred : Nav.Key -> String -> Session
decodeCred key value =
    case Decode.decodeString  credDecoder value of
        Ok cred ->
            Authenticated key cred

        Err _ ->

            Unauthenticated key