port module Utils.Session exposing (getNavKey,getCred,isLoggedIn,store,logout,storeV,changes,onStoreChange,decodeCred)

import Browser.Navigation as Nav
import Json.Decode        as Decode      exposing (Decoder, Value, decodeString, field, string)
import Utils.Types exposing (..)
import Utils.Transcoder exposing (..)

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

-- Viewer
store : Cred -> Cmd msg
store cred = storeCredWith cred -- TODO: this process with clean up this will be more separated into different files




storeCredWith : Cred -> Cmd msg
storeCredWith cred =
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


logout : Cmd msg
logout = 
    storeV Nothing


port storeV : Maybe Value -> Cmd msg

port onStoreChange : (String -> msg) -> Sub msg


changes : (Session -> msg) -> Nav.Key -> Sub msg
changes toMsg key =
     onStoreChange (\value -> toMsg (decodeCred key value)  )


decodeCred : Nav.Key -> String -> Session
decodeCred key value =
    case Decode.decodeString  credDecoder value of
        Ok cred ->
            Authenticated key cred

        Err _ ->

            Unauthenticated key
