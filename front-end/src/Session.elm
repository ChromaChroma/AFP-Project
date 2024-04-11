port module Session exposing (getNavKey,getCred,isLoggedIn,store,logout,storeV,changes,onStoreChange,credDecoder,decodeCred)

import Browser.Navigation as Nav
import Json.Encode        as Encode 
import Json.Decode        as Decode      exposing (Decoder, Value, decodeString, field, string)
import Utils.Types exposing (..)

-- TYPES

-- type alias Cred =
--     { access  : String
--     , refresh : String   
--     }

-- type Session
--     = Authenticated   Nav.Key Cred
--     | Unauthenticated Nav.Key

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


userEncoder : Cred -> Encode.Value
userEncoder cred = 
    Encode.object 
        [("access", Encode.string cred.access)
        ,("refesh", Encode.string cred.refresh)
        ]

storeCredWith : Cred -> Cmd msg
storeCredWith cred =
    let
        json = userEncoder cred
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

credDecoder : Decoder Cred
credDecoder =
  Decode.map2 Cred
    (Decode.field "access" Decode.string)
    (Decode.field "refresh" Decode.string)

-- viewerChanges : (Maybe Cred -> msg) -> Decoder (Cred -> cred) -> Sub msg
-- viewerChanges toMsg decoder =
--     onStoreChange (\value -> toMsg (decodeFromChange decoder value))

-- decodeFromChange : Decoder (Cred -> cred) -> Value -> Maybe cred
-- decodeFromChange viewerDecoder val =
--     -- It's stored in localStorage as a JSON String;
--     -- first decode the Value as a String, then
--     -- decode that String as JSON.
--     Decode.decodeValue (storageDecoder viewerDecoder) val
--         |> Result.toMaybe

changes : (Session -> msg) -> Nav.Key -> Sub msg
changes toMsg key =
    -- Api.viewerChanges (\maybeCred -> toMsg (fromViewer key maybeViewer)) credDecoder
     onStoreChange (\value -> toMsg (decodeCred key value)  )  |> Debug.log "changessssssssssssss"

-- fromViewer : Nav.Key -> Maybe Cred -> Session
-- fromViewer key maybeCred =
--     case cred of
--         Just cred ->
--             Authenticated key cred

--         Nothing ->
--             Unauthenticated key

decodeCred : Nav.Key -> String -> Session
decodeCred key value =
    case Decode.decodeString  credDecoder value of
        Ok cred ->
            -- Here you can use the decoded Cred value as needed
            -- For example, update the session or send a message
            -- Let's assume you send a message to update the session
            Authenticated key cred |> Debug.log "Authentic................."

        Err _ ->
            -- Handle decoding error if necessary
            -- For example, log an error message or send a default session
            -- Let's assume you send a message for a guest session
            Unauthenticated key |> Debug.log "Nothing................."
