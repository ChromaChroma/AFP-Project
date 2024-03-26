{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE ViewPatterns       #-}
{-# LANGUAGE DeriveAnyClass    #-}

module Security.Handlers 
  ( LoginRequest(..)
  , LoginResponse(..)
  , getUserHandler
  , loginHandler
  , refreshTokenHandler
  ) where

-- | Dependency imports
import Crypto.JWT                (SignedJWT)
import Crypto.JOSE               (JWK, encodeCompact)
import Control.Monad             (when, unless)
import Control.Monad.Catch       (MonadThrow(..))
import Control.Monad.IO.Class    (liftIO, MonadIO)
import Data.Aeson                (FromJSON, ToJSON)
import Data.ByteString.Lazy.UTF8 (toString)
import Data.Text                 (Text, pack)
import Data.Time                 (getCurrentTime)
import Data.UUID                 (nil, UUID, fromString)
import GHC.Generics              (Generic)
import Servant                   (err401, err404)
-- | Project imports
import Dummy                     (dummyUUID)
import Security.Auth             (signToken)
import Security.User             (User(..))
import Security.Claims           (RefreshClaims, accessClaims, refreshClaims, subjectClaim)


data LoginRequest = LoginRequest
  { username :: !Text
  , password :: !Text
  }
  deriving (Generic, FromJSON)

data LoginResponse = LoginResponse
  { access :: !Text
  , refresh :: !Text
  }
  deriving (Generic, ToJSON)

loginResponse :: (ToJSON a, ToJSON b, MonadThrow m, MonadIO m) 
              => JWK -> a -> b -> m LoginResponse
loginResponse jwk acc refr = do
  signedAccess  <- liftIO (signToken jwk acc)
  signedRefresh <- liftIO (signToken jwk refr)
  case (signedAccess, signedRefresh) of
    (Just aToken, Just rToken) -> return $ LoginResponse (toText aToken) (toText rToken)
    _                          -> throwM err401

toText :: SignedJWT -> Text
toText = pack . toString . encodeCompact

loginHandler :: (MonadThrow m, MonadIO m) => JWK -> LoginRequest -> m LoginResponse
loginHandler jwk LoginRequest {..} = case attemptLogin username password of 
  Just _  -> do
    now <- liftIO getCurrentTime
    loginResponse jwk (accessClaims dummyUUID now) (refreshClaims dummyUUID now)
  _       -> throwM err401
  where
    -- | TODO: Attempt actual login check
    attemptLogin :: Text -> Text -> Maybe User
    attemptLogin "user" "12345" = Just (User username "SomeEmail")
    attemptLogin _ _            = Nothing

refreshTokenHandler :: (MonadThrow m, MonadIO m) => JWK -> Maybe RefreshClaims -> m LoginResponse
refreshTokenHandler jwk (Just claims@(subjectClaim -> Just uid)) = do
  now <- liftIO getCurrentTime
  loginResponse jwk (accessClaims uid now) claims
  
refreshTokenHandler _ _ = throwM err401

getUserHandler :: MonadThrow m => UUID -> m User
getUserHandler uid
  | uid /= nil = pure (User "user" "user@mail.com") --TODO change to actual user data 
  | otherwise  = throwM err404