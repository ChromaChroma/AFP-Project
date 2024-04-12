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
import Control.Monad.Catch       (MonadThrow(..))
import Control.Monad.IO.Class    (liftIO, MonadIO)
import Database.PostgreSQL.Simple(Connection)
import Data.Aeson                (FromJSON, ToJSON)
import Data.ByteString.Lazy.UTF8 (toString)
import Data.Text                 (Text, pack)
import Data.Time                 (getCurrentTime)
import Data.UUID                 (nil, UUID, fromString)
import GHC.Generics              (Generic)
import Servant                   (err401, err404)
-- | Project imports
import Database.Repository       (authenticateUser, getUserById)
import Security.Auth             (signToken)
import Types.User                (User(..), UserId)
import Security.Claims           (RefreshClaims, accessClaims, refreshClaims, subjectClaim)

-- | Data transfer object for a login request
data LoginRequest = LoginRequest
  { username :: !Text
  , password :: !Text
  } deriving (Generic, FromJSON)

-- | Data transfer object for a login response
data LoginResponse = LoginResponse
  { access  :: !Text
  , refresh :: !Text
  } deriving (Generic, ToJSON)

-- | Build a login response with te provided JWT tokens
loginResponse :: (ToJSON a, ToJSON b, MonadThrow m, MonadIO m) 
              => JWK -> a -> b -> m LoginResponse
loginResponse jwk acc refr = do
  signedAccess  <- liftIO (signToken jwk acc)
  signedRefresh <- liftIO (signToken jwk refr)
  case (signedAccess, signedRefresh) of
    (Just aToken, Just rToken) -> return $ LoginResponse (toText aToken) (toText rToken)
    _                          -> throwM err401

-- | Utility function to convert a JWT token to text format, ready for JSON serialization.
toText :: SignedJWT -> Text
toText = pack . toString . encodeCompact

--
-- Handlers
--

-- | Implementation of 'API.login'
-- 
-- Returns a LoginResponse with the tokens
loginHandler :: (MonadThrow m, MonadIO m) => Connection -> JWK -> LoginRequest -> m LoginResponse
loginHandler conn jwk LoginRequest {..} =  do
  user <- liftIO $ authenticateUser conn username password
  now <- liftIO getCurrentTime
  let uid = _id user
  loginResponse jwk (accessClaims uid now) (refreshClaims uid now) 

-- | Implementation of 'API.refresh'
-- 
-- Returns a LoginResponse with the new tokens
refreshTokenHandler :: (MonadThrow m, MonadIO m) => JWK -> Maybe RefreshClaims -> m LoginResponse
refreshTokenHandler jwk (Just claims@(subjectClaim -> Just uid)) = do
  now <- liftIO getCurrentTime
  loginResponse jwk (accessClaims uid now) claims
refreshTokenHandler _ _ = throwM err401

-- | Returns the requested user.
getUserHandler :: (MonadThrow m, MonadIO m) => Connection -> UserId -> m User
getUserHandler conn uid = liftIO $ getUserById conn uid