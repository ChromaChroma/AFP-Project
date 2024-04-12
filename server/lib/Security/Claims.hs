{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE InstanceSigs       #-}

module Security.Claims
  ( AccessClaims(..)
  , RefreshClaims(..)
  , accessClaims
  , accessSettings
  , refreshClaims
  , refreshSettings
  , subjectClaim
  , extractSub
  ) where

-- | Dependency imports
import Control.Monad.Catch         (MonadThrow(..))
import Control.Monad.IO.Class      (MonadIO)
import Control.Lens                (Lens', view, (?~))
import Crypto.JWT                  (Audience(..), ClaimsSet, HasClaimsSet(..), JWTValidationSettings, NumericDate(..), 
                                    claimExp, claimIat, claimSub, defaultJWTValidationSettings, emptyClaimsSet, string)
import Data.Aeson                  (FromJSON, ToJSON)
import Data.Function               ((&))
import Data.String                 (fromString)
import Data.Time                   (UTCTime, addUTCTime)
import Data.UUID                   (UUID)
import qualified Data.UUID as Uuid (toString, fromText)
import GHC.Generics                (Generic)
import Servant                     (err401)

----------------------------------------------------

-- | AccessClaims claimSet
newtype AccessClaims = AccessClaims ClaimsSet
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- | HasClaimsSet instance for AccessClaims.
instance HasClaimsSet AccessClaims where
  claimsSet :: Lens' AccessClaims ClaimsSet
  claimsSet f (AccessClaims claims) = AccessClaims <$> f claims

-- | AccessClaims constructor function.
--
-- Takes the used UUID and current time to build a limited time use JWT token.
accessClaims :: UUID -> UTCTime -> AccessClaims
accessClaims userId issuedAt = emptyClaimsSet
  & claimSub ?~ fromString (Uuid.toString userId)
  & claimIat ?~ NumericDate issuedAt
  & claimExp ?~ NumericDate (addUTCTime 900 issuedAt)
  & claimAud ?~ Audience ["access"]
  & AccessClaims

-- | Access settings for JWT validation of access tokens. 
--
-- Should be used by authHandler.
accessSettings :: JWTValidationSettings
accessSettings = defaultJWTValidationSettings (== "access")

----------------------------------------------------

-- | RefreshClaims claimSet.
newtype RefreshClaims = RefreshClaims ClaimsSet
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- | HasClaimsSet instance for RefreshClaims.
instance HasClaimsSet RefreshClaims where
  claimsSet :: Lens' RefreshClaims ClaimsSet
  claimsSet f (RefreshClaims claims) = RefreshClaims <$> f claims

-- | RefreshClaims constructor function.
--
-- Takes the used UUID and current time to build a limited, but relatively long valid refresh token.
-- Token cannot be used to access data, but only for refreshing the access token.
refreshClaims :: UUID -> UTCTime -> RefreshClaims
refreshClaims userId issuedAt = emptyClaimsSet
  & claimSub ?~ fromString (Uuid.toString userId)
  & claimIat ?~ NumericDate issuedAt
  & claimExp ?~ NumericDate (addUTCTime 86400 issuedAt)
  & claimAud ?~ Audience ["refresh"]
  & RefreshClaims

-- | Refresh settings for JWT validation of access tokens. 
--
-- Should be used by authHandler.
refreshSettings :: JWTValidationSettings
refreshSettings = defaultJWTValidationSettings (== "refresh")

-- | Extracts the subject claim of a token (User's UUID) as a 'Maybe'.
subjectClaim :: HasClaimsSet a => a -> Maybe UUID
subjectClaim c = view claimSub c >>= Uuid.fromText . view string

-- | Extracts the subject claim of a token (User's UUID), but throws a 401 error when it can not.
--
-- Usefull for handlers that require the calling user's UUID for some of its functionality.
extractSub :: (MonadThrow m, MonadIO m) => AccessClaims -> m UUID
extractSub c = case subjectClaim c of 
    Just x  -> return x
    Nothing -> throwM err401