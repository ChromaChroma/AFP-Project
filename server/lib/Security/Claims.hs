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
  ) where

-- | Dependency imports
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

----------------------------------------------------

newtype AccessClaims = AccessClaims ClaimsSet
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

----------------------------------------------------

instance HasClaimsSet AccessClaims where
  claimsSet :: Lens' AccessClaims ClaimsSet
  claimsSet f (AccessClaims claims) = AccessClaims <$> f claims

accessClaims :: UUID -> UTCTime -> AccessClaims
accessClaims userId issuedAt = emptyClaimsSet
  & claimSub ?~ fromString (Uuid.toString userId)
  & claimIat ?~ NumericDate issuedAt
  & claimExp ?~ NumericDate (addUTCTime 900 issuedAt)
  & claimAud ?~ Audience ["access"]
  & AccessClaims

accessSettings :: JWTValidationSettings
accessSettings = defaultJWTValidationSettings (== "access")

----------------------------------------------------

newtype RefreshClaims = RefreshClaims ClaimsSet
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

instance HasClaimsSet RefreshClaims where
  claimsSet :: Lens' RefreshClaims ClaimsSet
  claimsSet f (RefreshClaims claims) = RefreshClaims <$> f claims

refreshClaims :: UUID -> UTCTime -> RefreshClaims
refreshClaims userId issuedAt = emptyClaimsSet
  & claimSub ?~ fromString (Uuid.toString userId)
  & claimIat ?~ NumericDate issuedAt
  & claimExp ?~ NumericDate (addUTCTime 86400 issuedAt)
  & claimAud ?~ Audience ["refresh"]
  & RefreshClaims

refreshSettings :: JWTValidationSettings
refreshSettings = defaultJWTValidationSettings (== "refresh")

subjectClaim :: HasClaimsSet a => a -> Maybe UUID
subjectClaim c = view claimSub c >>= Uuid.fromText . view string