{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE ViewPatterns       #-}
{-# LANGUAGE TypeApplications   #-}

module Security.Auth (AuthJwtAccess, AuthJwtRefresh, authHandler, signToken, generateKey) where

-- | Dependency imports
import Control.Monad                              (guard)
import Control.Monad.IO.Class                     (liftIO)
import Crypto.JOSE                                (JWK, KeyMaterialGenParam(OctGenParam), 
                                                   bestJWSAlg, decodeCompact, genJWK, newJWSHeader, runJOSE)
import Crypto.JWT                                 (HasClaimsSet, JWTError, JWTValidationSettings, SignedJWT, signJWT, verifyJWT)
import Data.Aeson                                 (FromJSON, ToJSON)
import Data.ByteString.UTF8                       (ByteString)
import qualified Data.ByteString.UTF8 as BS       (break, drop, toString)
import qualified Data.ByteString.Lazy.UTF8 as LBS (fromString)
import Network.Wai                                (Request, requestHeaders)
import Servant                                    (AuthProtect)
import Servant.Server.Experimental.Auth           (AuthHandler, mkAuthHandler, AuthServerData)
import Security.Claims                            (AccessClaims, RefreshClaims)


----------------------------------------------------

type AuthJwtAccess  = AuthProtect "jwt-access'"
type AuthJwtRefresh = AuthProtect "jwt-refresh'"

type instance AuthServerData AuthJwtAccess  = Maybe AccessClaims
type instance AuthServerData AuthJwtRefresh = Maybe RefreshClaims

----------------------------------------------------

-- 
-- Token Validation
-- 

-- | Authentication handler of the server.
--
-- Takes the token of a request and verifies it.
-- Used on authentication of token secured endpoints.
--
-- If the /token/ is /valid/, 
--  then returns the 'AuthServerData' of 'Just' 'AuthJwtAccess'/'AuthJwtRefresh' 
--  else Nothing. 
-- The AuthJwts contain claims of the token.
authHandler :: (HasClaimsSet a, FromJSON a) 
            => JWK -> JWTValidationSettings -> AuthHandler Request (Maybe a)
authHandler jwk settings = mkAuthHandler $ \case
  (getToken -> Just token) -> liftIO (verifyToken jwk settings token)
  _                        -> pure Nothing

-- | Extracts the JWT token from the request headers.
--
-- Expects header "Authorization" wiht the value "Bearer {token}" where '{token}' is the jwt token provided by the user.
getToken :: Request -> Maybe ByteString
getToken req = do
  (scheme, token)  <- BS.break (== ' ') <$> lookup "Authorization" (requestHeaders req)
  guard (scheme == "Bearer")
  return $ BS.drop 1 token

-- | Verifies the JWT token.
verifyToken :: (HasClaimsSet a, FromJSON a)
            => JWK -> JWTValidationSettings -> ByteString -> IO (Maybe a)
verifyToken jwk settings token = maybeRight <$> runJOSE @JWTError verify
  where
    lazy = LBS.fromString (BS.toString token)
    verify = decodeCompact lazy >>= verifyJWT settings jwk


-- 
-- Token creation
-- 

-- | Creates a JWT token with then provided claims and signs it.
--
-- Returns a 'Maybe' 'SignedJWT' if succesful. Otherwise 'Nothing'.
signToken :: (ToJSON a) => JWK -> a -> IO (Maybe SignedJWT)
signToken jwk claims = maybeRight <$> runJOSE @JWTError sign
  where
    sign = do
      alg <- bestJWSAlg jwk
      signJWT jwk (newJWSHeader ((), alg)) claims

-- | Converts an 'Either' Into a 'Maybe'
maybeRight :: Either a b -> Maybe b
maybeRight = either (const Nothing) Just

-- | Generates a JWK key based on a symetric key of 256 bytes.
generateKey :: IO JWK
generateKey = genJWK (OctGenParam 256)
