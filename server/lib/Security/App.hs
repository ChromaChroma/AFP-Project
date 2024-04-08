{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE TypeApplications            #-}

module Security.App (App(..), appToHandler, jwtDefaultContext) where

-- | Dependency imports
import Control.Monad.Catch              (MonadThrow, try)
import Control.Monad.Except             (ExceptT(..))
import Control.Monad.Identity           (IdentityT (..))
import Control.Monad.IO.Class           (MonadIO)
import Crypto.JWT                       (JWK)
import Servant                          (Handler(..))
import Servant.Server                   (Context(..))
import Servant.Server.Experimental.Auth (AuthHandler)
import Network.Wai.Internal             (Request)
-- | Project imports
import Security.Claims                  (AccessClaims, RefreshClaims, accessSettings, refreshSettings)
import Security.Auth                    (authHandler)

newtype App a = App (IdentityT IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow)

appToHandler :: App a -> Handler a
appToHandler (App app) = (Handler . ExceptT . try . runIdentityT) app

jwtDefaultContext :: JWK -> Context '[AuthHandler Request (Maybe AccessClaims), AuthHandler Request (Maybe RefreshClaims)]
jwtDefaultContext jwk = authHandler @AccessClaims jwk accessSettings
                  :. authHandler @RefreshClaims jwk refreshSettings
                  :. EmptyContext