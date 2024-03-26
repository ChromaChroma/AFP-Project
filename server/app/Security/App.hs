{-# LANGUAGE GeneralizedNewtypeDeriving  #-}

module Security.App (App(..), appToHandler) where

-- | Dependency imports
import Control.Monad.Catch    (MonadThrow, try)
import Control.Monad.Except   (ExceptT(..))
import Control.Monad.Identity (IdentityT (..))
import Control.Monad.IO.Class (MonadIO)
import Servant                (Handler(..))

newtype App a = App (IdentityT IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow)

appToHandler :: App a -> Handler a
appToHandler (App app) = (Handler . ExceptT . try . runIdentityT) app