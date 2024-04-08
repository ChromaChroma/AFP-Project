{-# LANGUAGE DeriveAnyClass    #-}

module Security.User (User(..)) where

-- | Dependency imports
import Data.Aeson   (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Data.Text    (Text)

data User = User
  { username :: !Text
  , email    :: !Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)