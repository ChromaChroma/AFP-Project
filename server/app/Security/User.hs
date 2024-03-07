{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}

module Security.User (User(..)) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Data.Text (Text)

data User = User
  { username :: !Text
  , email    :: !Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)