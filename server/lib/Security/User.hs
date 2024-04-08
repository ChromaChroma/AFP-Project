{-# LANGUAGE DeriveAnyClass    #-}

module Security.User (User(..)) where

-- | Dependency imports
import Data.Aeson   (FromJSON, ToJSON)
import Data.UUID    (UUID)
import GHC.Generics (Generic)
import Data.Text    (Text)
import Database.PostgreSQL.Simple (FromRow, ToRow)

data User = User
  { _id      :: !UUID
  , username :: !Text
  , email    :: !Text
  , password :: !Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, FromRow, ToRow)