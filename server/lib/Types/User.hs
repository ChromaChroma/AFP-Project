{-# LANGUAGE DeriveAnyClass    #-}

module Types.User where

-- | Dependency imports
import Data.Aeson                 (FromJSON, ToJSON)
import Data.UUID                  (UUID)
import GHC.Generics               (Generic)
import Data.Text                  (Text)
import Data.Typeable              (Typeable)
import Database.PostgreSQL.Simple (FromRow, ToRow)

type UserId   = UUID 
type Username = Text
type Email    = Text
type Password = Text -- ^ A hashed version of a password


data Role
  = UserRole  -- ^ Standard user
  | AdminRole -- ^ Administrative user
  deriving (Eq, Show, Generic, Typeable, FromJSON, ToJSON)
  
-- | User's account
data User = User
  { _id      :: !UserId
  , username :: !Username
  , email    :: !Email
  , password :: !Password
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, FromRow, ToRow)
