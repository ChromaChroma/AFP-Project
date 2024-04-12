{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}

module Types.Attempt where 
  
import Data.Aeson                 (FromJSON, ToJSON)
import Data.Text                  (Text)
import Data.Time                  (UTCTime)
import Data.UUID                  (UUID)
-- import Database.PostgreSQL.Simple (FromRow, ToRow)
-- import Database.PostgreSQL.Simple.FromField
-- import Database.PostgreSQL.Simple.ToField
import GHC.Generics               (Generic)
import Data.Typeable              (Typeable)
import Servant.Auth.JWT           (FromJWT, ToJWT)
-- | Project Dependency
import Types.CodingProblem

-- | Type synonym for a normal, simple attempt.
-- Gives string messages of succes of failure
type NormalAttempt = Attempt String String

data Code = Code
  { relatedProblem :: CodingProblemId,
    codeContents   :: Text
  }
  deriving (Generic, Show, Typeable, FromJSON, ToJSON)

-- r = succeeding result type
-- e = error type
data AttemptState r e
  = Queued
  | Compiling
  | Testing
  | Succeeded r
  | Failed e
  deriving (Generic, Show, Typeable, FromJSON, ToJSON)

data Attempt r e = Attempt
  { submittedOn     :: UTCTime,
    runCompletedOn  :: UTCTime,
    code            :: Code,
    state           :: AttemptState r e
  }
  deriving (Generic, Show, Typeable, FromJSON, ToJSON)

