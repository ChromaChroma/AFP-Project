{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE FlexibleInstances    #-}

module Types.Attempt where 
  
import Data.Aeson                       (FromJSON, ToJSON)
import qualified Data.ByteString as BS  (splitAt)
import Data.Text                        (Text, unpack)
import Data.Text.Encoding                   (decodeUtf8)
import Data.Time                        (UTCTime)
import Data.UUID                        (UUID)
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import GHC.Generics                     (Generic)
import Data.Typeable                    (Typeable)
import Servant.Auth.JWT                 (FromJWT, ToJWT)
-- | Project Dependency
import Types.CodingProblem


data Code = Code
  { relatedProblem :: CodingProblemId
  , codeContents   :: Text
  }
  deriving (Generic, Show, Typeable, FromJSON, ToJSON)

data AttemptState r e
  = Succeeded r 
  | Failed e
  deriving (Generic, Show, Typeable, FromJSON, ToJSON)

instance FromField (AttemptState Text Text) where
  fromField f (Just bs) = fromField' (BS.splitAt 2 bs)
    where
      fromField' (x, ys) | x == "S:" = return $ Succeeded $ decodeUtf8 ys
                         | x == "F:" = return $ Failed    $ decodeUtf8 ys
                         | otherwise = returnError ConversionFailed f "Invalid (AttemptState Text Text) value"
  fromField f _ = returnError UnexpectedNull f "AttemptState Text Text"

instance ToField (AttemptState Text Text) where
  toField (Succeeded x) = toField ("S:" ++ unpack x :: String)
  toField (Failed    e) = toField ("F:"++ unpack e :: String)
  
{-
-- More complex version for multithreaded for event-like processing of compilitation and tests. 
-- This would allow to create processes that the user can check up on if they are done, instead of waiting for full process to be done in one request.

-- r = succeeding result type
-- e = error type
data AttemptState r e
  = Queued
  | Compiling
  | Testing
  | Succeeded r
  | Failed e
  deriving (Generic, Show, Typeable, FromJSON, ToJSON)
-}

data Attempt r e = Attempt
  { userId          :: UUID
  , submittedOn     :: UTCTime
  , runCompletedOn  :: UTCTime
  , code            :: Code
  , state           :: AttemptState r e
  }
  deriving (Generic, Show, Typeable, FromJSON, ToJSON)

-- | Type synonym for a normal, simple attempt.
--
-- Gives Text message if failed.
type NormalAttempt = Attempt Text Text