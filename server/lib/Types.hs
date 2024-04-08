{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

-- | Dependency imports
import Data.Aeson                 (FromJSON, ToJSON)
import Data.Text                  (Text)
import Data.Time                  (UTCTime)
import Data.UUID                  (UUID)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import GHC.Generics               (Generic)
import Data.Typeable              (Typeable)
import Servant.Auth.JWT           (FromJWT, ToJWT)
-- import Data.ByteString.Builder.Internal.Builder (Builder(..))

{- Security -}
type UserId = Text 
type Username = Text

-- | A hashed version of a password
type Password = Text

data User = User
  { userId   :: UserId,
    username :: Username,
    password :: Password,
    role     :: Role
  }
  deriving (Eq, Show, Generic, Typeable, FromJSON, ToJSON, FromJWT, ToJWT)


data Role
  = UserRole  -- ^ Standard user
  | AdminRole -- ^ Administrative user
  deriving (Eq, Show, Generic, Typeable, FromJSON, ToJSON)

{- Domain -}

type TestCaseDescription = Text
type Input = Text
type Output = Text

data Visibility = Visible | Hidden 
  deriving (Generic, Show, Typeable, FromJSON, ToJSON, FromField, ToField)

-- | A test case for a coding assignment with a description, input values and expected output values
type TestCase = (TestCaseDescription, Input, Output, Visibility) 


type CodingProblemId = UUID

data ProblemDifficulty = Easy | Intermediate | Difficult | Extreme
 deriving (Generic, Show, Typeable, FromJSON, ToJSON)

instance FromField ProblemDifficulty where
  fromField f (Just "Easy")         = return Easy
  fromField f (Just "Intermediate") = return Intermediate
  fromField f (Just "Difficult")    = return Difficult
  fromField f (Just "Extreme")      = return Extreme
  fromField f (Just _)              = returnError ConversionFailed f "Invalid ProblemDifficulty value"
  fromField f Nothing               = returnError UnexpectedNull f "ProblemDifficulty"

instance ToField ProblemDifficulty where
  toField Easy         = toField ("Easy" :: String)
  toField Intermediate = toField ("Intermediate" :: String)
  toField Difficult    = toField ("Difficult" :: String)
  toField Extreme      = toField ("Extreme" :: String)

type Tag = Text

data CodingProblem = CodingProblem
  { _id          :: CodingProblemId,
    deadline     :: UTCTime,
    problemTags  :: [Tag],
    difficulty   :: ProblemDifficulty,
    title        :: Text,
    description  :: Text,
    testCases    :: [TestCase],
    templateCode :: Text
    -- leaderboard  :: Leaderboard
  } deriving (Generic, Show, Typeable, FromJSON, ToJSON, FromRow, ToRow)

-- TODO, implement actual version
instance ToField [TestCase] where 
  toField  _ = Plain ""
instance FromField [TestCase] where 
  fromField  _ _ = return []

  
-- TODO, implement actual version
instance ToField [Tag] where 
  toField  _ = Plain ""
instance FromField [Tag] where 
  fromField  _ _ = return []

{- Code Attempt -}

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

-- | Type synonym for a normal, simple attempt.
-- Gives string messages of succes of failure
type NormalAttempt = Attempt String String

{- Score system -}
type Score = Int

type LeaderboardEntry = (UserId, Username, Score)

data Leaderboard = Leaderboard CodingProblemId [LeaderboardEntry] 
  deriving (Generic, Show, Typeable, FromJSON, ToJSON)