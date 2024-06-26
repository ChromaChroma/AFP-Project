{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Types.CodingProblem where

-- | Dependency imports
import Data.Aeson                           (FromJSON, ToJSON)
import Data.UUID                            (UUID)
import GHC.Generics                         (Generic)
import qualified Data.ByteString.Char8 as B (unpack)
import Data.Text                            (Text, splitOn, intercalate)
import Data.String                          (fromString)
import Data.Text.Encoding                   (decodeUtf8)
import Data.Time                            (UTCTime)
import Data.Typeable                        (Typeable)
import Database.PostgreSQL.Simple           (FromRow, ToRow)
import Database.PostgreSQL.Simple.FromField 
import Database.PostgreSQL.Simple.ToField   


type CodingProblemId     = UUID
type Tag                 = Text
type TestCaseDescription = Text
type Input               = Text
type Output              = Text

-- 
-- Visibility
-- 

data Visibility = Visible | Hidden 
  deriving (Generic, Show, Typeable, FromJSON, ToJSON)

instance ToField Visibility where 
  toField Visible = Plain "Visible"
  toField Hidden  = Plain "Hidden"
instance FromField Visibility where 
  fromField  _ (Just "Visible") = return Visible
  fromField  _ (Just "Hidden")  = return Hidden
  fromField  f (Just dat)       = returnError ConversionFailed f (B.unpack dat)
  fromField  f _                = returnError UnexpectedNull f ""

-- | A test case for a coding assignment with a description, input values and expected output values
type TestCase = (TestCaseDescription, Input, Output, Visibility) 

-- 
-- ProblemDifficulty
-- 

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


data CodingProblemCases = CodingProblemCases 
  { casesCodingProblemId :: CodingProblemId
  , testCases            :: [TestCase]
  } deriving (Generic, Show, Typeable, FromJSON, ToJSON)

-- 
-- CodingProblem
-- 

data CodingProblem = CodingProblem
  { _id          :: CodingProblemId
  , deadline     :: UTCTime
  , problemTags  :: [Tag]
  , difficulty   :: ProblemDifficulty
  , title        :: Text
  , description  :: Text
  , templateCode :: Text
  -- , leaderboard  :: Leaderboard
  } deriving (Generic, Show, Typeable, FromJSON, ToJSON, FromRow, ToRow)

instance ToField [Tag] where 
  toField = toField . intercalate ","
instance FromField [Tag] where 
  fromField  _ (Just dat) = return . splitOn "," $ decodeUtf8 dat
  fromField  _ _          = return []
  