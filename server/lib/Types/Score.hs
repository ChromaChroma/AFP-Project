{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}

module Types.Score where 
  
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

type Score = Int

type LeaderboardEntry = (UserId, Username, Score)

data Leaderboard = Leaderboard CodingProblemId [LeaderboardEntry] 
  deriving (Generic, Show, Typeable, FromJSON, ToJSON)