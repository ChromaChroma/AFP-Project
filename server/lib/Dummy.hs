module Dummy where

-- | Dependency imports
import Data.Time  (UTCTime (..), fromGregorian, secondsToDiffTime)
import Data.Text  (Text)
import Data.Maybe (fromJust)
import Data.UUID  (fromString)
-- | Project imports
import Types

{- Dummy Values -}

randomDate = UTCTime (fromGregorian 2018 10 27) (secondsToDiffTime 0)

dummyUUID = fromJust $ fromString "08a2c3d9-b7ec-48e5-8f40-3a942ad01130"

dummyCodingProblems = [
  CodingProblem {
    _id = dummyUUID,
    deadline = randomDate,
    problemTags  = [ "Optimization" ],
    difficulty   = Intermediate,
    title        = "Optimize for loop",
    description  = "Optimize the while loop below. Currently it has a time complexity of O(n^2), but can be O(n).",
    -- testCases = [
    --   ("Test case 1: Should not crash", "[1,2,3,4,5]", "[2,4,6,8,10]", Visible)
    -- ],
    templateCode = "module OptimizeLoop where \n import Data.Functor \n"
  }
  ]