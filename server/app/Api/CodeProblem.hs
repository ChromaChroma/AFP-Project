{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Api.CodeProblem where

import Data.Aeson
import Data.Proxy
import Data.List (find)
import Data.Text (Text)
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Data.Typeable (Typeable)
import GHC.Generics

import Network.Wai.Handler.Warp
import Servant
import Servant.Server.Generic ()
import Servant.API.Generic ((:-))

import Types


{- API -}

codingProblemApi :: Proxy CodingProblemAPI
codingProblemApi = Proxy

type CodingProblemAPI = 
  -- | GET /coding-problems
  -- returns list of coding problems
  "coding-problems" 
    :> Get '[JSON] [CodingProblem]

  -- | GET /coding-problems/:id
  -- returns specific coding problem
  :<|> "coding-problems" :> Capture "id" Text 
    :> Get '[JSON] CodingProblem


{- Handlers -}

handlers :: Server CodingProblemAPI
handlers = getCodingProblems :<|> getCodingProblem
  where 
    getCodingProblems      = return dummyCodingProblems
    getCodingProblem ident = return $ case find ((ident ==) . _id) dummyCodingProblems of 
      Just x -> x 
      Nothing -> error "Could not find CodingProblem wiht id"


{- Dummy Values -}

randomDate = UTCTime (fromGregorian 2018 10 27) (secondsToDiffTime 0)

dummyCodingProblems = [
  CodingProblem {
    _id = "123",
    deadline = randomDate,
    problemTags  = [ "Optimization" ],
    difficulty   = Intermediate,
    title        = "Optimize for loop",
    description  = "Optimize the while loop below. Currently it has a time complexity of O(n^2), but can be O(n).",
    testCases = [
      ("Test case 1: Should not crash", "[1,2,3,4,5]", "[2,4,6,8,10]", Visible)
    ],
    templateCode = "module OptimizeLoop where \n import Data.Functor \n"
  }
  ]