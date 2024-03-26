{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
module Api.CodeProblem
-- (
--   CodingProblemAPI,
--   handlers
-- ) 
where

import Data.Proxy
import Data.List (find)
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Data.Typeable (Typeable)

import Security.Claims (AccessClaims)

import Network.Wai.Handler.Warp
-- import Servant (err401, err404)
import Servant
import Servant.Server.Generic ()
import Servant.API.Generic ((:-))

import Control.Monad.Catch (MonadThrow(..))
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Data.Text (Text)

import Types
import Security.Auth (AuthJwtAccess)

{- Data Transfer Objects -}

data AttemptDTO = AttemptDTO
  { code :: !Text
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

{- API -}

-- codingProblemApi :: Proxy CodingProblemAPI
-- codingProblemApi = Proxy

-- -- | GET /coding-problems
-- -- Returns list of coding problems
-- type GetCodingProblems = "coding-problems" 
--   :> Get '[JSON] [CodingProblem]

-- -- | GET /coding-problems/:id
-- -- Returns specific coding problem
-- type GetCodingProblem = "coding-problems" :> Capture "id" Text 
--   :> Get '[JSON] CodingProblem

--   -- | POST /coding-problems/:id/attemps (Protected)
--   -- Target for user to submit attempts
-- type SubmitCodingAttempt = AuthJwtAccess 
--   :> "coding-problems" :> Capture "id" Text  :>"attempts"
--   :> ReqBody '[JSON] AttemptDTO
--   :> Post '[JSON] Text

-- type CodingProblemAPI = 
--   --      GetCodingProblems
--   -- :<|> GetCodingProblem
--   -- :<|> 
--   SubmitCodingAttempt

{- Handlers -}

-- handlers :: Server SubmitCodingAttempt
-- handlers = submitAttempt --getCodingProblems :<|> getCodingProblem :<|> 
  
--   where 
--     -- getCodingProblems = return dummyCodingProblems

--     -- getCodingProblem ident (Just _) = return $ case findCodingProblemById ident of 
--     --   Just x -> x 
--     --   Nothing -> error "Could not find CodingProblem with id"
    
--     -- submitAttempt :: MonadThrow m => Maybe AccessClaims -> AttemptDTO -> m Text
--     -- submitAttempt :: SubmitCodingAttempt
--     submitAttempt (Just _) pId (AttemptDTO code) = case findCodingProblemById pId of 
--                                                       Just x  -> pure "Done"
--                                                       Nothing -> throwM err404
--     submitAttempt _ _ _                             = throwM err401















-- getUserHandler :: MonadThrow m => UUID -> m User
-- getUserHandler uid
--   | uid /= nil = pure (User "user" "user@mail.com") --TODO change to actual user data 
--   | otherwise  = throwM err404
  
findCodingProblemById :: Text -> Maybe CodingProblem
findCodingProblemById ident = find ((ident ==) . _id) dummyCodingProblems


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