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

import Control.Monad.Catch (MonadThrow(..))
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

import Network.Wai.Handler.Warp
import Servant
import Servant.API.Generic ((:-))
import Security.App (App)
import Security.Auth (AuthJwtAccess)
import Servant.Server.Generic (AsServerT)

import Types
import Security.Claims (AccessClaims)
import Database (allCodingProblems, findCodingProblemById)

{- Data Transfer Objects -}

newtype AttemptDTO = AttemptDTO { code :: Text } 
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

{- API -}

-- | Coding problem API 
type CodingProblemAPI mode = 
       GetCodingProblems mode
  :<|> GetCodingProblem mode
  :<|> SubmitCodingAttempt mode


-- | GET /coding-problems
-- Returns list of coding problems
type GetCodingProblems mode = mode :- "coding-problems" 
  :> Get '[JSON] [CodingProblem]

-- | GET /coding-problems/:id
-- Returns specific coding problem
type GetCodingProblem  mode = mode :- "coding-problems" :> Capture "id" Text 
  :> Get '[JSON] CodingProblem

  -- | POST /coding-problems/:id/attemps (Protected)
  -- Target for user to submit attempts
type SubmitCodingAttempt mode = mode :- AuthJwtAccess 
  :> "coding-problems" :> Capture "id" Text  :> "attempts"
  :> ReqBody '[JSON] AttemptDTO
  :> Post '[JSON] Text


{- Handlers -}

handlers :: CodingProblemAPI (AsServerT App)
handlers = getCodingProblems :<|> getCodingProblem :<|> submitAttempt 
  where 
    getCodingProblems = pure allCodingProblems

    getCodingProblem ident = pure $ case findCodingProblemById ident of 
      Just x -> x 
      Nothing -> error "Could not find CodingProblem with id"
  
    submitAttempt (Just _) pId (AttemptDTO code) = case findCodingProblemById pId of 
                                                      Just x  -> pure "Done"
                                                      Nothing -> throwM err404
    submitAttempt _ _ _                             = throwM err401
