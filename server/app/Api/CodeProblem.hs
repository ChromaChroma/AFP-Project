{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}

module Api.CodeProblem where

-- | Dependency imports
import Control.Monad.Catch      (MonadThrow(..))
import Control.Monad.IO.Class   (liftIO, MonadIO)
import Data.Aeson               (FromJSON, ToJSON)
import Data.Text                (Text)
import Data.Typeable            (Typeable)
import GHC.Generics             (Generic)
import Network.Wai.Handler.Warp 
import Servant                  
import Servant.API.Generic      ((:-))
import Security.App             (App)
import Security.Auth            (AuthJwtAccess)
import Servant.Server.Generic   (AsServerT)
-- | Project imports
import Types
import Security.Claims          (AccessClaims)
import Database                 (allCodingProblems, findCodingProblemById)

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

getCodingProblems :: (MonadThrow m) => m [CodingProblem]
getCodingProblems = pure allCodingProblems

getCodingProblem :: (MonadThrow m) => Text -> m CodingProblem
getCodingProblem = findCodingProblemById 
  
submitAttempt :: (MonadThrow m, MonadIO m) => Maybe AccessClaims -> Text -> AttemptDTO -> m Text
submitAttempt (Just _) pId (AttemptDTO code) = do 
                                                cp <- findCodingProblemById pId
                                                pure "Done"
submitAttempt _ _ _                             = throwM err401
