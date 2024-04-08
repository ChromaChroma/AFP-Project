{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}

module Api.CodeProblem where

-- | Dependency imports
import Control.Monad.Catch        (MonadThrow(..))
import Control.Monad.IO.Class     (liftIO, MonadIO)
import Data.Aeson                 (FromJSON, ToJSON)
import Data.Text                  (Text)
import Data.Typeable              (Typeable)
import Data.UUID                  (UUID)
import Database.PostgreSQL.Simple (Connection)
import GHC.Generics               (Generic)
import Network.Wai.Handler.Warp 
import Servant                  
import Servant.API.Generic        ((:-))
import Security.App               (App)
import Security.Auth              (AuthJwtAccess)
import Servant.Server.Generic     (AsServerT)
-- | Project imports
import Types
import Security.Claims            (AccessClaims)
import System.Processing          (processAttempt)
import qualified Database as DB   (getCodingProblems, getCodingProblemById)

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
type GetCodingProblem  mode = mode :- "coding-problems" :> Capture "id" UUID 
  :> Get '[JSON] CodingProblem

  -- | POST /coding-problems/:id/attemps (Protected)
  -- Target for user to submit attempts
type SubmitCodingAttempt mode = mode :- AuthJwtAccess 
  :> "coding-problems" :> Capture "id" UUID  :> "attempts"
  :> ReqBody '[JSON] AttemptDTO
  :> Post '[JSON] Text


{- Handlers -}

handlers :: Connection -> CodingProblemAPI (AsServerT App)
handlers conn = getCodingProblems conn :<|> getCodingProblem conn :<|> submitAttempt conn 

getCodingProblems :: (MonadThrow m, MonadIO m) => Connection -> m [CodingProblem]
getCodingProblems = liftIO . DB.getCodingProblems

getCodingProblem :: (MonadThrow m, MonadIO m) => Connection -> UUID -> m CodingProblem
getCodingProblem conn = liftIO . DB.getCodingProblemById conn
  
submitAttempt :: (MonadThrow m, MonadIO m) => Connection ->  Maybe AccessClaims -> UUID -> AttemptDTO -> m Text
submitAttempt conn (Just _) pId (AttemptDTO code) = do 
                                                cp  <- liftIO $ DB.getCodingProblemById conn pId
                                                res <- liftIO (print code)
                                                liftIO $ processAttempt code 
                                                pure  "Done!" 
submitAttempt _ _ _ _                           = throwM err401

