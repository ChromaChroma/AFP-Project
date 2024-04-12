{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}

module Api.CodeProblem where

-- | Dependency imports
import Control.Monad.Catch        (MonadThrow(..))
import Control.Monad.IO.Class     (liftIO, MonadIO)
import Crypto.JWT                 
import Data.String                (IsString(..))
import Data.Aeson                 (ToJSON, FromJSON)
import Data.Text                  (Text)
import Data.Time                  (getCurrentTime)
import Data.Typeable              (Typeable)
import Data.UUID                  (UUID)
import qualified Data.UUID as UUID (fromText)
import Database.PostgreSQL.Simple (Connection)
import GHC.Generics               (Generic)
import Network.Wai.Handler.Warp 
import Servant                  
import Servant.API.Generic        ((:-))
import Security.App               (App)
import Security.Auth              (AuthJwtAccess)
import Servant.Server.Generic     (AsServerT)
-- | Project imports
import Types.CodingProblem
import Types.Attempt
import Security.Claims            (AccessClaims, extractSub)
import System.Processing          (processAttempt)
import qualified Database.Repository as DB (getCodingProblems, getCodingProblemById, getCodingProblemCasesById, saveAttempt)

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

-- | Handlers for 'CodingProblemAPI'
handlers :: Connection -> CodingProblemAPI (AsServerT App)
handlers conn = getCodingProblems conn :<|> getCodingProblem conn :<|> submitAttempt conn 

-- | Handlers for 'GetCodingProblems'
getCodingProblems :: (MonadThrow m, MonadIO m) => Connection -> m [CodingProblem]
getCodingProblems = liftIO . DB.getCodingProblems

-- | Handlers for 'GetCodingProblem'
getCodingProblem :: (MonadThrow m, MonadIO m) => Connection -> UUID -> m CodingProblem
getCodingProblem conn = liftIO . DB.getCodingProblemById conn
  
-- | Handlers for 'SubmitCodingAttempt'
submitAttempt :: (MonadThrow m, MonadIO m) => Connection ->  Maybe AccessClaims -> UUID -> AttemptDTO -> m Text
submitAttempt conn (Just c) pId (AttemptDTO code) = do 
                                                    submitDate <- liftIO getCurrentTime
                                                    uid        <- extractSub c
                                                    cp         <- liftIO $ DB.getCodingProblemById conn pId
                                                    cases      <- liftIO $ DB.getCodingProblemCasesById conn pId
                                                    res        <- fromString <$> liftIO (processAttempt uid cp cases code)
                                                    now        <- liftIO getCurrentTime
                                                    liftIO $ DB.saveAttempt conn (Attempt submitDate now (Code pId code) (Succeeded res))
                                                    return res
submitAttempt _ _ _ _                             = throwM err401