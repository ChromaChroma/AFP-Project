{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE LambdaCase  #-}
module Database where 

-- | Dependency imports
import Control.Monad.Catch          (MonadThrow(..), catch)
import Database.PostgreSQL.Simple
import Data.List                    (find)
import Data.Text                    (Text)
import Data.UUID                    (UUID)
import Servant              
-- | Project imports
import qualified Dummy as D
import Types
import qualified Security.User as S (User(..))

allCodingProblems :: [CodingProblem]
allCodingProblems = D.dummyCodingProblems

findCodingProblemById :: (MonadThrow m) => UUID -> m CodingProblem
findCodingProblemById ident = case find ((ident ==) . _id) D.dummyCodingProblems of 
  Just x  -> pure x 
  Nothing -> throwM err404 { errBody = "Could not find coding problem with id" }

-- 
-- Util functions
-- 
unwrapValue :: (MonadThrow m) => [a] -> m a
unwrapValue []    = throwM err404 { errBody = "Could not find user with id" }
unwrapValue (x:_) = pure x

-- 
-- Database Queries
-- 
getUserById :: Connection -> UUID -> IO S.User
getUserById conn uid = do
  res <- query conn "SELECT * from Users where id = ? LIMIT 1;" (Only uid)   
  unwrapValue res

getCodingProblems :: Connection -> IO [CodingProblem]
getCodingProblems conn = query_ conn "SELECT * from CodingProblems;" 

getCodingProblemById :: Connection -> UUID -> IO CodingProblem
getCodingProblemById conn cpId = do
  res <- query conn "SELECT * from CodingProblems where id = ? LIMIT 1;" (Only cpId)   
  unwrapValue res

-- 
-- Datasensitive Functions
-- 
authenticateUser :: Connection -> Text -> Text -> IO S.User
authenticateUser conn username password = do
  res <- query conn "SELECT * from Users where username = ? LIMIT 1;" (Only username)   
  case res of  
    (u:_) | password == S.password u -> pure u
    _                                -> throwM err401 { errBody = "Could not authenticate" }