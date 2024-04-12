{-# LANGUAGE OverloadedStrings  #-}
module Database.Repository where 

-- | Dependency imports
import Control.Monad.Catch          (MonadThrow(..), catch)
import Database.PostgreSQL.Simple
import Data.List                    (find)
import Data.Text                    (Text)
import Data.UUID                    (UUID)
import Servant              
-- | Project imports
import Types.CodingProblem
import qualified Types.User as S (User(..))

-- 
-- Util functions
-- 
unwrapValue :: (MonadThrow m) => [a] -> m a
unwrapValue []    = throwM err404 { errBody = "Could not find user with id" }
unwrapValue (x:_) = pure x

-- 
-- Database Queries
-- 

-- | Gets user by id
getUserById :: Connection -> UUID -> IO S.User
getUserById conn uid = do
  res <- query conn "SELECT * from Users where id = ? LIMIT 1;" (Only uid)   
  unwrapValue res

-- | Gets list of coding problems
getCodingProblems :: Connection -> IO [CodingProblem]
getCodingProblems conn = query_ conn "SELECT * from CodingProblems;" 

-- | Gets coding problem by id
getCodingProblemById :: Connection -> UUID -> IO CodingProblem
getCodingProblemById conn cpId = do
  res <- query conn "SELECT * from CodingProblems where id = ? LIMIT 1;" (Only cpId)   
  unwrapValue res

-- | Gets coding problems test cases by the coding problem's id
getCodingProblemCasesById :: Connection -> UUID -> IO CodingProblemCases
getCodingProblemCasesById conn cpId = do
  res <- query conn "SELECT description, input, output, visibility from CodingProblemCases as cp where cp.casescodingproblemid = ?;" (Only cpId)   
  return $ CodingProblemCases cpId res

-- 
-- Datasensitive Functions
-- 

-- | Gets the user from the database and checks whether provided password is correct.
authenticateUser :: Connection -> Text -> Text -> IO S.User
authenticateUser conn username password = do
  res <- query conn "SELECT * from Users where username = ? LIMIT 1;" (Only username)   
  case res of  
    (u:_) | password == S.password u -> pure u
    _                                -> throwM err401 { errBody = "Could not authenticate" }