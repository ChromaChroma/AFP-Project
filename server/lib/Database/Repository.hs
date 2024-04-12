{-# LANGUAGE OverloadedStrings  #-}
module Database.Repository where 

-- | Dependency imports
import Control.Monad                (void)
import Control.Monad.Catch          (MonadThrow(..), catch)
import Database.PostgreSQL.Simple
import Data.List                    (find)
import Data.Text                    (Text)
import Data.UUID                    (UUID)
import Servant              
-- | Project imports
import Types.CodingProblem
import Types.Attempt
import qualified Types.User as S    (User(..), UserId, Username, Password)

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
getUserById :: Connection -> S.UserId -> IO S.User
getUserById conn uid = do
  res <- query conn "SELECT * FROM Users WHERE id = ? LIMIT 1;" (Only uid)   
  unwrapValue res

-- | Gets list of coding problems
getCodingProblems :: Connection -> IO [CodingProblem]
getCodingProblems conn = query_ conn "SELECT * FROM CodingProblems;" 

-- | Gets coding problem by id
getCodingProblemById :: Connection -> UUID -> IO CodingProblem
getCodingProblemById conn cpId = do
  res <- query conn "SELECT * FROM CodingProblems WHERE id = ? LIMIT 1;" (Only cpId)   
  unwrapValue res

-- | Gets coding problems test cases by the coding problem's id
getCodingProblemCasesById :: Connection -> CodingProblemId -> IO CodingProblemCases
getCodingProblemCasesById conn cpId = do
  res <- query conn "SELECT description, input, output, visibility FROM CodingProblemCases AS cp WHERE cp.casescodingproblemid = ?;" (Only cpId)   
  return $ CodingProblemCases cpId res

-- | Saves a 'CodeProblem' 'Attempt' in the Database
saveAttempt :: Connection -> NormalAttempt -> IO ()
saveAttempt conn (Attempt uid  d1 d2 (Code pId c) s) = do 
  void $ execute conn 
    "INSERT INTO attempts (userId, codingProblemId, submitted_on, completed_on, code, state) VALUES (?, ?, ?, ?, ?, ?);" 
    (uid, pId, d1, d2, c, s) 

-- | Gets list of coding problem attempts
getCodingProblemAttempts :: Connection -> CodingProblemId -> S.UserId -> IO [NormalAttempt]
getCodingProblemAttempts conn pid uid = query conn 
  "SELECT userId, submitted_on, completed_on, codingproblemid, code, state FROM Attempts WHERE codingproblemid=? AND userid=?;" 
  (pid, uid)

-- 
-- Datasensitive Functions
-- 

-- | Gets the user from the database and checks whether provided password is correct.
authenticateUser :: Connection -> S.Username -> S.Password -> IO S.User
authenticateUser conn username password = do
  res <- query conn "SELECT * FROM Users WHERE username = ? LIMIT 1;" (Only username)   
  case res of  
    (u:_) | password == S.password u -> pure u
    _                                -> throwM err401 { errBody = "Could not authenticate" }