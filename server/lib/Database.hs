module Database where 

-- | Dependency imports
import Control.Monad.Catch  (MonadThrow(..))
import Data.List            (find)
import Data.Text            (Text)
import Servant              
-- | Project imports
import qualified Dummy as D
import Types

allCodingProblems :: [CodingProblem]
allCodingProblems = D.dummyCodingProblems

findCodingProblemById :: (MonadThrow m) => Text -> m CodingProblem
findCodingProblemById ident = case find ((ident ==) . _id) D.dummyCodingProblems of 
  Just x  -> pure x 
  Nothing -> throwM err404 { errBody = "Could not find coding problem with id" }
 

