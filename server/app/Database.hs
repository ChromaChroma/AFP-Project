module Database where 

import Data.List (find)
import Data.Text (Text)

import qualified Dummy as D
import Types

allCodingProblems :: [CodingProblem]
allCodingProblems = D.dummyCodingProblems

findCodingProblemById :: Text -> Maybe CodingProblem
findCodingProblemById ident = find ((ident ==) . _id) D.dummyCodingProblems

