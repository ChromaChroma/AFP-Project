module Types where

{- Security -}
type UserId = String

type Username = String

-- | A hashed version of a password
type Password = String

data User = User
  { userId   :: UserId,
    username :: Username,
    password :: Password,
    role     :: Role
  }

data Role
  = UserRole  -- ^ Standard user
  | AdminRole -- ^ Administrative user

-- -- | Permissions a user can have for a resource
-- data Permissions
--   = NoPerm        -- ^ No permission at all
--   | ReadPerm      -- ^ Allowed to read
--   | WritePerm     -- ^ Allowed to write
--   | ReadWriteperm -- ^ Allowed to read and write
--   | AdminPerm     -- ^ Allowed to read, write and edit

{- Domain -}

type TestCaseDescription = String

type Input = String

type Output = String

data Visibility = Visible | Hidden

-- | A test case for a coding assignment with a description, input values and expected output values
type TestCase = (TestCaseDescription, Input, Output, Visibility)

type CodingProblemId = String

data ProblemDifficulty = Easy | Intermediate | Difficult | Extreme

type Tag = String

type DateTime = String -- TODO change to date datatype?

data CodingProblem = CodingProblem
  { id           :: CodingProblemId,
    deadline     :: DateTime,
    problemTags  :: [Tag],
    difficulty   :: ProblemDifficulty,
    title        :: String,
    description  :: String,
    testCases :: [TestCase],
    templateCode :: String
    -- leaderboard  :: Leaderboard
  }

{- Code Attempt -}

data Code = Code
  { relatedProblem :: CodingProblemId,
    codeContents   :: String
  }

-- r = succeeding result type
-- e = error type
data AttemptState r e
  = Queued
  | Compiling
  | Testing
  | Succeeded r
  | Failed e

data Attempt r e = Attempt
  { submittedOn     :: DateTime,
    runCompletedOn  :: DateTime,
    code            :: Code,
    state           :: AttemptState r e
  }

{- Score system -}
type Score = Int

type LeaderboardEntry = (UserId, Username, Score)

data Leaderboard = Leaderboard CodingProblemId [LeaderboardEntry]