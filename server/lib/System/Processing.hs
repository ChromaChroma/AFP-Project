{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}


module System.Processing where 

-- | Dependency imports
import Control.Monad          (void)
import Control.Monad.Catch    (MonadThrow(..))
import qualified Data.ByteString.Lazy.UTF8 as UTF8 (fromString)
import Data.Functor           ((<&>))
import Data.UUID              (UUID)
import Data.Text              (Text, unpack)
import Servant
import System.Exit            (ExitCode(..))
import System.IO              (hPutStr, hGetContents, hGetLine)
import System.IO.Unsafe       (unsafePerformIO)
import System.Process         (StdStream(..), CreateProcess(..), waitForProcess, proc, createProcess)
import System.Directory       (createDirectoryIfMissing, removeDirectoryRecursive)
import Control.Exception      (ErrorCall, handle)
import System.IO.Error        (catchIOError)
-- | Project imports
import Types.CodingProblem

import Debug.Trace (trace)

-- | Runs an IO computation and ignores any 'IOError' or 'ErrorCall' errors in they come up.
ignoreIOFail :: IO () -> IO ()
ignoreIOFail = handle handlerError . flip catchIOError handlIOError
  where
    -- | Handles IOExceptions thrown
    handlIOError :: IOError -> IO ()
    handlIOError e = void (print e)
    -- | Handles 'error' calls thrown
    handlerError :: ErrorCall -> IO ()
    handlerError e = void (print e)

-- | Runs a 'CreateProcess', force evaluates it and returns its result as an 'Either String ExitCode'.
-- 
-- The 'Either' contains the terminal error message as a 'Left' if process exited erroneous.
--
runProcess :: CreateProcess -> IO (Either String ExitCode)
runProcess proccess = do 
  (_, _, Just errHandle, exitHandle) <- createProcess proccess { std_err = CreatePipe }
  exitCode <- waitForProcess exitHandle
  case exitCode of 
    ExitSuccess   -> pure $ Right ExitSuccess
    ExitFailure _ -> Left <$> hGetContents errHandle

-- | Takes the result of a process attempt and creates a 409 response with the error message contained in the 'Left' value
throwIfError :: MonadThrow m => m (Either String a) -> m a
throwIfError io = io >>= \case
  Left err -> throwM err409 {errBody = UTF8.fromString err }
  Right x  -> return x

-- | Processes the given code.
--
-- Creates a clean empty directory, attempts to compile, run and test the code, and returns its result.
--
processAttempt :: UUID -> CodingProblem -> CodingProblemCases -> Text -> IO String
processAttempt uid cp (CodingProblemCases _ cases) code = do
  let tempDir     = "temp/" ++ show uid
      tempDirMain = tempDir ++ "/Main.hs" 
      tempExe     = tempDir ++ "/main"

  -- | Clean temp dir
  ignoreIOFail $ removeDirectoryRecursive tempDir
  -- | Creates new temp dir
  createDirectoryIfMissing True tempDir
  -- | Create Main file with code
  writeFile tempDirMain (unpack code)
  -- | Compile Main file (thus checks if it is valid code)
  --   Returns errors in the code back to the user when wrong
  throwIfError $ runProcess (proc "ghc" [tempDirMain])
  -- | Run testcases against program and build the response
  response <- throwIfError (testCasesResultMessage <$> runTestcases tempExe cases )
  -- | Clean temp dir
  ignoreIOFail $ removeDirectoryRecursive tempDir
  return response

-- | Runs all provided testcases against the program
--
-- Folds over the test cases and runs them using @runTest@.
-- Returns an 'Either' in an 'IO', containing either a 'Left' with the error message, or a 'Right' with the amount of succeeded tests (all).
-- Returns 'Left' at first failed testcase
runTestcases :: String -> [TestCase] -> IO (Either String Int)
runTestcases exePath = foldr f (pure $ Right 0)
  where 
    f :: TestCase -> IO (Either String Int) -> IO (Either String Int)
    f testcase acc = do 
      either <- runTest exePath testcase 
      case either of 
        Right x -> ((1 +) <$>) <$> acc
        Left  x -> pure $ Left x
         
-- | Creates a process that runs a testcase against the program
--
-- Returns an 'Either' in an 'IO', containing either a 'Left' with the error message, or a 'Right' with the output
runTest :: String -> TestCase -> IO (Either String String)
runTest exePath (unpack -> desc, inp, unpack -> expOut, _) = do 
  (_, Just oH, Just errH, eH) <- createProcess (proc exePath [unpack inp]) 
                                                      { std_out = CreatePipe 
                                                      , std_err = CreatePipe }
  exitCode <- waitForProcess eH
  o        <- hGetLine oH
  err      <- hGetContents errH
  return $ case exitCode of 
    ExitSuccess | o == expOut -> Right o 
    ExitSuccess               -> Left (testCaseFailedMessage desc expOut o)
    ExitFailure _             -> Left (testCaseErrorMessage desc expOut err)

-- 
-- Response messages
-- 

-- | Creates a message for a failed test
testCaseFailedMessage :: String -> String -> String -> String
testCaseFailedMessage desc exp expOut = "Testcase (\""++ desc ++"\"): Expected " ++ exp ++ ", but got: " ++ expOut

-- | Creates a message for a crashed test 
testCaseErrorMessage :: String -> String -> String -> String
testCaseErrorMessage desc exp err = "Testcase (\""++ desc ++"\"): Expected " ++ exp ++ ", but got an error: " ++ err

-- | Creates a message for completed test run results
testCasesResultMessage :: Either String Int -> Either String String
testCasesResultMessage testResults = (\x -> "All " ++ show x ++ " tests have been run successfully.") <$> testResults