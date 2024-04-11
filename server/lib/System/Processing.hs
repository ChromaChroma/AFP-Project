{-# LANGUAGE LambdaCase #-}

module System.Processing where 

-- | Dependency imports
import Control.Monad.Catch    (MonadThrow(..))
import qualified Data.ByteString.Lazy.UTF8 as UTF8 (fromString)
import Data.UUID              (UUID)
import Data.Text              (Text, unpack)
import Servant
import System.Exit            (ExitCode(..))
import System.IO              (hGetContents)
import System.IO.Unsafe       (unsafePerformIO)
import System.Process         (StdStream(..), CreateProcess(..), waitForProcess, proc, createProcess)
import System.Directory       (createDirectoryIfMissing, removeDirectoryRecursive)
import Control.Exception      (ErrorCall, handle)
import System.IO.Error        (catchIOError)
-- | Project imports
import Types

-- | Runs an IO computation and ignores any 'IOError' or 'ErrorCall' errors in they come up.
ignoreIOFail :: IO () -> IO ()
ignoreIOFail = handle handlerError . flip catchIOError handlIOError
  where
    -- | Handles IOExceptions thrown
    handlIOError :: IOError -> IO ()
    handlIOError _ = pure ()
    -- | Handles 'error' calls thrown
    handlerError :: ErrorCall -> IO ()
    handlerError _ = pure ()

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
throwIfError :: MonadThrow m => m (Either String a) -> m ()
throwIfError io = io >>= \case
  Left err -> throwM err409 {errBody = UTF8.fromString err }
  _        -> pure ()

-- | Processes the given code.
--
-- Creates a clean empty directory, attempts to compile, run and test the code, and returns its result.
--
processAttempt :: UUID -> CodingProblem -> Text -> IO () 
processAttempt uid cp code = do
  let tempDir = "temp/" ++ show uid
      tempDirMain = tempDir ++ "/Main.hs" 

  -- | Clean removes temp dir
  ignoreIOFail $ removeDirectoryRecursive tempDir

  -- | Creates temp dir
  createDirectoryIfMissing True tempDir

  -- | Create Main file for compilation
  writeFile tempDirMain (unpack code)

  -- | Compile file to same dir/dist (thus check validity of code)
  -- r <-  runProcess (proc "ghc" [tempDirMain])
  throwIfError $ runProcess (proc "ghc" [tempDirMain])
  
  -- | Confirm working executable
  throwIfError $ runProcess (proc (tempDir ++ "/main") [])

  -- | run file as test/on test cases
      -- Get testcases 
      -- Run over all of them / input them as list of cases
      
  -- | Build up result of code run

  -- | Remove temp dir
  -- runWait ("rm -r " ++ tempDir)

  -- | Return result of run

  putStrLn "Done"
