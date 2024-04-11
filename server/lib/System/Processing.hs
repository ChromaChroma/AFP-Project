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
import System.IO              (hPutStr, hGetContents)
import System.IO.Unsafe       (unsafePerformIO)
import System.Process         (StdStream(..), CreateProcess(..), waitForProcess, proc, createProcess)
import System.Directory       (createDirectoryIfMissing, removeDirectoryRecursive)
import Control.Exception      (ErrorCall, handle)
import System.IO.Error        (catchIOError)
-- | Project imports
import Types

import Debug.Trace (trace)

-- | Runs an IO computation and ignores any 'IOError' or 'ErrorCall' errors in they come up.
ignoreIOFail :: IO () -> IO ()
ignoreIOFail = handle handlerError . flip catchIOError handlIOError
  where
    -- | Handles IOExceptions thrown
    handlIOError :: IOError -> IO ()
    handlIOError e =  void (print e)
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
throwIfError :: MonadThrow m => m (Either String a) -> m ()
throwIfError io = io >>= \case
  Left err -> throwM err409 {errBody = UTF8.fromString err }
  _        -> pure ()

-- | Processes the given code.
--
-- Creates a clean empty directory, attempts to compile, run and test the code, and returns its result.
--
processAttempt :: UUID -> CodingProblem -> CodingProblemCases -> Text -> IO () 
processAttempt uid cp (CodingProblemCases _ cases) code = do
  let tempDir     = "temp/" ++ show uid
      tempDirMain = tempDir ++ "/Main.hs" 
      tempExe     = tempDir ++ "/main"

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
  throwIfError $ runProcess (proc tempExe [])

  -- | run file as test/on test cases
      -- Get testcases 
      -- Run over all of them / input them as list of cases
  let testCase  = head cases
  --TODO get either out here and return success or fail to usr
  ignoreIOFail (runTest tempExe testCase >>= print) 
  -- let tests = buildTestcases tempDirMain cases
      
  -- | Build up result of code run

  -- | Remove temp dir
  -- runWait ("rm -r " ++ tempDir)

  -- | Return result of run
  putStrLn "Done"


runTest :: String -> TestCase -> IO (Either String String)
runTest exePath (desc, inp, unpack -> out, _) = do 
  (_, Just oH, _, eH) <- createProcess (proc exePath [unpack inp]) 
                                                      { std_out = CreatePipe 
                                                      , std_err = CreatePipe }
  exitCode <- waitForProcess eH
  o <- safeHead . lines <$> hGetContents oH
  case exitCode of 
    ExitSuccess | o == out -> return $ Right o 
    ExitSuccess            -> return $ Left ("Testcase (\""++ unpack desc ++"\"): Expected " ++ out ++ ", but got: " ++ o)
    ExitFailure _          -> return $ Left "Failed Test run"


safeHead ::[String] -> String
safeHead []     = ""
safeHead (x:xs) = x

-- TODO
-- buildTestcases :: Text -> [TestCase] -> Text
-- buildTestcases _        []                         = ""
-- buildTestcases mainPath ((desc, inp, out, _) : xs) = runTest out (proc mainPath [inp]) : buildTestcases xs