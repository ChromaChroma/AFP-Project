module System.Processing where 


-- | Dependency imports
import Control.Monad  (liftM)
import Control.Monad.IO.Class   (liftIO)
import Data.Text      (Text)
import System.Exit    (ExitCode(..))
import System.IO.Unsafe (unsafePerformIO)
import System.Process (runCommand, waitForProcess)
-- | Project imports
import Dummy          (dummyUUID)


type CommandMonadIO a b = CommandMonad a (IO b)

data CommandMonad a b = Command b | Failure a 

instance Functor (CommandMonad a) where
  fmap f (Command y) = y `seq` Command (f y)
  fmap _ (Failure e) = Failure e

instance Applicative (CommandMonad e) where
  pure = Command
  Command f <*> x = f `seq` fmap f x
  Failure e <*> _ = Failure e

instance Monad (CommandMonad e) where
  Command x >>= f = x `seq` f x
  Failure e >>= _ = Failure e


-- ---------------------------------------------------------------------------
-- Functions over CommandMonad

-- | Returns 'True' iff its argument is of the form @Command _@ 
isCommand :: CommandMonad e a -> Bool
isCommand (Command _) = True
isCommand _           = False

-- | Returns 'True' iff its argument is of the form @Failure _@ 
isFailure :: CommandMonad e a -> Bool
isFailure (Failure _) = True
isFailure _           = False

-- | Takes a default value and a 'CommandMonad'. 
-- If ÇommandMonad is of the form @Command _@ it returns the command value, otherwise the default value.
fromCommand :: a -> CommandMonad e a -> a
fromCommand _ (Command x) = x
fromCommand d (Failure e) = d

-- | Takes a default value and a 'CommandMonad'. 
-- If ÇommandMonad is of the form @Failure _@ it the failure value, otherwise the default value.
fromFailure :: e -> CommandMonad e a -> e
fromFailure _ (Failure e) = e
fromFailure d (Command _) = d


updateFail :: CommandMonadIO ExitCode ExitCode -> CommandMonadIO ExitCode ExitCode
updateFail (Command x) = case unsafePerformIO x of 
  ExitSuccess   -> Command x
  ExitFailure i -> Failure (ExitFailure i)

updateSucceed :: CommandMonadIO ExitCode ExitCode -> CommandMonadIO ExitCode ExitCode
updateSucceed (Failure x) = case x of 
  ExitSuccess   -> Command $ pure x
  ExitFailure i -> Command $ pure (ExitFailure i)
updateSucceed (Command x) = case unsafePerformIO x of 
  ExitSuccess   -> Command x
  ExitFailure i -> Command $ pure (ExitFailure i)


-- | Takes a shell command as String, runs it, waits for its completion and returns the 'ExitCode'
-- 
-- ==== __Examples__
--
-- >>> fromCommand $ await "ghc --version" -- Given that ghc is installed of course
-- Command ExitSuccess
-- 
-- >>> fromCommand $ await "someErroneousCall 123" 
-- Command (ExitFailure 1)
--
await :: Bool -> String -> CommandMonadIO ExitCode ExitCode
await ignoreFailure cmnd = 
  let updateFunc = if ignoreFailure then updateSucceed else updateFail
  -- updateFail . 
  -- -- updateSucceed .
  in updateFunc . pure $
  do 
    handle <- runCommand cmnd 
    return $! unsafePerformIO (waitForProcess handle)

  -- let exitCode = do 
  --                 handle   <- runCommand cmnd 
  --                 waitForProcess handle

  --     f exitCode = case exitCode of 
  --                   ExitSuccess     -> Command exitCode
  --                   (ExitFailure i) -> Failure $ ExitFailure i
  -- in liftM f exitCode


-- liftIOCommandMonad :: IO a -> IO () -> CommandMonadIO e a
-- liftIOCommandMonad x io = io `seq` pure x


{-
TODO: 
Running dooo (normal process) right after repl instance, works correct, then IF deleting dir it should create before making file, 
gets error that dir it wants to make file in does not exist.

-}







good :: CommandMonadIO ExitCode ExitCode
good = Command $ pure $ ExitFailure 1


dooo2 :: IO ()
dooo2 = do 
  case updateFail good of 
    Command x -> x >>= print
    Failure e -> error "Some unexpected failure"


dooo :: IO ()
dooo = do 
  case compileFile "text c" of 
    Command x -> x >>= putStrLn . ("tt" ++) . show 
    Failure e -> error "Some unexpected failure"


compileFile :: Text -> CommandMonadIO ExitCode ExitCode
compileFile code = 
  let tempDir = "temp/" ++ show dummyUUID -- Temp dir of user
  in do 
  await True  ("rm -r " ++ tempDir)
  await False ("install -Dv /dev/null " ++ tempDir ++ "/Main.hs" 
              ++ " && echo " ++ show code ++ " > " ++ tempDir ++ "/Main.hs")
  -- await False ("mkdir -p " ++ tempDir)
  -- await False ("echo " ++ show code ++ " > " ++ tempDir ++ "/Main.hs")

  -- Gets called (after deleting dir) before creation is done. 
  -- Misshien dit gebruiken: https://stackoverflow.com/questions/48977455/how-do-i-force-evaluation-of-an-io-action-within-unsafeperformio
  await False ("cat " ++ tempDir ++ "/Main.hs")

  
  -- await False("(rm -r " ++ tempDir ++ "|| true )" ++ " && mkdir -p " ++ tempDir ++ " && echo " ++ show code ++ " > " ++ tempDir ++ "/Main.hs")

  -- liftIOCommandMonad r $ putStrLn "Done Compiling"
  -- | Compile file to same dir/dist (thus check validity of code)
  
  -- | run file as test/on test cases

  -- | Build up result of code run

  -- | Remove temp dir
  -- runWait ("rm -r " ++ tempDir)

  -- | Return result of run
  -- putStrLn "Done Compiling"

  -- liftIO $ putStrLn "Done Compiling"


{- 
runWait :: String -> IO ExitCode
runWait cmnd = runCommand cmnd >>= waitForProcess  

compileFile :: Text -> IO ()
compileFile code = 
  let tempDir = "temp/" ++ show dummyUUID -- Temp dir of user
  in do 
  -- now <- liftIO getCurrentTime
  
 
  -- | Clean temp dir
  runWait ("rm -r " ++ tempDir)
  
  -- | Create temp dir
  runWait ("mkdir -p " ++ tempDir)
  -- (exitCode, output, err) <- readProcessWithExitCode "mkdir" [] tempDir
  -- guard (exitCode == ExitSuccess)

  -- -- | Create file to be compiled
  runWait ("echo " ++ show code ++ " >> " ++ tempDir ++ "/Main.hs")

  -- | Compile file to same dir/dist (thus check validity of code)
  
  -- | run file as test/on test cases

  -- | Build up result of code run

  -- | Remove temp dir
  -- runWait ("rm -r " ++ tempDir)

  -- | Return result of run
  putStrLn "Done Compiling"
-}