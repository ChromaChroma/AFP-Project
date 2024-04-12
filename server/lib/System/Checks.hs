module System.Checks (ghcCheckIO, ghcCheck, awaitShutdown) where

import Control.Monad  (forever, guard)
import System.Exit    (ExitCode(..))
import System.Process (readProcessWithExitCode)


-- | Awaits the shutdown input and stops the program .
-- 
-- Unless forcably stopped in another way.
awaitShutdown :: IO ()
awaitShutdown = do 
  input <- getLine
  case input of 
    "q" -> putStrLn "Server Shutting Down."
    _    -> awaitShutdown


-- | Checks if GHC version is correctly installed on system and guards it
ghcCheckIO :: String ->  IO ()
ghcCheckIO ver = do
  installed <- ghcCheck ver
  guard installed

-- | Checks if GHC version is correctly installed on system
ghcCheck :: String -> IO Bool
ghcCheck v = 
  let errMsg = "GHC " ++ v ++ " needs to be installed and accessible for the server to work properly"
      checkOutput ("The Glorious Glasgow Haskell Compilation System, version ", right) 
        | right /= v = failMsg ("Wrong version of GHC installed, expected " ++ v ++ ", but was: " ++ show right)
        | otherwise  = succMsg "GHC correctly installed on server"
      checkOutput _  = failMsg errMsg
  in do
  (exitCode, output, _) <- readProcessWithExitCode "ghc" ["--version"] ""

  case exitCode of
    ExitSuccess   -> checkOutput . splitAt 57 $ head (lines output)
    ExitFailure _ -> failMsg errMsg

-- | Logs message and returns IO False
failMsg :: String -> IO Bool
failMsg msg = putStrLn msg >> pure False

-- | Logs message and returns IO True
succMsg :: String -> IO Bool
succMsg msg = putStrLn msg >> pure True 

