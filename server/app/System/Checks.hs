module System.Checks (ghcCheck) where

import System.Process (readProcessWithExitCode)
import System.Exit    (ExitCode(..))

-- | Checks if GHC version is correctly installed on system
ghcCheck :: String -> IO Bool
ghcCheck v = 
  let checkOutput ("The Glorious Glasgow Haskell Compilation System, version ", right) 
        | right /= v = failMsg ("Wrong version of GHC installed, expected " ++ v ++ ", but was: " ++ show right)
        | otherwise  = succMsg "GHC correctly installed on server"
      checkOutput _  = failMsg errMsg
      
      errMsg = "GHC " ++ v ++ " needs to be installed and accessible for the server to work properly"
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
