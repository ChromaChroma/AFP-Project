{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeApplications  #-}
module Main where

-- | Dependency imports
import Control.Concurrent       (forkIO)
import Control.Monad            (forever, guard)
import Control.Monad.Catch      (MonadThrow, try)
import Control.Monad.Except     (ExceptT(..))
import Control.Monad.Identity   (IdentityT (..))
import Control.Monad.IO.Class   (MonadIO)
import Network.Wai.Handler.Warp (run)
import Servant.Server.Generic   (genericServeTWithContext)
import Servant                  (Context(..), Handler(..))
-- | Project imports
import Security.API             (api)
import Security.App             (appToHandler)
import Security.Auth            (authHandler, generateKey)
import Security.Claims          (AccessClaims, RefreshClaims, accessSettings, refreshSettings)

import System.Process
import System.Exit

port :: Int
port = 8080

ghcCheck :: IO Bool
ghcCheck = do
  (exitCode, output, _) <- readProcessWithExitCode "ghc" ["--version"] ""
  let errMsg = "GHC 9.4.8 needs to be installed and accessible for the server to work properly"

  -- Check if GHC is installed properly
  case exitCode of
    ExitSuccess   -> case splitAt 57 $ head (lines output) of 
      ("The Glorious Glasgow Haskell Compilation System, version " , "9.4.8") -> putStrLn "GHC correctly installed on server" >> pure True
      ("The Glorious Glasgow Haskell Compilation System, version " , right)   -> putStrLn ("Wrong version of GHC installed, expected 9.4.8, but was: " ++ show right) >> pure False
      _                                                                       -> putStrLn errMsg >> pure False
    ExitFailure _ -> putStrLn errMsg >> pure False

main :: IO ()
main = do
  installed <- ghcCheck
  guard installed

  putStrLn banner

  jwk <- generateKey
  let ctx = authHandler @AccessClaims jwk accessSettings
        :. authHandler @RefreshClaims jwk refreshSettings
        :. EmptyContext
      app = genericServeTWithContext appToHandler (api jwk) ctx

  -- Fork the program to allow the program to take other inputs
  _ <- forkIO $ run port app

  awaitShutdown

-- | Awaits the shutdown input and stops the program .
-- 
-- Unless forcably stopped in another way.
awaitShutdown :: IO ()
awaitShutdown = do 
  input <- getLine
  case input of 
    "q" -> putStrLn "Server Shutting Down."
    _    -> awaitShutdown

-- | Banner that is printed at startup
--
-- Includes run information and input keywords the user can input.
banner :: String
banner = "\n"
  ++ " ██████╗ ██████╗ ██████╗ ███████╗     ██████╗ ██████╗ ███╗   ███╗███╗   ███╗██╗████████╗\n"
  ++ "██╔════╝██╔═══██╗██╔══██╗██╔════╝    ██╔════╝██╔═══██╗████╗ ████║████╗ ████║██║╚══██╔══╝\n"
  ++ "██║     ██║   ██║██║  ██║█████╗      ██║     ██║   ██║██╔████╔██║██╔████╔██║██║   ██║   \n"
  ++ "██║     ██║   ██║██║  ██║██╔══╝      ██║     ██║   ██║██║╚██╔╝██║██║╚██╔╝██║██║   ██║   \n"
  ++ "╚██████╗╚██████╔╝██████╔╝███████╗    ╚██████╗╚██████╔╝██║ ╚═╝ ██║██║ ╚═╝ ██║██║   ██║   \n"
  ++ " ╚═════╝ ╚═════╝ ╚═════╝ ╚══════╝     ╚═════╝ ╚═════╝ ╚═╝     ╚═╝╚═╝     ╚═╝╚═╝   ╚═╝   \n"
  ++ "\n"
  ++ "  Running server on port: " ++ show port ++ "\n"
  ++ "  Enter q to shut down\n"
