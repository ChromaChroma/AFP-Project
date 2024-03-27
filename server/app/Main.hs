{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeApplications  #-}

module Main where

-- | Dependency imports
import Control.Concurrent       (forkIO)
import Control.Monad            (forever, guard)
import Control.Monad.Catch      (MonadThrow, try, SomeException(..))
import Control.Monad.Except     (ExceptT(..))
import Control.Monad.Identity   (IdentityT (..))
import Control.Monad.IO.Class   (MonadIO)
import Data.Either              (Either(..))
import Data.Text                (Text)
import Network.Wai.Handler.Warp (run)
import Servant.Server.Generic   (genericServeTWithContext)
import Servant                  (Context(..), Handler(..))
import System.Process
import System.Exit
-- | Project imports
import Security.API             (api)
import Security.App             (appToHandler)
import Security.Auth            (authHandler, generateKey)
import Security.Claims          (AccessClaims, RefreshClaims, accessSettings, refreshSettings)
import Dummy                    (dummyUUID)
import System.Checks            (ghcCheck)

port :: Int
port = 8080


main :: IO ()
main = do
  installed <- ghcCheck "9.4.8"
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
