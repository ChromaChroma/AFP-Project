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
import Network.Wai.Middleware.Cors (simpleCors, cors, CorsResourcePolicy(..))
import Servant                  (Raw, Context(..), Handler(..))
import Servant.Server           (Application)
import Servant.Server.Generic   ( genericServeTWithContext)
import System.Process
import System.Exit
-- | Project imports
import Security.API             (Api, api)
import Security.App             (App, appToHandler)
import Security.Auth            (authHandler, generateKey)
import Security.Claims          (AccessClaims, RefreshClaims, accessSettings, refreshSettings)
import Dummy                    (dummyUUID)
import System.Checks            (ghcCheck)

port :: Int
port = 8080

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

enableCors :: Application -> Application
enableCors = cors (const $ Just corsSettings)
  where 
    corsSettings =  CorsResourcePolicy
      { corsOrigins = Nothing
      , corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]  -- Specify allowed HTTP methods
      , corsRequestHeaders = ["Content-Type"]
      , corsExposedHeaders = Nothing
      , corsMaxAge = Nothing
      , corsVaryOrigin = False
      , corsRequireOrigin = False
      , corsIgnoreFailures = False
      }

-- | Awaits the shutdown input and stops the program .
-- 
-- Unless forcably stopped in another way.
awaitShutdown :: IO ()
awaitShutdown = do 
  input <- getLine
  case input of 
    "q" -> putStrLn "Server Shutting Down."
    _    -> awaitShutdown

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
  _ <- forkIO . run port . enableCors $ app

  awaitShutdown
