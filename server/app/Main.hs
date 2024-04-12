{-# LANGUAGE OverloadedStrings  #-}
module Main where

-- | Dependency imports
import Control.Concurrent       (forkIO)
import Network.Wai.Handler.Warp (run)
import Servant.Server.Generic   (genericServeTWithContext)

-- | Project imports
import Security.API             (api)
import Security.App             (appToHandler, jwtDefaultContext)
import Security.Auth            (generateKey)
import System.Checks            (ghcCheckIO, awaitShutdown)
import Database.Config          (getConnection)

port :: Int
port = 8080

main :: IO ()
main = do
  ghcCheckIO "9.4.8"

  dbConn <- getConnection
  jwk <- generateKey
  let app = genericServeTWithContext appToHandler (api dbConn jwk) (jwtDefaultContext jwk)

  -- Fork the program to allow the program to take other inputs
  _ <- forkIO $ putStrLn banner >> run port app
  awaitShutdown

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
