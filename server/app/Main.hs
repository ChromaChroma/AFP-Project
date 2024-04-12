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