{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeApplications  #-}
module Main where

-- | Dependency Iimports
import Control.Concurrent       (forkIO)
import Control.Monad            (forever)
import Control.Monad.Catch      (MonadThrow, try)
import Control.Monad.Except     (ExceptT(..))
import Control.Monad.Identity   (IdentityT (..))
import Control.Monad.IO.Class   (MonadIO)
import Network.Wai.Handler.Warp (run)
import Servant.Server.Generic   (genericServeTWithContext)
import Servant                  (Context(..), Handler(..))

-- | Project imports
import Security.API    (api)
import Security.App    (appToHandler)
import Security.Auth   (authHandler, generateKey)
import Security.Claims (AccessClaims, RefreshClaims, accessSettings, refreshSettings)
-- import qualified Security


-- -- | run our server
-- genAuthMain :: IO ()
-- genAuthMain = run 8080 (serveWithContext genAuthAPI genAuthServerContext genAuthServer)

-- type API = CP.CodingProblemAPI 

-- apiProxy ::Proxy API 
-- apiProxy :: Proxy (API '[JWT])
-- apiProxy = Proxy

-- server :: Server API
-- server = CP.handlers

-- server :: CookieSettings -> JWTSettings -> Server (API auths)
-- server cs jwts = protected :<|> unprotected cs jwts :<|> CP.handlers


-- newtype App a = App (IdentityT IO a)
--   deriving (Functor, Applicative, Monad, MonadIO, MonadThrow)

-- appToHandler :: App a -> Handler a
-- appToHandler (App app) = (Handler . ExceptT . try . runIdentityT) app

port :: Int
port = 8080

main :: IO ()
main = do
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
