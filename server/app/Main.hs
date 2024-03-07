{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
-- {-# LANGUAGE RecordWildCards   #-}
-- {-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts  #-}

module Main where

import qualified Security

-- import Prelude hiding (words, getLine)

-- -- import Network.Wai
-- import Network.Wai.Handler.Warp

-- -- import Control.Lens
-- import Control.Monad (forever)
-- import Control.Concurrent (forkIO)
-- import Control.Monad.IO.Class (liftIO)

-- import Data.Aeson
-- import Data.Aeson.Encode.Pretty (encodePretty)
-- import Data.Proxy
-- import Data.Text 
-- import Data.Text.IO (getLine)
-- import Data.Time (UTCTime (..), fromGregorian)
-- import GHC.Generics
-- import qualified Data.ByteString.Lazy.Char8 as BL8
-- import Data.Typeable (Typeable)

-- import Servant
-- import Servant.Server.Generic ()
-- import Servant.API.Generic ((:-))

-- import Servant.Auth.Server

-- import qualified Api.CodeProblem as CP
-- import Types


-- import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)
-- import Network.Wai (Request, requestHeaders)
-- import qualified Data.Map as Map (Map, fromList, lookup)




-- -- | private data that needs protection
-- newtype PrivateData = PrivateData { ssshhh :: Text } deriving (Eq, Show, Generic)
-- instance ToJSON PrivateData

-- -- | public data that anyone can use.
-- newtype PublicData = PublicData { somedata :: Text } deriving (Eq, Show, Generic)
-- instance ToJSON PublicData

-- -- | A user we'll grab from the database when we authenticate someone
-- newtype User = User { userName :: Text }
--   deriving (Eq, Show)

-- -- -- | a value holding a proxy of our API type
-- -- basicAuthApi :: Proxy BasicAPI
-- -- basicAuthApi = Proxy




-- ----------------------------------------
-- -- | An account type that we "fetch from the database" after
-- -- performing authentication
-- newtype Account = Account { unAccount :: Text }

-- -- | A (pure) database mapping keys to accounts.
-- database :: Map.Map String Account
-- database = Map.fromList [ ("tester", Account "test")]

-- -- | A method that, when given a password, will return an Account.
-- -- This is our bespoke (and bad) authentication logic.
-- lookupAccount :: String -> Handler Account
-- lookupAccount key = case Map.lookup key database of
--   Nothing -> throwError (err403 { errBody = "Invalid Authorization token" })
--   Just usr -> return usr

-- --- | The auth handler wraps a function from Request -> Handler Account.
-- --- We look for a token in the request headers that we expect to be in the cookie.
-- --- The token is then passed to our `lookupAccount` function.
-- authHandler :: AuthHandler Request Account
-- authHandler = mkAuthHandler handler
--   where
--   maybeToEither e = maybe (Left e) Right
--   throw401 msg = throwError $ err401 { errBody = msg }

--   handler req = either throw401 (lookupAccount . show) $ do
--     maybeToEither "Missing Authorization header" $ lookup "Authorization" $ requestHeaders req
--     -- maybeToEither "Missing token in cookie" $ lookup "servant-auth-cookie" $ parseCookies cookie


-- -- | Our API, with auth-protection
-- type AuthGenAPI = 
--     "private" :> Get '[JSON] PrivateData
--   :<|> 
--   "public"  :> Get '[JSON] [PublicData]

-- -- | A value holding our type-level API
-- genAuthAPI :: Proxy AuthGenAPI
-- genAuthAPI = Proxy


-- -- | We need to specify the data returned after authentication
-- type instance AuthServerData (AuthProtect "cookie-auth") = Account

-- -- | The context that will be made available to request handlers. We supply the
-- -- "cookie-auth"-tagged request handler defined above, so that the 'HasServer' instance
-- -- of 'AuthProtect' can extract the handler and run it on the request.
-- genAuthServerContext :: Context (AuthHandler Request Account ': '[])
-- genAuthServerContext = authHandler :. EmptyContext

-- -- | Our API, where we provide all the author-supplied handlers for each end
-- -- point. Note that 'privateDataFunc' is a function that takes 'Account' as an
-- -- argument. We don't worry about the authentication instrumentation here,
-- -- that is taken care of by supplying context
-- genAuthServer :: Server AuthGenAPI
-- genAuthServer =
--   let privateDataFunc = return (PrivateData "private")
--       -- privateDataFunc (Account name) =
--       --     return (PrivateData ("this is a secret: " <> name))
--       publicData = return [PublicData "this is a public piece of data"]
--   in  privateDataFunc :<|> publicData

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

main :: IO ()
main = do 
  Security.main
  putStrLn "ddddd"
  -- genAuthMain
  -- do
  -- let port = 8001
  -- putStrLn $ "Running server on " <> show port
  -- run port $ serve apiProxy server
  -- genAuthMain

-- main :: IO ()
-- main = do
-- -- We generate the key for signing tokens. This would generally be persisted,
-- -- and kept safely
--   myKey <- generateKey
--   -- Adding some configurations. All authentications require CookieSettings to
--   -- be in the context.
--   let jwtCfg = defaultJWTSettings myKey
--       cfg = defaultCookieSettings :. jwtCfg :. EmptyContext
--       --- Here we actually make concrete
--       api = Proxy :: Proxy (API '[JWT])
--   _ <- forkIO $ run 8001 $ serveWithContext apiProxy cfg (server defaultCookieSettings jwtCfg)
--   -- _ <- run 8001$  serveWithContext apiProxy cfg (server defaultCookieSettings jwtCfg)
  
--   putStrLn "Started server on localhost:8001"
--   putStrLn "Enter name and email separated by a space for a new token"

--   forever $ do
--     xs <- words <$> getLine
--     case xs of
--       [name', pwd'] -> do
--         etoken <- makeJWT (User "222" name' pwd' UserRole) jwtCfg Nothing
--         case etoken of
--           Left e -> putStrLn $ "Error generating token:t" ++ show e
--           Right v -> putStrLn $ "New token:\t" ++ show v
--       _ -> putStrLn "Expecting a name and email separated by spaces"
      
--   putStrLn "Done"

-- -- mainWithCookies :: IO ()
-- -- mainWithCookies = do
-- --   -- We *also* need a key to sign the cookies
-- --   myKey <- generateKey
-- --   -- Adding some configurations. 'Cookie' requires, in addition to
-- --   -- CookieSettings, JWTSettings (for signing), so everything is just as before
-- --   let jwtCfg = defaultJWTSettings myKey
-- --       cfg = defaultCookieSettings :. jwtCfg :. EmptyContext
-- --       --- Here is the actual change
-- --       api = Proxy :: Proxy (API '[Cookie])
-- --   run 7249 $ serveWithContext api cfg (server defaultCookieSettings jwtCfg)

-- -- Here is the login handler
-- checkCreds :: CookieSettings
--            -> JWTSettings
--            -> Login
--            -> Handler (Headers '[ Header "Set-Cookie" SetCookie
--                                 , Header "Set-Cookie" SetCookie]
--                                NoContent)
-- checkCreds cookieSettings jwtSettings (Login "test" "pwd") = do
--    -- Usually you would ask a database for the user info. This is just a
--    -- regular servant handler, so you can follow your normal database access
--    -- patterns (including using 'enter').
--    let usr = User "111" "Ali Baba" "ali@email.com" AdminRole
--   --  let usr = User "Ali Baba" "ali@email.com"
--    mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings usr
--    case mApplyCookies of
--      Nothing           -> throwError err401
--      Just applyCookies -> return $ applyCookies NoContent
-- checkCreds _ _ _ = throwError err401

