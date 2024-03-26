{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE DeriveAnyClass    #-}
module Security where 

-- import Servant
-- import Servant.Auth.Server
-- import Servant.Server.Generic ()
-- import Servant.API.Generic ((:-))

-- import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)


-- import Network.Wai (Request, requestHeaders)


-- import qualified Api.CodeProblem as CP
-- import Types

import Data.Text (Text)
-- import qualified Data.Map as Map (Map, fromList, lookup)

import Data.Aeson
import GHC.Generics
-- import Data.Proxy
-- import System.IO
-- import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.Wai.Handler.Warp
import Servant as S
-- import Servant.Client
import Servant.Auth as SA
import Servant.Auth.Server as SAS
-- import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as M
import Data.ByteString (ByteString)



-- Based on https://docs.servant.dev/en/stable/cookbook/jwt-and-basic-auth/JWTAndBasicAuth.html




----------------------------------------
----- Logic for accounts
----------------------------------------
data AuthenticatedUser = AUser 
  { auID :: Int 
  , auOrgID :: Int
  } deriving (Show, Generic, ToJSON, FromJSON, ToJWT, FromJWT)

----------------------------------------


----------------------------------------
----- Logic for accounts
----------------------------------------
type Login      = ByteString
type Password   = ByteString
type DB         = M.Map (Login, Password) AuthenticatedUser
type Connection = DB
type Pool a     = a
----------------------------------------
initConnPool :: IO (Pool Connection)
initConnPool = pure $ M.fromList [ 
    (("test", "pwd"), AUser 1 1)
  , (("admin", "pwd"), AUser 2 1) 
  ]

authCheck :: Pool Connection
          -> BasicAuthData
          -> IO (AuthResult AuthenticatedUser)
authCheck connPool (BasicAuthData login password) 
  = pure 
      $ maybe SAS.Indefinite Authenticated 
      $ M.lookup (login, password) connPool


type instance BasicAuthCfg = BasicAuthData -> IO (AuthResult AuthenticatedUser)

instance FromBasicAuthData AuthenticatedUser where
  fromBasicAuthData authData authCheckFunction = authCheckFunction authData



data LoginDTO = LoginDTO {
    username :: String,
    password :: String
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToJWT, FromJWT)


type LoginAPI = "login" :> ReqBody '[JSON] LoginDTO 
                  :> Post '[JSON] (Headers '[Header "Authorization" String] NoContent)

type TestAPIServer = 
  Auth '[SA.JWT] AuthenticatedUser 
    :> "foo" :> Capture "i" Int 
    :> Get '[JSON] String 

type TotalTestAPI = TestAPIServer :<|> LoginAPI


apiServer :: Server TotalTestAPI
apiServer = server :<|> loginServer

loginServer :: Server LoginAPI
loginServer = login
  where 
    login :: LoginDTO -> Handler (Headers '[Header "Authorization" String] NoContent)
    login (LoginDTO usrName usrPwd) = return $ addHeader ("Bearer: SomeToken123+"<>usrName<>"+"<>usrPwd) NoContent

server :: Server TestAPIServer
server (Authenticated user) = handleFoo 
  where
    handleFoo :: Int -> Handler String
    handleFoo n = return $ concat ["foo: ", show user, " / ", show n]
server _ = throwAll err401



mkApp :: Pool Connection -> IO Application
mkApp connPool = do
  myKey <- generateKey
  let jwtCfg = defaultJWTSettings myKey
      authCfg = authCheck connPool
      cfg = jwtCfg :. defaultCookieSettings :. authCfg :. EmptyContext
      api = Proxy :: Proxy TotalTestAPI
  pure $ serveWithContext api cfg apiServer

main :: IO ()
main = do
  connPool <- initConnPool
  let port = 8001
      settings = setPort port $ setBeforeMainLoop 
        (putStrLn ("listening on port " ++ show port)) 
        defaultSettings
  runSettings settings =<< mkApp connPool

-- todo:
-- https://nicolasurquiola.ar/blog/2023-10-28-generalised-auth-with-jwt-in-servant



-- -- | A (pure) database mapping keys to accounts.
-- database :: M.Map String Account
-- database = M.fromList [ ("tester", Account "test")]

-- | A method that, when given a password, will return an Account.
-- This is our bespoke (and bad) authentication logic.

-- lookupAccount :: String -> Handler Account
-- lookupAccount key = case M.lookup key database of
--   Nothing -> throwError (err403 { errBody = "Invalid Cookie" })
--   Just usr -> return usr

----------------------------------------

----------------------------------------
----- Logic for accounts
----------------------------------------
-- Here is the login handler
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

-- -- mainWithCookies = do
-- --   -- We *also* need a key to sign the cookies
-- --   myKey <- generateKey
-- --   -- Adding some configurations. 'Cookie' requires, in addition to
-- --   -- CookieSettings, JWTSettings (for signing), so everything is just as before
-- --   let jwtCfg = defaultJWTSettings myKey
-- --       cfg = defaultCookieSettings :. jwtCfg :. EmptyContext
-- --       --- Here is the actual change
-- --       api = Proxy :: Proxy (API '[Cookie])
-- --   run 7249 $ serveWithContext api cfg (server defaultCookieSettings jwtCfg
