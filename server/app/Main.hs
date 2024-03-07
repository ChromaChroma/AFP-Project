{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

-- import Network.Wai.Handler.Warp


-- import Network.Wai
import Network.Wai.Handler.Warp

import Control.Lens
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Proxy
import Data.Swagger
import Data.Text
import Data.Time (UTCTime (..), fromGregorian)
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Typeable (Typeable)

import Servant
import Servant.Server.Generic ()
import Servant.API.Generic ((:-))

import qualified Api.CodeProblem as CP

-- * Example

-- -- | A greet message data type
-- newtype Greet = Greet { _msg :: Text }
--   deriving (Generic, Show, Typeable)

-- instance FromJSON Greet
-- instance ToJSON Greet

-- instance ToSchema Greet where
--   declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
--     & mapped.schema.description ?~ "This is some real Greet right here"
--     & mapped.schema.example ?~ toJSON (Greet "Example greet msg for John Doe")


-- -- | API for serving @swagger.json@.
-- type SwaggerAPI =  "swagger.json" :> Get '[JSON] Swagger
-- API specification
type API = CP.CodingProblemAPI -- :<|>

-- type TestApi =
--   SwaggerAPI
--        -- GET /hello/:name?capital={true, false}  returns a Greet as JSON
--   :<|> "hello" :> Capture "name" Text :> QueryParam "capital" Bool :> Get '[JSON] Greet
--        -- POST /greet with a Greet as JSON in the request body,
--        --             returns a Greet as JSON
--   :<|> "greet" :> ReqBody '[JSON] Greet :> Post '[JSON] Greet
--        -- DELETE /greet/:greetid
--   :<|> "greet" :> Capture "greetid" Text :> Delete '[JSON] NoContent
--   :<|> NamedRoutes OtherRoutes

-- data OtherRoutes mode = OtherRoutes
--   { version2 :: mode :- Get '[JSON] Int
--   , bye :: mode :- "bye" :> Capture "name" Text :> Get '[JSON] Text
--   }
--   deriving Generic


-- Server-side handlers.
--
-- There's one handler per endpoint, which, just like in the type
-- that represents the API, are glued together using :<|>.
--
-- Each handler runs in the 'Handler' monad.
-- server :: Server TestApi
-- server = return swagger :<|> helloH :<|> postGreetH :<|> deleteGreetH :<|> otherRoutes
--   where otherRoutes = OtherRoutes {..}

--         bye name = pure $ "Bye, " <> name <> " !"
--         version = pure 42

--         helloH name Nothing = helloH name (Just False)
--         helloH name (Just False) = return . Greet $ "Hello, " <> name
--         helloH name (Just True) = return . Greet . toUpper $ "Hello, " <> name

--         postGreetH greet = return greet

--         deleteGreetH _ = return NoContent

-- testApi :: Proxy TestApi
-- testApi = Proxy

-- -- | Swagger spec for Todo API.
-- swagger :: Swagger
-- swagger = toSwagger testApi
--   & info.title   .~ "Todo API"
--   & info.version .~ "1.0"
--   & info.description ?~ "This is an API that tests swagger integration"
--   & info.license ?~ ("MIT" & url ?~ URL "http://mit.com")


-- -- | Output generated @swagger.json@ file for the @'TodoAPI'@.
-- writeSwaggerJSON :: IO ()
-- writeSwaggerJSON = BL8.writeFile "server/swagger.json" (encodePretty swagger)


-- Put this all to work!
main :: IO ()
main = do
  putStrLn "Running server on 8001"
  -- runTestServer 8001
  run 8001 $ serve (Proxy :: Proxy API) CP.handlers