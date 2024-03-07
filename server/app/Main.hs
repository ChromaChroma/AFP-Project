{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE RecordWildCards   #-}
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
import Servant.Swagger





-- * Example

-- | A greet message data type
newtype Greet = Greet { _msg :: Text }
  deriving (Generic, Show, Typeable)

instance FromJSON Greet
instance ToJSON Greet


instance ToSchema Greet where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "This is some real Greet right here"
    & mapped.schema.example ?~ toJSON (Greet "Example greet msg for John Doe")


-- API specification
type TestApi =
      -- | API for serving @swagger.json@.
      "swagger.json" :> Get '[JSON] Swagger

       -- GET /hello/:name?capital={true, false}  returns a Greet as JSON
  :<|> "hello" :> Capture "name" Text :> QueryParam "capital" Bool :> Get '[JSON] Greet

       -- POST /greet with a Greet as JSON in the request body,
       --             returns a Greet as JSON
  :<|> "greet" :> ReqBody '[JSON] Greet :> Post '[JSON] Greet

       -- DELETE /greet/:greetid
  :<|> "greet" :> Capture "greetid" Text :> Delete '[JSON] NoContent

  :<|> NamedRoutes OtherRoutes

data OtherRoutes mode = OtherRoutes
  { version2 :: mode :- Get '[JSON] Int
  , bye :: mode :- "bye" :> Capture "name" Text :> Get '[JSON] Text
  }
  deriving Generic

testApi :: Proxy TestApi
testApi = Proxy

-- Server-side handlers.
--
-- There's one handler per endpoint, which, just like in the type
-- that represents the API, are glued together using :<|>.
--
-- Each handler runs in the 'Handler' monad.
server :: Server TestApi
server = todoSwagger :<|> helloH :<|> postGreetH :<|> deleteGreetH :<|> otherRoutes
  where otherRoutes = OtherRoutes {..}

        bye name = pure $ "Bye, " <> name <> " !"
        version = pure 42

        helloH name Nothing = helloH name (Just False)
        helloH name (Just False) = return . Greet $ "Hello, " <> name
        helloH name (Just True) = return . Greet . toUpper $ "Hello, " <> name

        postGreetH greet = return greet

        deleteGreetH _ = return NoContent

-- | Swagger spec for Todo API.
todoSwagger :: Swagger
todoSwagger = toSwagger testApi
  & info.title   .~ "Todo API"
  & info.version .~ "1.0"
  & info.description ?~ "This is an API that tests swagger integration"
  & info.license ?~ ("MIT" & url ?~ URL "http://mit.com")

-- -- Turn the server into a WAI app. 'serve' is provided by servant,
-- -- more precisely by the Servant.Server module.
-- test :: Application
-- test = serve testApi server

-- -- Run the server.
-- --
-- -- 'run' comes from Network.Wai.Handler.Warp
-- runTestServer :: Port -> IO ()
-- runTestServer port = run port test


-- | Output generated @swagger.json@ file for the @'TodoAPI'@.
writeSwaggerJSON :: IO ()
writeSwaggerJSON = BL8.writeFile "example/swagger.json" (encodePretty todoSwagger)

-- Put this all to work!
main :: IO ()
main = do
  putStrLn "Running server on 8001"
  -- runTestServer 8001
  run 8001 $ serve(Proxy :: Proxy TestApi) server