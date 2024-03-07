{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Api.CodeProblem where

import Data.Aeson
import Data.Proxy
import Data.Text 
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Data.Typeable (Typeable)
import GHC.Generics

import Network.Wai.Handler.Warp
import Servant
import Servant.Server.Generic ()
import Servant.API.Generic ((:-))

--API

userApi :: Proxy UserAPI
userApi = Proxy

type UserAPI = 
  "users" :> Get '[JSON] [User] 
    -- | GET /hello/:name?capital={true, false}  returns a Greet as JSON
  :<|> "users" :> Capture "name" Text :> QueryParam "capital" Bool :> Get '[JSON] User

-- Datatypes

data User = User
  { name :: Text,
    age :: Int,
    email :: Text,
    registration_date :: UTCTime
  } 
  deriving (Generic, Show, Typeable)

-- type CodeProblemAPI = "code-problems"  :>

instance FromJSON User
instance ToJSON User

-- Server-side handlers
userApiHandlers :: Server UserAPI
userApiHandlers = users :<|> helloUser
  where 
    randomDate = UTCTime (fromGregorian 2018 10 27) (secondsToDiffTime 0)

    users = return [ User "John Doe" 35 "some@gmail.com" randomDate ]

    helloUser name Nothing      = helloUser name (Just False)
    helloUser name (Just False) = return $ User name 35 "some@gmail.com" randomDate
    helloUser name (Just True)  = return $ User (toUpper name) 35 "some@gmail.com" randomDate