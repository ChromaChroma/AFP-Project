{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE TypeFamilies   #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE OverloadedStrings #-}

module Security.API (api, Api, AuthJwtAccess, AuthJwtRefresh) where

import Control.Exception (throw)
import Crypto.JWT (JWK)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Servant (AuthProtect , Capture , Get , JSON , NamedRoutes , Post , ReqBody , err401 , type (:>))
import Servant.API.Generic (type (:-))
import Servant.Server.Generic (AsServerT)

import Security.App (App)
import Security.Handlers (LoginRequest , LoginResponse , getUserHandler , loginHandler , refreshTokenHandler)
import Security.User (User(..))
import Security.Auth (AuthJwtAccess, AuthJwtRefresh)
import Security.Claims (AccessClaims)

import  Api.CodeProblem
import Control.Monad.Catch (MonadThrow(..))
import Data.Text (Text)
import Servant (err401, err404)

data Api mode = Api  
  { -- | POST /login
    login  :: mode :- "login" :> ReqBody '[JSON] LoginRequest :> Post '[JSON] LoginResponse
    -- | POST /refresh
  , refresh :: mode :- "refresh" :> AuthJwtRefresh :> Post '[JSON] LoginResponse
  , secured :: mode :- AuthJwtAccess :> NamedRoutes SecuredRoutes
  , codingProblems :: CodingProblemAPI mode
  }
  deriving Generic

api :: JWK -> Api (AsServerT App)
api jwk = Api
  { login   = loginHandler jwk
  , refresh = refreshTokenHandler jwk
  , secured = securedHandlers
  , codingProblems = handlers
  }

----------------------------------------------------

newtype SecuredRoutes mode = SecuredRoutes
  { -- GET /users/:userId
    getUser :: mode :- "users" :> Capture "userId" UUID :> Get '[JSON] User
  -- , codingProblem :: mode :- SubmitCodingAttempt
  }
  deriving Generic

securedHandlers :: Maybe AccessClaims -> SecuredRoutes (AsServerT App)
securedHandlers (Just _) = SecuredRoutes { 
  getUser = getUserHandler
  -- , codingProblem = submit 
  }
securedHandlers _        =  throw err401