{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE TypeFamilies   #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE DeriveGeneric  #-}

module Security.API (api, Api) where

import Control.Exception (throw)
import Crypto.JWT (JWK)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Servant (AuthProtect , Capture , Get , JSON , NamedRoutes , Post , ReqBody , err401 , type (:>))
import Servant.API.Generic (type (:-))
import Servant.Server.Experimental.Auth (AuthServerData)
import Servant.Server.Generic (AsServerT)

import Security.App (App)
import Security.Claims (AccessClaims, RefreshClaims)
import Security.Handlers (LoginRequest , LoginResponse , getUserHandler , loginHandler , refreshTokenHandler)
import Security.User (User(..))

----------------------------------------------------

type AuthJwtAccess  = AuthProtect "jwt-access'"
type AuthJwtRefresh = AuthProtect "jwt-refresh'"

type instance AuthServerData AuthJwtAccess  = Maybe AccessClaims
type instance AuthServerData AuthJwtRefresh = Maybe RefreshClaims

----------------------------------------------------

data Api mode = Api  
  { -- | POST /login
    login :: mode :- "login" :> ReqBody '[JSON] LoginRequest :> Post '[JSON] LoginResponse
    -- | POST /refresh
  , refresh :: mode :- "refresh" :> AuthJwtRefresh :> Post '[JSON] LoginResponse
  , secured :: mode :- AuthJwtAccess :> NamedRoutes SecuredRoutes
  }
  deriving Generic

api :: JWK -> Api (AsServerT App)
api jwk = Api
  { login   = loginHandler jwk
  , refresh = refreshTokenHandler jwk
  , secured = securedHandlers
  }

----------------------------------------------------

newtype SecuredRoutes mode = SecuredRoutes
  { -- GET /users/:userId
    getUser :: mode :- "users" :> Capture "userId" UUID :> Get '[JSON] User
  }
  deriving Generic

securedHandlers :: Maybe AccessClaims -> SecuredRoutes (AsServerT App)
securedHandlers (Just _) = SecuredRoutes { getUser = getUserHandler }
securedHandlers _        = throw err401