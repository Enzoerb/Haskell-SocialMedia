{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module API where

import Servant
import Schema (User, UserInsert, UserUpdate)
import Data.UUID (UUID)
import Control.Monad.IO.Class (liftIO)
import qualified Service.UserService as UserService

type API =
    "users" :> Get '[JSON] [User]
    :<|> "user" :> "username" :> Capture "username" String :> Get '[JSON] (Maybe User)
    :<|> "user" :> "email" :> Capture "email" String :> Get '[JSON] (Maybe User)
    :<|> "user" :> "id" :> Capture "id" UUID :> Get '[JSON] (Maybe User)
    :<|> "user" :> ReqBody '[JSON] UserInsert :> Put '[JSON] ()
    :<|> "user" :> ReqBody '[JSON] UserUpdate :> Patch '[JSON] ()
    :<|> "user" :> Capture "id" UUID :> Delete '[JSON] ()

api :: Proxy API
api = Proxy
