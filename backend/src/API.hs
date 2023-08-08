{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module API where

import Servant
import Schema (User)
import Control.Monad.IO.Class (liftIO)
import qualified Service.UserService as UserService

type API =
    "users" :> Get '[JSON] [User]
    :<|> "user" :> Capture "username" String :> Get '[JSON] (Maybe User)

api :: Proxy API
api = Proxy
