{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module API where

import Servant
import Schema (User, UserInsert, UserUpdate, Post, PostInsert, PostUpdate)
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
    -- Post endpoints
    :<|> "posts" :> Get '[JSON] [Schema.Post]
    :<|> "posts" :> "user" :> Capture "user_id" UUID :> Get '[JSON] [Schema.Post]
    :<|> "posts" :> "follow" :> Capture "user_id" UUID :> Get '[JSON] [Schema.Post]
    :<|> "post" :> Capture "post_id" UUID :> Get '[JSON] (Maybe Schema.Post)
    :<|> "post" :> ReqBody '[JSON] PostInsert :> Put '[JSON] ()
    :<|> "post" :> ReqBody '[JSON] PostUpdate :> Patch '[JSON] ()
    :<|> "post" :> Capture "id" UUID :> Delete '[JSON] ()

api :: Proxy API
api = Proxy
