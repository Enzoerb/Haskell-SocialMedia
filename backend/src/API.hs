{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module API where

import Servant
import Schema (User, UserInsert, UserUpdate, Post, PostInsert, PostUpdate, FollowInsert)
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
    -- Follow endpoints
    :<|> "follows" :> "following" :> Capture "user_id" UUID :> Get '[JSON] [User]
    :<|> "follows" :> "followers" :> Capture "user_id" UUID :> Get '[JSON] [User]
    :<|> "follow" :> ReqBody '[JSON] FollowInsert :> Put '[JSON] ()
    :<|> "follow" :> QueryParam "user_followed" UUID :> QueryParam "user_follower" UUID :> Delete '[JSON] ()
    -- Identicoin
    :<|> "identicon" :> Capture "hash" String :> Get '[JSON] (String)
    :<|> "healthcheck" :> Get '[JSON] (String)


api :: Proxy API
api = Proxy
