{-# LANGUAGE OverloadedStrings #-}
module Controller.FollowController where

import Servant
import qualified Service.FollowService as FollowService
import Data.UUID (UUID, fromString)
import Schema (User, FollowInsert)
import Control.Monad.IO.Class (liftIO)
import Database.PostgreSQL.Simple (Connection)

getFollowingHandler :: Connection -> UUID -> Handler [User]
getFollowingHandler conn userId = liftIO $ FollowService.getFollowing conn userId

getFollowerHandler :: Connection -> UUID -> Handler [User]
getFollowerHandler conn userId = liftIO $ FollowService.getFollower conn userId

insertFollowHandler :: Connection -> FollowInsert -> Handler ()
insertFollowHandler conn follow = liftIO (FollowService.insertFollow conn follow)

deleteFollowHandler :: Connection -> UUID -> UUID -> Handler ()
deleteFollowHandler conn userFollowedId userFollowerId = liftIO (FollowService.deleteFollow conn userFollowedId userFollowerId)
