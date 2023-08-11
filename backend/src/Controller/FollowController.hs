{-# LANGUAGE OverloadedStrings #-}
module Controller.FollowController where

import Servant
import qualified Service.FollowService as FollowService
import Data.UUID (UUID, fromString)
import Schema (User, FollowInsert)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (void)
import Database.PostgreSQL.Simple (Connection)

getFollowingHandler :: Connection -> UUID -> Handler [User]
getFollowingHandler conn userId = liftIO $ FollowService.getFollowing conn userId

getFollowersHandler :: Connection -> UUID -> Handler [User]
getFollowersHandler conn userId = liftIO $ FollowService.getFollowers conn userId

insertFollowHandler :: Connection -> FollowInsert -> Handler ()
insertFollowHandler conn follow = liftIO (FollowService.insertFollow conn follow)

deleteFollowHandler :: Connection -> Maybe UUID -> Maybe UUID -> Handler ()
deleteFollowHandler conn Nothing Nothing = do
  return ()
deleteFollowHandler conn Nothing userFollowerId = do
  return ()
deleteFollowHandler conn userFollowedId Nothing = do
  return ()
deleteFollowHandler conn (Just userFollowedId) (Just userFollowerId) = liftIO (FollowService.deleteFollow conn userFollowedId userFollowerId)
