{-# LANGUAGE OverloadedStrings #-}

module Service.FollowService where

import Data.UUID (UUID)
import qualified Repository.FollowRepository as FollowRepo
import Database.PostgreSQL.Simple (Connection)
import Schema

deleteFollow :: Connection -> UUID -> UUID -> IO ()
deleteFollow conn userFollowedId userFollowerId = FollowRepo.deleteFollow conn userFollowedId userFollowerId

insertFollow :: Connection -> Schema.FollowInsert -> IO ()
insertFollow conn follow = FollowRepo.insertFollow conn follow

getFollowing :: Connection -> UUID -> IO [Schema.User]
getFollowing conn userId = FollowRepo.getFollowing conn userId

getFollowers :: Connection -> UUID -> IO [Schema.User]
getFollowers conn userId = FollowRepo.getFollowers conn userId
