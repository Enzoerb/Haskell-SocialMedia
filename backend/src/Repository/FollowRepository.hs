{-# LANGUAGE OverloadedStrings #-}

module Repository.FollowRepository where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime, formatTime)
import qualified Data.Time.Format as TimeFormat
import qualified Data.Time.Clock as Clock
import Data.UUID (UUID, fromText)
import qualified Data.UUID as UUID
import Data.UUID.V4 (nextRandom)
import Database.PostgreSQL.Simple (Connection, connect, close, execute, query, query_, defaultConnectInfo)
import qualified Database.PostgreSQL.Simple as PGSimple
import qualified Database.PostgreSQL.Simple.FromRow as PGFromRow
import qualified Database.PostgreSQL.Simple.ToField as PGToField
import qualified Database.PostgreSQL.Simple.ToRow as PGToRow
import qualified Database.PostgreSQL.Simple.Types as PGTypes
import Utils.PasswordEncryption (encryptPassword)
import Schema

instance PGToRow.ToRow UUID where
  toRow uuid = [PGToField.toField uuid]

instance PGToField.ToField Char where
  toField = PGToField.toField . T.pack . (:[])

deleteFollow :: PGSimple.Connection -> UUID -> UUID -> IO ()
deleteFollow conn userFollowedId userFollowerId = do
  let queryString =
        "DELETE FROM users WHERE user_followed_id = ? AND user_follower_id = ?"

  void $ PGSimple.execute conn queryString (userFollowedId, userFollowerId)


insertFollow :: PGSimple.Connection -> Schema.FollowInsert -> IO ()
insertFollow conn (Schema.FollowInsert insertUserFollowedId insertUserFollowerId) = do
  let queryString =
        "INSERT INTO users (id, user_followed_id, user_follower_id, created_at, updated_at) VALUES (?, ?, ?, ?, ?)"

  createdAt <- getCurrentTime
  updatedAt <- getCurrentTime
  followId <- nextRandom
  void $ PGSimple.execute conn queryString (followId, insertUserFollowedId, insertUserFollowerId, createdAt, updatedAt)


getFollowing :: PGSimple.Connection -> UUID -> IO [Schema.User]
getFollowing conn userId = PGSimple.query conn "SELECT user.id, user.username, user.first_name, user.last_name, user.email, user.password, user.created_at, user.updated_at FROM follow JOIN user ON user.id = follow.user_followed_id WHERE user_follower_id = ?" (userId)

getFollowers :: PGSimple.Connection -> UUID -> IO [Schema.User]
getFollowers conn userId = PGSimple.query conn "SELECT user.id, user.username, user.first_name, user.last_name, user.email, user.password, user.created_at, user.updated_at FROM follow JOIN user ON user.id = follow.user_follower_id WHERE user_followed_id = ?" (userId)
