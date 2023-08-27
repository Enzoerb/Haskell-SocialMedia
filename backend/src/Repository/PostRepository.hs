{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

module Repository.PostRepository where

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
import Data.UUID (UUID)
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

updatePost :: PGSimple.Connection -> Schema.PostUpdate -> IO ()
updatePost conn (Schema.PostUpdate oldPostId newContent newPostType) = do
    let queryString =
            "UPDATE post SET content = ?, post_type = ?, updated_at = ? WHERE id = ?"
    updatedAt <- getCurrentTime
    void $ PGSimple.execute conn queryString (newContent, newPostType, updatedAt, oldPostId)


deletePost :: PGSimple.Connection -> UUID -> IO ()
deletePost conn postId = do
  let queryString =
        "DELETE FROM post WHERE id = ?"

  void $ PGSimple.execute conn queryString postId


insertPost :: PGSimple.Connection -> Schema.PostInsert -> IO ()
insertPost conn (Schema.PostInsert insertPostUserId insertContent insertPostType) = do
  let queryString =
        "INSERT INTO post (id, user_id, content, post_type, created_at, updated_at) VALUES (?, ?, ?, ?, ?, ?)"

  createdAt <- getCurrentTime
  updatedAt <- getCurrentTime
  postId <- nextRandom
  void $ PGSimple.execute conn queryString (postId, insertPostUserId, insertContent, insertPostType, createdAt, updatedAt)


getPostById :: PGSimple.Connection -> UUID -> IO (Maybe Schema.Post)
getPostById conn postId = do
  result <- PGSimple.query conn "SELECT post.*, users.first_name, users.username FROM post JOIN users ON post.user_id = users.id WHERE id = ?" (PGTypes.Only postId)
  case result of
    [user] -> return $ Just user
    _      -> return Nothing


getPostByUserId :: PGSimple.Connection -> UUID -> IO [Schema.Post]
getPostByUserId conn = PGSimple.query conn "SELECT post.*, users.first_name, users.username FROM post JOIN users ON post.user_id = users.id WHERE user_id = ? ORDER BY post.created_at DESC"


getPostByFollows :: PGSimple.Connection -> UUID -> IO [Schema.Post]
getPostByFollows conn = PGSimple.query conn "SELECT post.*, users.first_name, users.username FROM post JOIN users ON post.user_id = users.id WHERE user_id in (SELECT user_followed_id as user_id FROM follow WHERE user_follower_id = ?) ORDER BY post.created_at DESC"


getAllPosts :: PGSimple.Connection -> IO [Schema.Post]
getAllPosts conn = PGSimple.query_ conn "SELECT post.*, users.first_name, users.username FROM post JOIN users ON post.user_id = users.id ORDER BY post.created_at DESC"
