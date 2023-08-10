{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

module Repository.UserRepository where

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

instance PGFromRow.FromRow Schema.User where
  fromRow = Schema.User
    <$> PGFromRow.field  -- userUserId
    <*> PGFromRow.field  -- username
    <*> PGFromRow.field  -- firstName
    <*> PGFromRow.field  -- lastName
    <*> PGFromRow.field  -- email
    <*> PGFromRow.field  -- password
    <*> PGFromRow.field  -- useerCreatedAt
    <*> PGFromRow.field  -- userUpdatedAt

instance PGToRow.ToRow UUID where
  toRow uuid = [PGToField.toField uuid]

instance PGToField.ToField Char where
  toField = PGToField.toField . T.pack . (:[])

updateUser :: PGSimple.Connection -> Schema.UserUpdate -> IO ()
updateUser conn (Schema.UserUpdate oldUserId newUsername newFirstName newLastName newEmail newPassword) = do
    let queryString =
            "UPDATE users SET username = ?, first_name = ?, last_name = ?, email = ?, password = ?, updated_at = ? WHERE id = ?"
    updatedAt <- getCurrentTime
    let encryptedPassowrd = encryptPassword newPassword
    void $ PGSimple.execute conn queryString (newUsername, newFirstName, newLastName, newEmail, encryptedPassowrd, updatedAt, oldUserId)


deleteUser :: PGSimple.Connection -> UUID -> IO ()
deleteUser conn userId = do
  let queryString =
        "DELETE FROM users WHERE id = ?"

  void $ PGSimple.execute conn queryString userId


insertUser :: PGSimple.Connection -> Schema.UserInsert -> IO ()
insertUser conn (Schema.UserInsert insertUsername insertFirstName insertLastName insertEmail insertPassword) = do
  let queryString =
        "INSERT INTO users (id, username, first_name, last_name, email, password, created_at, updated_at) VALUES (?, ?, ?, ?, ?, ?, ?, ?)"

  createdAt <- getCurrentTime
  updatedAt <- getCurrentTime
  userId <- nextRandom
  let encryptedPassword = encryptPassword insertPassword
  void $ PGSimple.execute conn queryString (userId, insertUsername, insertFirstName, insertLastName, insertEmail, encryptedPassword, createdAt, updatedAt)


getUsersById :: PGSimple.Connection -> UUID -> IO (Maybe Schema.User)
getUsersById conn userId = do
  result <- PGSimple.query conn "SELECT * FROM users WHERE id = ?" (PGTypes.Only userId)
  case result of
    [user] -> return $ Just user
    _      -> return Nothing


getUsersByEmail :: PGSimple.Connection -> String -> IO (Maybe Schema.User)
getUsersByEmail conn email = do
  result <- PGSimple.query conn "SELECT * FROM users WHERE email = ?" (PGTypes.Only email)
  case result of
    [user] -> return $ Just user
    _      -> return Nothing


getUsersByUsername :: PGSimple.Connection -> String -> IO (Maybe Schema.User)
getUsersByUsername conn username = do
  result <- PGSimple.query conn "SELECT * FROM users WHERE username = ?" (PGTypes.Only username)
  case result of
    [user] -> return $ Just user
    _      -> return Nothing


getAllUsers :: PGSimple.Connection -> IO [Schema.User]
getAllUsers conn = PGSimple.query_ conn "SELECT * FROM users"
