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
import Schema

data UserUpdate = UserUpdate
  { userId :: UUID
  , newUsername :: String
  , newFirstName :: String
  , newLastName :: String
  , newEmail :: String
  , newPassword :: String
  }

instance Show Schema.User where
  show user = "User { userUserId = " ++ show (Schema.userUserId user)
              ++ ", username = " ++ show (Schema.username user)
              ++ ", firstName = " ++ show (Schema.firstName user)
              ++ ", lastName = " ++ show (Schema.lastName user)
              ++ ", email = " ++ show (Schema.email user)
              ++ ", password = " ++ show (Schema.password user)
              ++ ", userCreatedAt = " ++ show (Schema.userCreatedAt user)
              ++ ", userUpdatedAt = " ++ show (Schema.userUpdatedAt user)
              ++ " }"

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

instance PGToRow.ToRow Schema.User where
  toRow user =
    [ PGToField.toField (Schema.userUserId user)
    , PGToField.toField (Schema.username user)
    , PGToField.toField (Schema.firstName user)
    , PGToField.toField (Schema.lastName user)
    , PGToField.toField (Schema.email user)
    , PGToField.toField (Schema.password user)
    , PGToField.toField (formatUTCTime $ Schema.userCreatedAt user)
    , PGToField.toField (formatUTCTime $ Schema.userUpdatedAt user)
    ]

instance PGToRow.ToRow UUID where
  toRow uuid = [PGToField.toField uuid]

instance PGToField.ToField Char where
  toField = PGToField.toField . T.pack . (:[])

parseUTCTime :: String -> UTCTime
parseUTCTime input = case TimeFormat.parseTimeM True TimeFormat.defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q" input of
  Just time -> time
  Nothing -> error "Failed to parse UTCTime"


formatUTCTime :: UTCTime -> String
formatUTCTime = formatTime TimeFormat.defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q"


updateUser :: PGSimple.Connection -> UserUpdate -> IO ()
updateUser conn (UserUpdate userId newUsername newFirstName newLastName newEmail newPassword) = do
    let queryString =
            "UPDATE users SET username = ?, first_name = ?, last_name = ?, email = ?, password = ? WHERE id = ?"

    void $ PGSimple.execute conn queryString (newUsername, newFirstName, newLastName, newEmail, newPassword, userId)

deleteUser :: PGSimple.Connection -> UUID -> IO ()
deleteUser conn userId = do
  let queryString =
        "DELETE FROM users WHERE id = ?"

  void $ PGSimple.execute conn queryString userId


insertUser :: PGSimple.Connection -> Schema.User -> IO ()
insertUser conn user = do
  let queryString =
        "INSERT INTO users (id, username, first_name, last_name, email, password, created_at, updated_at) VALUES (?, ?, ?, ?, ?, ?, ?, ?)"

  void $ PGSimple.execute conn queryString user


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


main :: IO ()
main = do
  conn <- PGSimple.connect PGSimple.defaultConnectInfo
      { PGSimple.connectHost = "postgres"
      , PGSimple.connectPort = 5432
      , PGSimple.connectUser = "postgres"
      , PGSimple.connectPassword = "mysecretpassword"
      , PGSimple.connectDatabase = "postgres"
      }

  -- deleteFromEmail
  let targetUserEmail = "john.doe3@example.com"
  maybeUserByEmail <- getUsersByEmail conn targetUserEmail
  case maybeUserByEmail of
    Just user -> do
      -- Update the user's last name to "Silva"
      deleteUser conn (userUserId user)
    Nothing -> putStrLn "User not found"

  -- Generate UUIDs and insert data
  uuid1 <- nextRandom
  currentTime <- getCurrentTime
  let user1 = Schema.User
          { Schema.userUserId = uuid1
          , Schema.username = "user3"
          , Schema.firstName = "John"
          , Schema.lastName = "Doe"
          , Schema.email = "john.doe3@example.com"
          , Schema.password = "pass123"
          , Schema.userCreatedAt = currentTime
          , Schema.userUpdatedAt = currentTime
          }

  -- Insert more data as needed
  insertUser conn user1

  -- Fetch and print all users
  allUsers <- getAllUsers conn
  putStrLn "All Users:"
  mapM_ print allUsers

  -- Fetch and print user by ID
  let targetUserId = fromJust $ UUID.fromString "f1c8e742-07e4-4d13-94ef-460c2886f937"
  maybeUser <- getUsersById conn targetUserId
  putStrLn "User by ID:"
  case maybeUser of
    Just user -> print user
    Nothing -> putStrLn "User not found"

  -- updateLastName
  let targetUserEmail = "john.doe@example.com"
  maybeUserByEmail <- getUsersByEmail conn targetUserEmail
  case maybeUserByEmail of
    Just user -> do
      -- Update the user's last name to "Silva"
      let updatedUser = user { lastName = "Silva" }
      updateUser conn (UserUpdate (userUserId user) (username user) (firstName user) (lastName updatedUser) (email user) (password user))
    Nothing -> putStrLn "User not found"

  -- By Email
  let targetUserEmail = "john.doe@example.com"
  maybeUser <- getUsersByEmail conn targetUserEmail
  putStrLn "User by Email:"
  case maybeUser of
    Just user -> print user
    Nothing -> putStrLn "User not found"

   -- By Email
  let targetUserUsername = "user2"
  maybeUser <- getUsersByUsername conn targetUserUsername
  putStrLn "User by Email:"
  case maybeUser of
    Just user -> print user
    Nothing -> putStrLn "User not found"

  PGSimple.close conn
