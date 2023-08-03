{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Database.PostgreSQL.Simple as PGSimple
import qualified Migrations
import qualified Schema
import Data.UUID.V4 (nextRandom)
import Data.UUID (toString)
import Data.Time (getCurrentTime)
import Control.Monad (void)
import qualified Database.PostgreSQL.Simple.ToRow as PGToRow
import qualified Database.PostgreSQL.Simple.ToField as PGToField

instance PGToRow.ToRow Schema.User where
  toRow user =
    [ PGToField.toField (Schema.userUserId user)
    , PGToField.toField (Schema.username user)
    , PGToField.toField (Schema.firstName user)
    , PGToField.toField (Schema.lastName user)
    , PGToField.toField (Schema.email user)
    , PGToField.toField (Schema.password user)
    , PGToField.toField (Schema.userCreatedAt user)
    , PGToField.toField (Schema.userUpdatedAt user)
    ]

-- Function to insert a user into the database
insertUser :: Schema.User -> IO ()
insertUser user = do
  -- Establish a connection to the PostgreSQL database
  conn <- PGSimple.connect PGSimple.defaultConnectInfo
    { PGSimple.connectHost = "localhost"
    , PGSimple.connectPort = 5432
    , PGSimple.connectUser = "postgres"
    , PGSimple.connectPassword = "mysecretpassword"
    , PGSimple.connectDatabase = "postgres"
    }

  -- Define the SQL query to insert a user
  let queryString =
        "INSERT INTO users (id, username, first_name, last_name, email, password, created_at, updated_at) VALUES (?, ?, ?, ?, ?, ?, ?, ?)"

  -- Execute the query with the user data
  _ <- PGSimple.execute conn queryString user

  -- Close the connection when done
  PGSimple.close conn

main :: IO ()
main = do
  -- Connect to the PostgreSQL database
  conn <- PGSimple.connect PGSimple.defaultConnectInfo
    { PGSimple.connectHost = "localhost"
    , PGSimple.connectPort = 5432
    , PGSimple.connectUser = "postgres"
    , PGSimple.connectPassword = "mysecretpassword"
    , PGSimple.connectDatabase = "postgres"
    }

  -- Create tables if they don't exist
  Migrations.createTables conn

  -- Generate UUIDs and insert data
  uuid1 <- nextRandom
  currentTime <- getCurrentTime
  let user1 = Schema.User
        { Schema.userUserId = uuid1
        , Schema.username = "user1"
        , Schema.firstName = "John"
        , Schema.lastName = "Doe"
        , Schema.email = "john.doe@example.com"
        , Schema.password = "pass123"
        , Schema.userCreatedAt = currentTime
        , Schema.userUpdatedAt = currentTime
        }

  -- Insert more data as needed
  insertUser user1

  -- Close the connection
  PGSimple.close conn
