{-# LANGUAGE OverloadedStrings #-}

module Migrations where

import Database.PostgreSQL.Simple
import Data.Time (getCurrentTime)
import Control.Monad (void) -- Import the void function

-- Function to create tables if they don't exist
createTables :: Connection -> IO ()
createTables conn = do
  void $ execute_ conn "CREATE TABLE IF NOT EXISTS users (id UUID PRIMARY KEY, username VARCHAR NOT NULL, first_name VARCHAR NOT NULL, last_name VARCHAR NOT NULL, email VARCHAR NOT NULL, password VARCHAR NOT NULL, created_at TIMESTAMP NOT NULL, updated_at TIMESTAMP NOT NULL)"
  void $ execute_ conn "CREATE TABLE IF NOT EXISTS follow (id UUID PRIMARY KEY, user_followed_id UUID NOT NULL REFERENCES users (id) ON DELETE CASCADE, user_follower_id UUID NOT NULL REFERENCES users (id) ON DELETE CASCADE, created_at TIMESTAMP NOT NULL, updated_at TIMESTAMP NOT NULL)"
  void $ execute_ conn "CREATE TABLE IF NOT EXISTS post (id UUID PRIMARY KEY, user_id UUID NOT NULL REFERENCES users (id) ON DELETE CASCADE, content TEXT NOT NULL, post_type VARCHAR NOT NULL, created_at TIMESTAMP NOT NULL, updated_at TIMESTAMP NOT NULL)"

-- Function to add timestamps to the user
addTimestamps :: Connection -> IO ()
addTimestamps conn = do
  currentTime <- getCurrentTime
  void $ execute conn "ALTER TABLE users ADD COLUMN IF NOT EXISTS created_at TIMESTAMP NOT NULL DEFAULT ?" [currentTime]
  void $ execute conn "ALTER TABLE users ADD COLUMN IF NOT EXISTS updated_at TIMESTAMP NOT NULL DEFAULT ?" [currentTime]

-- Function to add timestamps to the follow
addFollowTimestamps :: Connection -> IO ()
addFollowTimestamps conn = do
  currentTime <- getCurrentTime
  void $ execute conn "ALTER TABLE follow ADD COLUMN IF NOT EXISTS created_at TIMESTAMP NOT NULL DEFAULT ?" [currentTime]
  void $ execute conn "ALTER TABLE follow ADD COLUMN IF NOT EXISTS updated_at TIMESTAMP NOT NULL DEFAULT ?" [currentTime]

-- Function to add timestamps to the post
addPostTimestamps :: Connection -> IO ()
addPostTimestamps conn = do
  currentTime <- getCurrentTime
  void $ execute conn "ALTER TABLE post ADD COLUMN IF NOT EXISTS created_at TIMESTAMP NOT NULL DEFAULT ?" [currentTime]
  void $ execute conn "ALTER TABLE post ADD COLUMN IF NOT EXISTS updated_at TIMESTAMP NOT NULL DEFAULT ?" [currentTime]
