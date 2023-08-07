{-# LANGUAGE OverloadedStrings #-}

module Migrations where

import Database.PostgreSQL.Simple
import Data.Time (getCurrentTime)
import Control.Monad (void) -- Import the void function

-- Function to create tables if they don't exist
createTables :: Connection -> IO ()
createTables conn = do
  void $ execute_ conn "CREATE EXTENSION IF NOT EXISTS \"pgcrypto\"" -- Add this line to enable pgcrypto extension for generating UUIDs
  void $ execute_ conn "CREATE TABLE IF NOT EXISTS users (id UUID DEFAULT gen_random_uuid() PRIMARY KEY, username VARCHAR NOT NULL UNIQUE, first_name VARCHAR NOT NULL, last_name VARCHAR NOT NULL, email VARCHAR NOT NULL UNIQUE, password VARCHAR NOT NULL, created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp, updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp)"
  void $ execute_ conn "CREATE TABLE IF NOT EXISTS follow (id UUID DEFAULT gen_random_uuid() PRIMARY KEY, user_followed_id UUID NOT NULL REFERENCES users (id) ON DELETE CASCADE, user_follower_id UUID NOT NULL REFERENCES users (id) ON DELETE CASCADE, created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp, updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp)"
  void $ execute_ conn "CREATE TABLE IF NOT EXISTS post (id UUID DEFAULT gen_random_uuid() PRIMARY KEY, user_id UUID NOT NULL REFERENCES users (id) ON DELETE CASCADE, content TEXT NOT NULL, post_type VARCHAR NOT NULL, created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp, updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp)"
