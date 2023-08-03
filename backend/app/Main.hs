{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Database.PostgreSQL.Simple as PGSimple
import qualified Migrations

main :: IO ()
main = do
  -- Connect to the PostgreSQL database
  conn <- PGSimple.connect PGSimple.defaultConnectInfo
    { PGSimple.connectHost = "postgres"
    , PGSimple.connectPort = 5432
    , PGSimple.connectUser = "postgres"
    , PGSimple.connectPassword = "mysecretpassword"
    , PGSimple.connectDatabase = "postgres"
    }

  -- Create tables if they don't exist
  Migrations.createTables conn

  -- Close the connection
  PGSimple.close conn
