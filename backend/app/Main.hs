{-# LANGUAGE OverloadedStrings #-}
module Main where

import Servant
import Servant.API
import Data.Text (Text)
import Data.Aeson (ToJSON, FromJSON)
import Network.Wai.Handler.Warp (run)
import API (api)
import qualified Database.PostgreSQL.Simple as PGSimple
import qualified Controller.UserController as UserController
import qualified Controller.PostController as PostController
import qualified Controller.FollowController as FollowController
import qualified Migrations
import System.IO (hFlush, stdout)

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

  -- Run the Servant server
  putStrLn "Running server on port 8080..."
  hFlush stdout  -- Flush the buffer to ensure immediate display
  run 8080 (serve API.api (UserController.getAllUsersHandler conn
                           :<|> UserController.getUserByUsernameHandler conn
                           :<|> UserController.getUserByEmailHandler conn
                           :<|> UserController.getUserByIdHandler conn
                           :<|> UserController.insertUserHandler conn
                           :<|> UserController.updateUserHandler conn
                           :<|> UserController.deleteUserHandler conn
                           :<|> PostController.getAllPostsHandler conn
                           :<|> PostController.getPostsByUserIdHandler conn
                           :<|> PostController.getPostsByFollowsHandler conn
                           :<|> PostController.getPostByIdHandler conn
                           :<|> PostController.insertPostHandler conn
                           :<|> PostController.updatePostHandler conn
                           :<|> PostController.deletePostHandler conn
                           :<|> FollowController.getFollowingHandler conn
                           :<|> FollowController.getFollowerHandler conn
                           :<|> FollowController.insertFollowHandler conn
                           :<|> FollowController.deleteFollowHandler conn
                          ))

  -- Close the connection
  PGSimple.close conn
