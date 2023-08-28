{-# LANGUAGE OverloadedStrings #-}
module Main where

import Servant
import Network.Wai.Middleware.Cors
import Network.Wai.Handler.Warp (run)
import API (api)
import qualified Database.PostgreSQL.Simple as PGSimple
import qualified Controller.UserController as UserController
import qualified Controller.PostController as PostController
import qualified Controller.FollowController as FollowController
import qualified Controller.IdenticonController as IdenticonController
import Control.Concurrent (forkIO)
import Control.Monad (void)
import qualified Migrations
import qualified Websocket as Ws
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

  -- run websocket server
  putStrLn "Running websocket server on port 9160.."
  void $ forkIO Ws.runServer

  let frontCors = simpleCorsResourcePolicy { corsOrigins = Just (["http://localhost:3000"],  True)
                                           , corsMethods = ["DELETE", "GET", "PUT", "POST", "PATCH"]
                                           , corsRequestHeaders = ["Authorization", "Content-Type"] }

  -- Run the Servant server
  putStrLn "Running server on port 8080.."
  hFlush stdout  -- Flush the buffer to ensure immediate display
  run 8080 $ cors (const $ Just frontCors) (serve API.api (
                                UserController.getAllUsersHandler conn
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
                           :<|> FollowController.getFollowersHandler conn
                           :<|> FollowController.insertFollowHandler conn
                           :<|> FollowController.deleteFollowHandler conn
                           :<|> IdenticonController.generateIdenticonHandler
                          ))

  -- Close the connection
  PGSimple.close conn