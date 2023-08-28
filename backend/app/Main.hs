{-# LANGUAGE OverloadedStrings #-}
module Main where

import Servant
import Servant.API
import Data.Text (Text)
import Data.Aeson (ToJSON, FromJSON)
import Network.Wai.Middleware.Cors
import Network.Wai.Handler.Warp (run)
import API (api)
import qualified Websocket as Websocket
import qualified Database.PostgreSQL.Simple as PGSimple
import qualified Controller.UserController as UserController
import qualified Controller.PostController as PostController
import qualified Controller.FollowController as FollowController
import qualified Controller.IdenticonController as IdenticonController
import qualified Migrations
import System.IO (hFlush, stdout)
import qualified Network.WebSockets as WS
import Control.Concurrent (MVar, newMVar, forkIO)

import Control.Monad (void)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (defaultConnectionOptions)

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


  let frontCors = simpleCorsResourcePolicy { corsOrigins = Just (["http://localhost:3000"],  True)
                                           , corsMethods = ["DELETE", "GET", "PUT", "POST", "PATCH"]
                                           , corsRequestHeaders = ["Authorization", "Content-Type"] }


  putStrLn "Running websocket server on port 9160.."
  state <- newMVar Websocket.newServerState
  void $ forkIO $ WS.runServer "localhost" 9160 $ Websocket.application state

    -- Run the Servant server
  putStrLn "Running api server on port 8080.."
  run 8080 $ cors (const $ Just $ frontCors) $ app state conn

  -- Close the connection
  PGSimple.close conn

app :: MVar Websocket.ServerState -> PGSimple.Connection -> Application
app state conn = websocketsOr defaultConnectionOptions (Websocket.application state) $ serve API.api (
  (UserController.getAllUsersHandler conn)
  :<|> (UserController.getUserByUsernameHandler conn)
  :<|> (UserController.getUserByEmailHandler conn)
  :<|> (UserController.getUserByIdHandler conn)
  :<|> (UserController.insertUserHandler conn)
  :<|> (UserController.updateUserHandler conn)
  :<|> (UserController.deleteUserHandler conn)
  :<|> (PostController.getAllPostsHandler conn)
  :<|> (PostController.getPostsByUserIdHandler conn)
  :<|> (PostController.getPostsByFollowsHandler conn)
  :<|> (PostController.getPostByIdHandler conn)
  :<|> (PostController.insertPostHandler conn)
  :<|> (PostController.updatePostHandler conn)
  :<|> (PostController.deletePostHandler conn)
  :<|> (FollowController.getFollowingHandler conn)
  :<|> (FollowController.getFollowersHandler conn)
  :<|> (FollowController.insertFollowHandler conn)
  :<|> (FollowController.deleteFollowHandler conn)
  :<|> (IdenticonController.generateIdenticonHandler)
  :<|> (IdenticonController.generateIdenticonHandler "b20397a3-5f0b-4b4b-8d35-5a7b35a58b2a")
  )
