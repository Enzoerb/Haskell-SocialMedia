{-# LANGUAGE OverloadedStrings #-}
module Controller.PostController where

import Servant
import qualified Service.PostService as PostService
import Data.UUID (UUID, fromString)
import Schema (Post, PostInsert, PostUpdate)
import Control.Monad.IO.Class (liftIO)
import Database.PostgreSQL.Simple (Connection)

getPostByIdHandler :: Connection -> UUID -> Handler (Maybe Schema.Post)
getPostByIdHandler conn postId = liftIO $ PostService.getPostById conn postId

getPostsByUserIdHandler :: Connection -> UUID -> Handler [Schema.Post]
getPostsByUserIdHandler conn userId = liftIO $ PostService.getPostByUserId conn userId

getPostsByFollowsHandler :: Connection -> UUID -> Handler [Schema.Post]
getPostsByFollowsHandler conn userId = liftIO $ PostService.getPostByFollows conn userId

getAllPostsHandler :: Connection -> Handler [Schema.Post]
getAllPostsHandler conn = liftIO (PostService.getAllPosts conn)

insertPostHandler :: Connection -> PostInsert -> Handler ()
insertPostHandler conn user = liftIO (PostService.insertPost conn user)

updatePostHandler :: Connection -> PostUpdate -> Handler ()
updatePostHandler conn user = liftIO (PostService.updatePost conn user)

deletePostHandler :: Connection -> UUID -> Handler ()
deletePostHandler conn id = liftIO (PostService.deletePost conn id)
