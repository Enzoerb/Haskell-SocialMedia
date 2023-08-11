{-# LANGUAGE OverloadedStrings #-}

module Service.PostService where

import Data.UUID (UUID)
import qualified Repository.PostRepository as PostRepo
import Database.PostgreSQL.Simple (Connection)
import Schema

insertPost :: Connection -> Schema.PostInsert -> IO ()
insertPost conn postToInsert = PostRepo.insertPost conn postToInsert

updatePost :: Connection -> Schema.PostUpdate -> IO ()
updatePost conn updatedPost = PostRepo.updatePost conn updatedPost

deletePost :: Connection -> UUID -> IO ()
deletePost conn postId = PostRepo.deletePost conn postId

getPostById :: Connection -> UUID -> IO (Maybe Schema.Post)
getPostById conn postId = PostRepo.getPostById conn postId

getPostByUserId :: Connection -> UUID -> IO [Schema.Post]
getPostByUserId conn userId = PostRepo.getPostByUserId conn userId

getPostByFollows :: Connection -> UUID -> IO [Schema.Post]
getPostByFollows conn userId = PostRepo.getPostByFollows conn userId

getAllPosts :: Connection -> IO [Schema.Post]
getAllPosts conn = PostRepo.getAllPosts conn
