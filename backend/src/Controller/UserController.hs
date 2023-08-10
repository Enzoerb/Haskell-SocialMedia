{-# LANGUAGE OverloadedStrings #-}
module Controller.UserController where

import Servant
import qualified Service.UserService as UserService
import Data.UUID (UUID, fromString)
import Schema (User, UserInsert, UserUpdate)
import Control.Monad.IO.Class (liftIO)
import Database.PostgreSQL.Simple (Connection)

getUserByUsernameHandler :: Connection -> String -> Handler (Maybe User)
getUserByUsernameHandler conn username = liftIO $ UserService.getUserByUsername conn username

getUserByEmailHandler :: Connection -> String -> Handler (Maybe User)
getUserByEmailHandler conn email = liftIO $ UserService.getUserByEmail conn email

getUserByIdHandler :: Connection -> UUID -> Handler (Maybe User)
getUserByIdHandler conn id = liftIO $ UserService.getUserById conn id

getAllUsersHandler :: Connection -> Handler [User]
getAllUsersHandler conn = liftIO (UserService.getAllUsers conn)

insertUserHandler :: Connection -> UserInsert -> Handler ()
insertUserHandler conn user = liftIO (UserService.insertUser conn user)

updateUserHandler :: Connection -> UserUpdate -> Handler ()
updateUserHandler conn user = liftIO (UserService.updateUser conn user)

deleteUserHandler :: Connection -> UUID -> Handler ()
deleteUserHandler conn id = liftIO (UserService.deleteUser conn id)
