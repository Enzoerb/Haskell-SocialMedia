{-# LANGUAGE OverloadedStrings #-}

module Service.UserService where

import Data.UUID (UUID)
import qualified Repository.UserRepository as UserRepo
import Database.PostgreSQL.Simple (Connection)
import Schema

insertUser :: Connection -> Schema.User -> IO ()
insertUser conn user = UserRepo.insertUser conn user

upgradeUser :: Connection -> Schema.User -> String -> IO (Maybe Schema.User)
upgradeUser conn newUser username = do
  maybeUserByUsername <- UserRepo.getUsersByUsername conn username
  case maybeUserByUsername of
    Just oldUser -> do
      let _ = UserRepo.updateUser conn (Schema.UserUpdate (userUserId oldUser) username (firstName newUser) (lastName newUser) (email newUser) (password newUser))
      UserRepo.getUsersById conn (userUserId oldUser)
    Nothing -> return Nothing

deleteUser :: Connection -> UUID -> IO ()
deleteUser conn userId = UserRepo.deleteUser conn userId

deleteUserByUsername :: Connection -> String -> IO (String)
deleteUserByUsername conn username = do
  maybeUserByUsername <- UserRepo.getUsersByUsername conn username
  case maybeUserByUsername of
    Just user -> do
      let _ = UserRepo.deleteUser conn (userUserId user)
      return "OK"
    Nothing -> return "User Not Found"

getUserByEmail :: Connection -> String -> IO (Maybe Schema.User)
getUserByEmail conn email = UserRepo.getUsersByEmail conn email

getUserById :: Connection -> UUID -> IO (Maybe Schema.User)
getUserById conn userId = UserRepo.getUsersById conn userId

getUserByUsername :: Connection -> String -> IO (Maybe Schema.User)
getUserByUsername conn username = UserRepo.getUsersByUsername conn username

getAllUsers :: Connection -> IO [Schema.User]
getAllUsers conn = UserRepo.getAllUsers conn
