{-# LANGUAGE OverloadedStrings #-}
module Controller.UserController where

import Servant
import qualified Service.UserService as UserService
import Schema (User)
import Control.Monad.IO.Class (liftIO)
import Database.PostgreSQL.Simple (Connection)

getUserByUsernameHandler :: Connection -> String -> Handler (Maybe User)
getUserByUsernameHandler conn username = liftIO $ UserService.getUserByUsername conn username

getAllUsersHandler :: Connection -> Handler [User]
getAllUsersHandler conn = liftIO (UserService.getAllUsers conn)
