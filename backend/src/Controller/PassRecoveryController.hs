{-# LANGUAGE OverloadedStrings #-}
module Controller.PassRecoveryController where

import Servant
import Schema (UserPassRecovery)
import qualified Service.PassRecoveryService as PassRecoveryService
import Data.UUID (UUID, fromString)
import Control.Monad.IO.Class (liftIO)
import Database.PostgreSQL.Simple (Connection)

requestPassRecoveryHandler :: String -> UUID -> Handler (String)
requestPassRecoveryHandler email userId = liftIO $ PassRecoveryService.requestPassRecovery email userId

resetPasswordHandler :: Connection -> UserPassRecovery -> Handler ()
resetPasswordHandler conn user = liftIO (PassRecoveryService.resetPassword conn user)