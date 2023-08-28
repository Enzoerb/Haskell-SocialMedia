{-# LANGUAGE OverloadedStrings #-}
module Controller.PassRecoveryController where

import Servant
import qualified Service.PassRecoveryService as PassRecoveryService
import Data.UUID (UUID, fromString)
import Control.Monad.IO.Class (liftIO)

requestPassRecoveryHandler :: String -> UUID -> Handler (String)
requestPassRecoveryHandler email userId = liftIO $ PassRecoveryService.requestPassRecovery email userId
