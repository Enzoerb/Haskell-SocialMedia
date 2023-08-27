{-# LANGUAGE OverloadedStrings #-}
module Controller.IdenticonController where

import Servant
import qualified Service.IdenticonService as IdenticonService
import Control.Monad.IO.Class (liftIO)

generateIdenticonHandler :: String -> Handler (String)
generateIdenticonHandler hash = liftIO $ IdenticonService.generateIdenticon hash
