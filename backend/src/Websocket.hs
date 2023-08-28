{-# LANGUAGE OverloadedStrings #-}

module Websocket where

import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets
import Network.WebSockets
import qualified Data.Text as Text
import Control.Concurrent.MVar
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (forever, forM_, when)
import Network.HTTP.Types.Status (status400)
import Data.Aeson (ToJSON, toJSON, object, (.=), encode)
import Data.Time.Clock (getCurrentTime, UTCTime)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B


type ClientId = String
type Conn = (ClientId, Connection)
type ClientList = MVar [Conn]

data ChatMessage = ChatMessage
    { clientId  :: ClientId
    , message   :: String
    , datetime  :: UTCTime
    } deriving (Show)

instance ToJSON ChatMessage where
    toJSON (ChatMessage cId msg dt) =
        object ["clientId" .= cId, "message" .= msg, "datetime" .= dt]

runServer :: IO ()
runServer = do
    clients <- newMVar []
    run 9160 (app clients)

pingThread :: Connection -> Int -> IO ()
pingThread conn seconds = forever $ do
    threadDelay (seconds * 1000000)  -- microseconds
    sendPing conn ("keepalive" :: Text.Text)

app :: ClientList -> Application
app clients = websocketsOr defaultConnectionOptions (wsApp clients) backupApp

wsApp :: ClientList -> ServerApp
wsApp clients pendingConn = do
    let pathText = (requestPath $ pendingRequest pendingConn)
        clientId = B.unpack $ B.tail pathText

    conn <- acceptRequest pendingConn
    -- ping thread, a cada 20s
    _ <- forkIO $ pingThread conn 20

    modifyMVar_ clients (\cs -> return ((clientId, conn):cs))
    putStrLn $ "Cliente " ++ show clientId ++ " conectado"
    
    forever $ do
        msg <- receiveData conn
        broadcast clients clientId (Text.unpack msg)

    --  "Limpar ao desconectar cliente"
    modifyMVar_ clients (\cs -> return (filter ((/= clientId) . fst) cs))
    putStrLn $ "Cliente " ++ show clientId ++ " disconectado"


backupApp :: Application
backupApp _ respond = respond $ responseLBS status400 [] "Não é uma conexão websocket"

broadcast :: ClientList -> ClientId -> String -> IO ()
broadcast clients srcClientId msg = 
    withMVar clients $ \cs -> do
        dt <- getCurrentTime
        let chatMsg = ChatMessage srcClientId msg dt
            jsonMsg = BL.toStrict $ encode chatMsg
        forM_ cs $ \(clientId, conn) ->
            when (clientId /= srcClientId) $ sendTextData conn jsonMsg

