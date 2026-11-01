-- Server/Launcher.hs
-- Khởi chạy máy chủ dùng chung cho tiến trình độc lập và máy chủ nhúng trong client

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.Launcher
  ( defaultTcpPort
  , defaultUdpPort
  , tryOpenServerSocket
  , serveForever
  , startEmbeddedServer
  ) where

import Protocol (decodeCommand, encodeClientId)
import Server.State (initServerState, initCommandQueue, enqueueCommand)
import Server.Network (initClientMap, openServerSocket, receiveLinesLoop, runUdpDiscoveryLoop)
import Server.Tick (runTickLoop)
import Server.Supervisor (superviseClient)
import Network.Socket (Socket, accept, close)
import Network.Socket.ByteString (sendAll)
import Control.Concurrent (forkIO, newMVar, modifyMVar)
import Control.Exception (SomeException, IOException, catch, finally)
import Control.Monad (forever)

defaultTcpPort :: String
defaultTcpPort = "9123"

defaultUdpPort :: String
defaultUdpPort = "9124"

-- Thử mở cổng TCP máy chủ; trả về Nothing nếu cổng đã được tiến trình khác sử dụng
tryOpenServerSocket :: String -> IO (Maybe Socket)
tryOpenServerSocket !port =
  (Just <$> openServerSocket "0.0.0.0" port)
    `catch` (\(_ :: IOException) -> return Nothing)

-- Khởi chạy luồng nhịp 30 FPS, luồng phản hồi UDP LAN và vòng lặp chấp nhận kết nối TCP
serveForever :: Socket -> String -> IO ()
serveForever !serverSock !udpPort = do
  stVar     <- initServerState
  cmdQueue  <- initCommandQueue
  clientMap <- initClientMap
  idCounter <- newMVar (1 :: Int)

  _ <- forkIO (runTickLoop stVar cmdQueue clientMap)
  _ <- forkIO (runUdpDiscoveryLoop stVar udpPort)

  forever $ do
    (clientSock, clientAddr) <- accept serverSock
    assignedId <- modifyMVar idCounter (\curId -> return (curId + 1, curId))
    putStrLn $ "[SERVER] Nguoi choi " ++ show assignedId ++ " ket noi tu " ++ show clientAddr

    sendAll clientSock (encodeClientId assignedId)
      `catch` (\(_ :: SomeException) -> return ())

    _ <- forkIO $ superviseClient assignedId clientSock clientAddr stVar clientMap $
      receiveLinesLoop clientSock $ \lineBs ->
        case decodeCommand lineBs of
          Right cmd -> enqueueCommand cmdQueue assignedId cmd
          Left err  -> putStrLn $ "[SERVER] Goi tin khong hop le tu nguoi choi " ++ show assignedId ++ ": " ++ err
    return ()

startEmbeddedServer :: IO Bool
startEmbeddedServer = do
  mSock <- tryOpenServerSocket defaultTcpPort
  case mSock of
    Nothing -> return False
    Just sock -> do
      putStrLn $ "Server khoi tao thanh cong tai cong " ++ defaultTcpPort ++ "..."
      _ <- forkIO $
        (serveForever sock defaultUdpPort
          `catch` (\(e :: SomeException) -> putStrLn ("[SERVER] May chu dung hoat dong: " ++ show e)))
          `finally` close sock
      return True
