-- Server/Network.hs
-- Truyền thông mạng TCP và phản hồi dò tìm phòng LAN qua UDP

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.Network
  ( ClientMap
  , initClientMap
  , openServerSocket
  , receiveLinesLoop
  , broadcastGameState
  , runUdpDiscoveryLoop
  , getLocalLanIp
  , extractIpString
  ) where

import Types
import Protocol (encodeGameState, encodeLanReply, lanDiscoverMessage)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll, recvFrom, sendTo)
import Control.Concurrent (MVar, newMVar, readMVar)
import Control.Concurrent.STM (TVar, readTVarIO)
import Control.Exception (SomeException, IOException, catch, try)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as Map
import Control.Monad (unless, when, forever)
import Data.List (isInfixOf, intercalate)
import System.Info (os)

type ClientMap = MVar (Map.Map Int Socket)

initClientMap :: IO ClientMap
initClientMap = newMVar Map.empty

-- Không bật ReuseAddr trên Windows để phát hiện khi cổng TCP 9123 đã có máy chủ khác chiếm giữ
openServerSocket :: HostName -> ServiceName -> IO Socket
openServerSocket host port = do
  let hints = defaultHints
        { addrFlags      = [AI_PASSIVE]
        , addrSocketType = Stream
        , addrFamily     = AF_INET
        }
  addrs <- getAddrInfo (Just hints) (Just host) (Just port)
  case addrs of
    [] -> ioError (userError $ "Khong the tim thay dia chi hop le cho " ++ host ++ ":" ++ port)
    (serverAddr:_) -> do
      sock <- socket (addrFamily serverAddr) (addrSocketType serverAddr) (addrProtocol serverAddr)
      when (os /= "mingw32") (setSocketOption sock ReuseAddr 1)
      bindRes <- try (bind sock (addrAddress serverAddr) >> listen sock 10)
      case bindRes of
        Left (e :: IOException) -> close sock >> ioError e
        Right () -> return sock

-- Đọc luồng byte TCP, ghép phần dư và tách từng dòng kết thúc bằng '\n'
receiveLinesLoop :: Socket -> (BS.ByteString -> IO ()) -> IO ()
receiveLinesLoop sock onLine = loop BS.empty
  where
    safeRecv = recv sock 2048 `catch` (\(_ :: IOException) -> return BS.empty)
    loop !buffer = do
      chunk <- safeRecv
      if BS.null chunk
        then return ()
        else do
          let !combined            = BS.append buffer chunk
              (!done, !remainder)  = BS.breakEnd (== '\n') combined
              completeLines        = BS.split '\n' done
          mapM_ (\line -> do
            let !trimmed = BS.filter (\c -> c /= '\r') line
            unless (BS.null trimmed) (onLine trimmed)
            ) completeLines
          loop remainder

broadcastGameState :: ClientMap -> GameState -> IO ()
broadcastGameState clientMapVar !st = do
  clients <- readMVar clientMapVar
  let !payload = encodeGameState st
  mapM_ (\(!_cId, !sock) ->
    catch (sendAll sock payload)
          (\(_ex :: SomeException) -> return ()
          )
    ) (Map.toList clients)

-- Lắng nghe gói dò tìm trên cổng UDP 9124 và phản hồi IP LAN cùng mã phòng hiện tại
runUdpDiscoveryLoop :: TVar GameState -> ServiceName -> IO ()
runUdpDiscoveryLoop stVar port = catch (do
  let hints = defaultHints
        { addrFlags      = [AI_PASSIVE]
        , addrSocketType = Datagram
        , addrFamily     = AF_INET
        }
  addrs <- getAddrInfo (Just hints) Nothing (Just port)
  case addrs of
    [] -> putStrLn $ "[LAN] Khong the lay dia chi cho cong UDP " ++ port
    (udpAddr:_) -> do
      sock <- socket (addrFamily udpAddr) Datagram defaultProtocol
      setSocketOption sock ReuseAddr 1
      setSocketOption sock Broadcast 1
      bind sock (addrAddress udpAddr)
      putStrLn $ "[LAN] San sang phan hoi tim kiem phong tai cong UDP " ++ port
      forever $ catch (do
        (msg, senderAddr) <- recvFrom sock 1024
        let msgStr = BS.unpack msg
        if lanDiscoverMessage `isInfixOf` msgStr
          then do
            st <- readTVarIO stVar
            lanIp <- getLocalLanIp
            let code  = gsRoomCode st
                reply = BS.pack (encodeLanReply lanIp (if null code then Nothing else Just code))
            _ <- sendTo sock reply senderAddr
            putStrLn $ "[LAN] Tra loi tim kiem phong cho " ++ show senderAddr
            return ()
          else return ()
        ) (\(_ :: SomeException) -> return ())
  ) (\(e :: SomeException) -> putStrLn $ "[LAN] Khong mo duoc cong UDP " ++ port ++ ": " ++ show e)

-- Xác định địa chỉ IPv4 LAN của máy qua bảng định tuyến UDP nội bộ
getLocalLanIp :: IO String
getLocalLanIp = tryTargets ["8.8.8.8", "192.168.1.1", "192.168.0.1", "192.168.137.1", "10.0.0.1"]
  where
    tryTargets [] = return "127.0.0.1"
    tryTargets (t : ts) = do
      ip <- probeTarget t
      if ip /= "127.0.0.1" && ip /= "0.0.0.0"
        then return ip
        else tryTargets ts
    probeTarget target = (do
      let hints = defaultHints { addrSocketType = Datagram, addrFamily = AF_INET }
      addrs <- getAddrInfo (Just hints) (Just target) (Just "80")
      case addrs of
        [] -> return "127.0.0.1"
        (destAddr:_) -> do
          sock <- socket (addrFamily destAddr) Datagram defaultProtocol
          connect sock (addrAddress destAddr)
          sockAddr <- getSocketName sock
          close sock
          case sockAddr of
            SockAddrInet _ hostAddr -> return (extractIpString hostAddr)
            _ -> return "127.0.0.1"
      ) `catch` (\(_ :: SomeException) -> return "127.0.0.1")

extractIpString :: HostAddress -> String
extractIpString ha =
  let (b1, b2, b3, b4) = hostAddressToTuple ha
  in intercalate "." [show b1, show b2, show b3, show b4]
