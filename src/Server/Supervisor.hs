-- Server/Supervisor.hs
-- Giám sát luồng phục vụ từng Client bằng withAsync, waitCatch và finally

{-# LANGUAGE ScopedTypeVariables #-}

module Server.Supervisor
  ( superviseClient
  ) where

import Server.State (ServerState, removePlayer)
import Server.Network (ClientMap)
import Network.Socket (Socket, SockAddr, close)
import Control.Concurrent.Async (waitCatch, withAsync)
import Control.Concurrent (modifyMVar_)
import Control.Exception (SomeException, catch, finally)
import qualified Data.Map.Strict as Map

-- Đăng ký socket khi kết nối, giám sát ngoại lệ trong lúc chơi và dọn dẹp tài nguyên khi ngắt kết nối
superviseClient
  :: Int
  -> Socket
  -> SockAddr
  -> ServerState
  -> ClientMap
  -> IO ()
  -> IO ()
superviseClient !cId !sock !_addr !stVar !clientMapVar !clientAction = do
  modifyMVar_ clientMapVar (\m -> return (Map.insert cId sock m))

  let monitoredAction = do
        res <- withAsync clientAction waitCatch
        case res of
          Left (ex :: SomeException) ->
            putStrLn $ "[SERVER] Luong phuc vu nguoi choi " ++ show cId ++ " gap loi: " ++ show ex
          Right () ->
            putStrLn $ "[SERVER] Nguoi choi " ++ show cId ++ " da ngat ket noi."

  monitoredAction `finally` cleanupClient
  where
    cleanupClient = do
      modifyMVar_ clientMapVar (\m -> return (Map.delete cId m))
      removePlayer stVar cId
      catch (close sock) (\(_ :: SomeException) -> return ())
