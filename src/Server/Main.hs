-- Server/Main.hs
-- Điểm khởi chạy tiến trình máy chủ độc lập tank-server

{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import GHC.Conc (setNumCapabilities, getNumProcessors)

import Server.Launcher (defaultTcpPort, defaultUdpPort, tryOpenServerSocket, serveForever)
import Network.Socket (close)
import Control.Exception (catch, finally, SomeException)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Process (spawnCommand)
import System.IO (hSetEncoding, hSetBuffering, stdout, stderr, utf8, BufferMode(LineBuffering))

autoConfigureFirewall :: IO ()
autoConfigureFirewall = catch (do
  let cmd = "netsh advfirewall firewall add rule name=\"TankBattleTCP\" dir=in action=allow protocol=TCP localport=9123 >nul 2>&1 & "
         ++ "netsh advfirewall firewall add rule name=\"TankBattleUDP\" dir=in action=allow protocol=UDP localport=9124 >nul 2>&1"
  _ <- spawnCommand cmd
  return ()
  ) (\(_ :: SomeException) -> return ())

main :: IO ()
main = do
  cores <- getNumProcessors
  setNumCapabilities cores
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  putStrLn $ "So capability: " ++ show cores
  autoConfigureFirewall
  args <- getArgs
  let port = case args of
        (p:_) -> p
        []    -> defaultTcpPort

  mSock <- tryOpenServerSocket port
  case mSock of
    Nothing -> do
      putStrLn $ "Khong mo duoc cong " ++ port ++ ": tren may da co mot may chu Tank Battle dang chay."
      exitFailure
    Just serverSock -> do
      putStrLn $ "Server khoi tao thanh cong tai cong " ++ port ++ "..."
      serveForever serverSock defaultUdpPort `finally` close serverSock
