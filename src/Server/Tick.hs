-- Server/Tick.hs
-- Vòng lặp nhịp 30 FPS cập nhật trạng thái thế giới trò chơi trên máy chủ

{-# LANGUAGE BangPatterns #-}

module Server.Tick
  ( runTickLoop
  ) where

import GameLogic (stepGameTick, updateGameState)
import Server.State (ServerState, CommandQueue, drainCommands)
import Server.Network (ClientMap, broadcastGameState)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically, modifyTVar', readTVar)
import Control.Monad (when)
import GHC.Clock (getMonotonicTime)

tickSeconds :: Double
tickSeconds = 1.0 / 30.0

-- Hẹn giờ theo đồng hồ đơn điệu (getMonotonicTime) để bù độ trễ làm tròn của threadDelay trên Windows
runTickLoop :: ServerState -> CommandQueue -> ClientMap -> IO ()
runTickLoop stVar cmdQueue clientMap = do
  start <- getMonotonicTime
  loop (start + tickSeconds)
  where
    loop !deadline = do
      now <- getMonotonicTime
      let !waitUs = floor ((deadline - now) * 1000000) :: Int
      when (waitUs > 0) (threadDelay waitUs)

      !latestState <- atomically $ do
        queuedCmds <- drainCommands cmdQueue
        modifyTVar' stVar (\(!s0) ->
          let !s1 = foldl' (\(!acc) (!pid, !cmd) -> updateGameState pid cmd acc) s0 queuedCmds
              !s2 = stepGameTick s1
          in s2
          )
        readTVar stVar

      broadcastGameState clientMap latestState

      after <- getMonotonicTime
      let !next = if after - deadline > 0.25 then after + tickSeconds else deadline + tickSeconds
      loop next
