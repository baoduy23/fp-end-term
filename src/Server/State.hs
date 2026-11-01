-- Server/State.hs
-- Bộ nhớ giao dịch STM (TVar GameState) và hàng đợi lệnh TChan trên máy chủ

{-# LANGUAGE BangPatterns #-}

module Server.State
  ( ServerState
  , CommandQueue
  , initServerState
  , initCommandQueue
  , applyClientCommand
  , addPlayer
  , addPlayerWithClass
  , removePlayer
  , enqueueCommand
  , drainCommands
  , applyBatchCommands
  ) where

import Types
import GameLogic (updateGameState, spawnTank, changeMap)
import Control.Concurrent.STM
import qualified Data.Map.Strict as Map
import System.Random (randomRIO)

type ServerState = TVar GameState

type CommandQueue = TChan (Int, Command)

initServerState :: IO ServerState
initServerState = do
  idx  <- randomRIO (0, mapCount - 1)
  seed <- randomRIO (1, 1000000)
  let !startMap = toEnum idx :: MapId
  newTVarIO ((emptyGameState startMap) { worldSeed = seed })

initCommandQueue :: IO CommandQueue
initCommandQueue =
  newTChanIO

enqueueCommand :: CommandQueue -> Int -> Command -> IO ()
enqueueCommand !q !pId !cmd = atomically $
  writeTChan q (pId, cmd)

-- Rút toàn bộ lệnh đang chờ trong TChan để xử lý theo lô trong một giao dịch STM
drainCommands :: CommandQueue -> STM [(Int, Command)]
drainCommands !q = loop []
  where
    loop !acc = do
      empty <- isEmptyTChan q
      if empty
        then return (reverse acc)
        else do
          cmd <- readTChan q
          loop (cmd : acc)

applyBatchCommands :: ServerState -> [(Int, Command)] -> STM ()
applyBatchCommands !stVar !batchCmds =
  modifyTVar' stVar (\(!s0) ->
    foldl' (\(!acc) (!pid, !cmd) -> updateGameState pid cmd acc) s0 batchCmds
  )

applyClientCommand :: ServerState -> Int -> Command -> IO ()
applyClientCommand !stVar !pId !cmd = atomically $
  modifyTVar' stVar (\(!st) -> updateGameState pId cmd st)

addPlayer :: ServerState -> Int -> IO ()
addPlayer !stVar !pId =
  addPlayerWithClass stVar pId VanguardCyan

addPlayerWithClass :: ServerState -> Int -> TankClass -> IO ()
addPlayerWithClass !stVar !pId !cls = atomically $
  modifyTVar' stVar (\(!st) -> spawnTank pId cls st)

-- Xử lý ngắt kết nối tương đương đầu hàng; khi phòng trống hoàn toàn thì đổi bản đồ cho ván kế tiếp
removePlayer :: ServerState -> Int -> IO ()
removePlayer !stVar !pId = do
  k <- randomRIO (1, mapCount - 1)
  (wasInRoom, roomClosed) <- atomically $ do
    st <- readTVar stVar
    let !afterLeave = updateGameState pId Disconnect st
        !humansLeft = any (\t -> not (isBotPlayer (tankId t))) (Map.elems (tanks afterLeave))
        !closed     = gsRoomPhase afterLeave == PhaseNoRoom && not humansLeft
    writeTVar stVar (if closed && gsRoomPhase st /= PhaseNoRoom
                       then changeMap (cycleMap k (currentMap afterLeave)) afterLeave
                       else afterLeave)
    return (Map.member pId (gsLobbyPlayers st), closed && gsRoomPhase st /= PhaseNoRoom)
  if roomClosed
    then putStrLn "[SERVER] Phong da trong, dong phong va doi ban do cho luot choi sau."
    else if wasInRoom
      then putStrLn $ "[SERVER] Nguoi choi " ++ show pId ++ " roi phong (xu ly nhu dau hang)."
      else return ()
