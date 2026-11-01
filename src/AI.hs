-- AI.hs
-- Trí tuệ nhân tạo cho xe tăng Bot: tìm đường BFS và cây quyết định thuần túy

{-# LANGUAGE BangPatterns #-}

module AI
  ( BotMemory(..)
  , initBotMemory
  , decideBotAction
  , bfsNextDirection
  , hasClearLineOfSight
  , firingSpots
  ) where

import Types
import Hero
import GameLogic (isInsideGrid, isWall, movePos, ultWouldHit)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (minimumBy, find)
import Data.Ord (comparing)

data BotMemory = BotMemory
  { botId          :: !Int
  , tickCount      :: !Int
  , lastTargetId   :: !(Maybe Int)
  , botChosenClass :: !TankClass
  , lastMoveDir    :: !(Maybe Direction)
  } deriving (Eq, Show)

initBotMemory :: Int -> BotMemory
initBotMemory !bId = BotMemory
  { botId          = bId
  , tickCount      = 0
  , lastTargetId   = Nothing
  , botChosenClass = TitanCrimson
  , lastMoveDir    = Nothing
  }

-- Tìm đường ngắn nhất theo chiều rộng (BFS) trên lưới 32x22 với giới hạn 40 bước
bfsNextDirection :: Position -> Position -> [Position] -> Maybe Direction
bfsNextDirection !start !target !walls
  | start == target = Nothing
  | otherwise =
      let !wallSet = Set.fromList walls
          isFree (Pos x y) =
            x >= 0 && x < gridWidth && y >= 0 && y < gridHeight && not (Set.member (Pos x y) wallSet)

          startNeighbors =
            [ (d, movePos d start)
            | d <- [North, East, South, West]
            , isFree (movePos d start)
            ]

          initQueue = [ (nxt, d, 1 :: Int) | (d, nxt) <- startNeighbors ]
          initVisited = Set.insert start (Set.fromList (map snd startNeighbors))
          maxSteps = 40 :: Int

          bfs [] !_ = Nothing
          bfs ((curr, firstDir, !steps) : rest) !visited
            | curr == target = Just firstDir
            | steps >= maxSteps = bfs rest visited
            | otherwise =
                let nxts =
                      [ nxt
                      | d <- [North, East, South, West]
                      , let nxt = movePos d curr
                      , isFree nxt
                      , not (Set.member nxt visited)
                      ]
                    newVisited = foldl' (flip Set.insert) visited nxts
                    newItems   = [ (nxt, firstDir, steps + 1) | nxt <- nxts ]
                in bfs (rest ++ newItems) newVisited
      in case find (\(_, p) -> p == target) startNeighbors of
           Just (d, _) -> Just d
           Nothing     ->
             case bfs initQueue initVisited of
               Just d  -> Just d
               Nothing ->
                 let candidates = map fst startNeighbors
                 in if null candidates
                      then Nothing
                      else Just (minimumBy (comparing (\d -> manhattanDist (movePos d start) target)) candidates)

-- Kiểm tra đường ngắm thẳng hàng giữa hai vị trí không bị tường che khuất
hasClearLineOfSight :: GameState -> Position -> Position -> Maybe Direction
hasClearLineOfSight st from to
  | posX from == posX to && posY to > posY from =
      if clearPathNorth st from to then Just North else Nothing
  | posX from == posX to && posY to < posY from =
      if clearPathSouth st from to then Just South else Nothing
  | posY from == posY to && posX to > posX from =
      if clearPathEast st from to then Just East else Nothing
  | posY from == posY to && posX to < posX from =
      if clearPathWest st from to then Just West else Nothing
  | otherwise = Nothing

isStandable :: GameState -> Int -> Position -> Bool
isStandable !st !selfId !p =
  isInsideGrid p && not (isWall (currentMap st) p)
  && not (any (\t -> tankId t /= selfId && tankRespawnTimer t <= 0 && tankPos t == p) (Map.elems (tanks st)))

-- Các ô cùng hàng hoặc cột với mục tiêu, cách từ 3 đến 6 ô và có đường ngắm thông suốt
firingSpots :: GameState -> Int -> Position -> [Position]
firingSpots !st !selfId !ePos =
  [ p
  | d <- [North, East, South, West]
  , k <- [3 .. 6 :: Int]
  , let p = iterate (movePos d) ePos !! k
  , isStandable st selfId p
  , hasClearLineOfSight st p ePos /= Nothing
  ]

-- Hàm thuần ra quyết định cho Bot ở mỗi nhịp: ưu tiên tuyệt kỹ, bắn thẳng, né đạn, nhặt ngọc hoặc tiếp cận
decideBotAction :: BotMemory -> GameState -> (Maybe Command, BotMemory)
decideBotAction !mem !st =
  let !bId   = botId mem
      !mem'  = mem { tickCount = tickCount mem + 1 }
      !walls = Set.toList (mapWallSet (currentMap st))
  in if gsRoomPhase st == PhaseMatchOver
       then (Nothing, mem')
       else case Map.lookup bId (tanks st) of
         Nothing
           | gsRoomPhase st == PhaseNoRoom -> (Just (SelectClass (botChosenClass mem)), mem')
           | otherwise                     -> (Nothing, mem')
         Just botTank
           | tankRespawnTimer botTank > 0 -> (Nothing, mem')
           | otherwise ->
               let !myPos   = tankPos botTank
                   !enemies = filter (\t -> tankId t /= bId && tankRespawnTimer t <= 0 && tankStealthTimer t <= 0)
                                     (Map.elems (tanks st))
               in if null enemies
                    then let (cmd, m2) = patrolMove st myPos (tankDir botTank) mem' in (Just cmd, m2)
                    else
                      let !target = minimumBy (comparing (manhattanDist myPos . tankPos)) enemies
                      in huntTarget st botTank target walls mem' { lastTargetId = Just (tankId target) }

huntTarget :: GameState -> Tank -> Tank -> [Position] -> BotMemory -> (Maybe Command, BotMemory)
huntTarget !st !me !target !walls !mem =
  let !bId      = tankId me
      !myPos    = tankPos me
      !myDir    = tankDir me
      !ePos     = tankPos target
      !dist     = manhattanDist myPos ePos
      !fullUlt  = tankEnergy me >= maxEnergy && tankSilenceTimer me <= 0 && tankEmpTimer me <= 0
      !mLos     = hasClearLineOfSight st myPos ePos
      !lowHp    = tankHealth me * 100 < getClassMaxHP (tankClass me) * 35
      moveTo d  = (Just (Move d), mem { lastMoveDir = Just d })
      nearest ks = case filter (\o -> orbType o `elem` ks) (powerUps st) of
                     [] -> Nothing
                     os -> Just (minimumBy (comparing (manhattanDist myPos . orbPos)) os)
      antiOscillate d = case lastMoveDir mem of
                          Just prevDir | prevDir == oppositeDir d ->
                            pickNonWallDir st myPos [turnRight prevDir, turnLeft prevDir, d]
                          _ -> d
      stepToward p = case bfsNextDirection myPos p walls of
                       Just d  -> moveTo (antiOscillate d)
                       Nothing -> moveTo (navigateToward st myPos p)
      lineUpDirs = [ d | d <- [North, East, South, West]
                       , let p = movePos d myPos
                       , isStandable st bId p
                       , hasClearLineOfSight st p ePos /= Nothing
                       , manhattanDist p ePos <= 10 ]
  in case () of
       _ | fullUlt && ultWouldHit bId st -> (Just CastUltimate, mem)

         | Just attackDir <- mLos, dist <= 12 ->
             if myDir /= attackDir
               then moveTo attackDir
               else if tankFireCooldown me <= 0 && tankEmpTimer me <= 0
                 then (Just Shoot, mem)
                 else case findThreateningBullet myPos (bullets st) of
                        Just b  -> moveTo (pickDodgeDir st myPos (bulletDir b))
                        Nothing -> (Nothing, mem)

         | Just b <- findThreateningBullet myPos (bullets st) ->
             moveTo (pickDodgeDir st myPos (bulletDir b))

         | lowHp, Just o <- nearest [HealOrb], manhattanDist myPos (orbPos o) <= 6 ->
             stepToward (orbPos o)

         | (d : _) <- lineUpDirs -> moveTo d

         | Just o <- nearest [EnergyOrb, OverchargeOrb], manhattanDist myPos (orbPos o) <= 3 ->
             stepToward (orbPos o)

         | otherwise ->
             case firingSpots st bId ePos of
               []    -> stepToward ePos
               spots -> stepToward (minimumBy (comparing (manhattanDist myPos)) spots)

manhattanDist :: Position -> Position -> Int
manhattanDist (Pos !x1 !y1) (Pos !x2 !y2) = abs (x1 - x2) + abs (y1 - y2)

findThreateningBullet :: Position -> [Bullet] -> Maybe Bullet
findThreateningBullet (Pos mx my) bs =
  find isThreatening bs
  where
    isThreatening b = case bulletDir b of
      North -> posX (bulletPos b) == mx && posY (bulletPos b) < my && (my - posY (bulletPos b) <= 4)
      South -> posX (bulletPos b) == mx && posY (bulletPos b) > my && (posY (bulletPos b) - my <= 4)
      East  -> posY (bulletPos b) == my && posX (bulletPos b) < mx && (mx - posX (bulletPos b) <= 4)
      West  -> posY (bulletPos b) == my && posX (bulletPos b) > mx && (posX (bulletPos b) - mx <= 4)

pickDodgeDir :: GameState -> Position -> Direction -> Direction
pickDodgeDir !m !p !bDir =
  let candidates = case bDir of
        North -> [East, West]
        South -> [West, East]
        East  -> [North, South]
        West  -> [South, North]
  in pickNonWallDir m p candidates

navigateToward :: GameState -> Position -> Position -> Direction
navigateToward !m !pFrom !pTo =
  let !dx = posX pTo - posX pFrom
      !dy = posY pTo - posY pFrom
      preferred =
        if abs dx >= abs dy
          then [ if dx > 0 then East else West
               , if dy > 0 then North else South
               , if dy > 0 then South else North
               , if dx > 0 then West else East
               ]
          else [ if dy > 0 then North else South
               , if dx > 0 then East else West
               , if dx > 0 then West else East
               , if dy > 0 then South else North
               ]
  in pickNonWallDir m pFrom preferred

clearPathNorth :: GameState -> Position -> Position -> Bool
clearPathNorth m (Pos x y1) (Pos _ y2) =
  not (any (\y -> isWall (currentMap m) (Pos x y)) [y1 + 1 .. y2 - 1])

clearPathSouth :: GameState -> Position -> Position -> Bool
clearPathSouth m (Pos x y1) (Pos _ y2) =
  not (any (\y -> isWall (currentMap m) (Pos x y)) [y2 + 1 .. y1 - 1])

clearPathEast :: GameState -> Position -> Position -> Bool
clearPathEast m (Pos x1 y) (Pos x2 _) =
  not (any (\x -> isWall (currentMap m) (Pos x y)) [x1 + 1 .. x2 - 1])

clearPathWest :: GameState -> Position -> Position -> Bool
clearPathWest m (Pos x1 y) (Pos x2 _) =
  not (any (\x -> isWall (currentMap m) (Pos x y)) [x2 + 1 .. x1 - 1])

-- Chọn hướng đi hợp lệ đầu tiên không bị tường chắn và ưu tiên tránh ô dung nham
pickNonWallDir :: GameState -> Position -> [Direction] -> Direction
pickNonWallDir !m !p dirs =
  case filter isSafe dirs of
    (d:_) -> d
    []    -> case filter isPassable dirs of
               (d:_) -> d
               []    -> case filter isSafe [North, East, South, West] of
                          (d:_) -> d
                          []    -> case filter isPassable [North, East, South, West] of
                                     (d:_) -> d
                                     []    -> North
  where
    isPassable d =
      let target = movePos d p
      in isInsideGrid target && not (isWall (currentMap m) target)
    isSafe d = isPassable d && tileAt (effectTiles m) (movePos d p) /= Just FireTile

patrolMove :: GameState -> Position -> Direction -> BotMemory -> (Command, BotMemory)
patrolMove !m !pos !dir !mem =
  let !target = movePos dir pos
      isTargetSafe = isInsideGrid target
                  && not (isWall (currentMap m) target)
                  && tileAt (effectTiles m) target /= Just FireTile
  in if isTargetSafe && (tickCount mem `mod` 7 /= 0)
       then (Move dir, mem)
       else
         let validCandidates = [turnRight dir, turnLeft dir, oppositeDir dir]
             bestDir = pickNonWallDir m pos validCandidates
         in (Move bestDir, mem)

turnRight :: Direction -> Direction
turnRight North = East
turnRight East  = South
turnRight South = West
turnRight West  = North

turnLeft :: Direction -> Direction
turnLeft North = West
turnLeft West  = South
turnLeft South = East
turnLeft East  = North

oppositeDir :: Direction -> Direction
oppositeDir North = South
oppositeDir South = North
oppositeDir East  = West
oppositeDir West  = East

