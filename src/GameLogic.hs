-- GameLogic.hs
-- Logic trò chơi: va chạm, sát thương và trạng thái thế giới

{-# LANGUAGE BangPatterns #-}

module GameLogic
  ( movePos
  , isInsideGrid
  , isWall
  , chebyshevDist
  , manhattanDist
  , pseudoRandom
  , findSafeSpawn
  , spawnTank
  , changeMap
  , applyDamage
  , applyDamageWith
  , applyHalfDamage
  , playerNameOf
  , setPlayerNames
  , tankIsHasted
  , weakenedDamage
  , fireCooldownOf
  , heroHeldByOthers
  , roomStartBlockReason
  , handlePlayerSurrenderOrDisconnect
  , creditKill
  , executeHeroUltimate
  , ultDamageOn
  , ultWouldHit
  , inRadius
  , blinkDestination
  , volleyLanes
  , ultLineCells
  , ultTouches
  , rayCells
  , initialOrbs
  , matchLeader
  , applyTileOnEnter
  , randomStream
  , generateTiles
  , starSpot
  , stepWorldEvents
  , updateGameState
  , stepGameTick
  , spawnPowerUp
  , collectPowerUps
  , oppositeDir
  , parCheckCollisions
  , parMapValues
  , makeOfflineState
  , makeOfflineStateWithBot
  , pickBotHero
  , makeLocal2PState
  , resolveMatchOutcome
  , resolveLocal2PMatchOutcome
  ) where

import GHC.Conc (par, pseq)

import Types
import Hero
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (find, minimumBy, intercalate, sortBy)
import Data.Ord (comparing, Down(..))
import Data.Maybe (fromMaybe)

movePos :: Direction -> Position -> Position
movePos dir (Pos !x !y) = case dir of
  North -> Pos x (y + 1)
  South -> Pos x (y - 1)
  East  -> Pos (x + 1) y
  West  -> Pos (x - 1) y

chebyshevDist :: Position -> Position -> Int
chebyshevDist (Pos !x1 !y1) (Pos !x2 !y2) =
  max (abs (x1 - x2)) (abs (y1 - y2))

manhattanDist :: Position -> Position -> Int
manhattanDist (Pos !x1 !y1) (Pos !x2 !y2) =
  abs (x1 - x2) + abs (y1 - y2)

isInsideGrid :: Position -> Bool
isInsideGrid (Pos !x !y) =
  x >= 0 && x < gridWidth && y >= 0 && y < gridHeight

isWall :: MapId -> Position -> Bool
isWall !m !p = Set.member p (mapWallSet m)

-- Bộ sinh số giả ngẫu nhiên tuyến tính thuần túy (LCG)
pseudoRandom :: Int -> Int
pseudoRandom !seed =
  let !x1 = (seed * 1103515245 + 12345) `mod` 2147483648
      !x2 = (x1 * 1103515245 + 12345) `mod` 2147483648
  in x2 `div` 65536

-- Chọn ngẫu nhiên một góc xuất phát còn trống, hoặc quét mở rộng quanh góc nếu cả 4 góc đều bận
findSafeSpawn :: MapId -> Int -> [Tank] -> Position
findSafeSpawn !m !seed aliveTanks =
  let occupiedPos = map tankPos (filter (\t -> tankRespawnTimer t <= 0) aliveTanks)
      isFree p    = isInsideGrid p && not (isWall m p) && p `notElem` occupiedPos
      corners     = mapSpawnCorners m
      freeCorners = filter isFree corners
      nearCorners = [ Pos (cx + dx) (cy + dy)
                    | r <- [1 .. 6], Pos cx cy <- corners
                    , dx <- [-r .. r], dy <- [-r .. r]
                    , max (abs dx) (abs dy) == r ]
  in case freeCorners of
       [] -> case find isFree nearCorners of
               Just p  -> p
               Nothing -> Pos 1 1
       _  -> freeCorners !! (pseudoRandom seed `mod` length freeCorners)

spawnSeed :: Int -> Int -> Int
spawnSeed !tick !pId = tick * 7919 + pId * 104729

spawnTank :: Int -> TankClass -> GameState -> GameState
spawnTank !pId !cls !st =
  let !safeP   = findSafeSpawn (currentMap st) (spawnSeed (gameTickCount st) pId)
                               (filter (\t -> tankId t /= pId) (Map.elems (tanks st)))
      !pName   = playerNameOf st pId
      !newTank = Tank
        { tankId              = pId
        , tankPos             = safeP
        , tankDir             = North
        , tankHealth          = heroMaxHealth cls
        , tankEnergy          = 0
        , tankFireCooldown    = 0
        , tankMoveCooldown    = 0
        , tankRespawnTimer    = 0
        , tankInvincibleTimer = 60
        , tankClass           = cls
        , tankShield          = 0
        , tankBurnTimer       = 0
        , tankSilenceTimer    = 0
        , tankStarTimer       = 0
        , tankPierceTimer     = 0
        , tankStealthTimer    = 0
        , tankFreezeTimer     = 0
        , tankEmpTimer        = 0
        , tankHexShieldTimer  = 0
        , tankAcidTimer       = 0
        , tankBlackHoleTimer  = 0
        , tankLaserTimer      = 0
        , tankLightningTimer  = 0
        , tankName            = pName
        , tankArmorBreakTimer = 0
        , tankPoisonTimer     = 0
        , tankBlindTimer      = 0
        , tankHasteTimer      = 0
        , tankReflectTimer    = 0
        , tankWeakenTimer     = 0
        , tankDamageCarry     = 0
        }
      !newTanks = Map.insert pId newTank (tanks st)
      !newScore = Map.insertWith (\_ old -> old) pId 0 (score st)
  in st { tanks = newTanks, score = newScore }

changeMap :: MapId -> GameState -> GameState
changeMap !newMap !st =
  let !base = st { currentMap = newMap, bullets = [], powerUps = []
                 , tanks = Map.empty
                 , effectTiles = Map.empty
                 , worldSeed = worldSeed st + 7919
                 , score = Map.map (const 0) (Map.restrictKeys (score st) (Map.keysSet (tanks st))) }
      respawnOne !acc t = spawnTank (tankId t) (tankClass t) acc
  in foldl' respawnOne base (Map.elems (tanks st))

applyDamage :: Int -> Tank -> (Tank, Bool)
applyDamage = applyDamageWith False

-- Trừ sát thương qua giáp ảo trước khi trừ máu thực (trừ khi là sát thương chuẩn)
applyDamageWith :: Bool -> Int -> Tank -> (Tank, Bool)
applyDamageWith !trueDamage !dmg !t
  | tankRespawnTimer t > 0 || tankInvincibleTimer t > 0 = (t, False)
  | otherwise =
      let !absorbed  = if trueDamage then 0 else min (tankShield t) dmg
          !shield'   = tankShield t - absorbed
          !health'   = tankHealth t - (dmg - absorbed)
      in if health' <= 0
           then ( t { tankHealth = 0, tankShield = 0, tankRespawnTimer = 90
                    , tankBurnTimer = 0, tankSilenceTimer = 0, tankPierceTimer = 0
                    , tankPoisonTimer = 0, tankFreezeTimer = 0, tankEmpTimer = 0 }
                , True )
           else (t { tankHealth = health', tankShield = shield' }, False)

-- Tính sát thương theo đơn vị nửa điểm máu, phần dư 0.5 lưu vào tankDamageCarry cho phát kế tiếp
applyHalfDamage :: Bool -> Int -> Tank -> (Tank, Bool)
applyHalfDamage !trueDamage !halves !t
  | tankRespawnTimer t > 0 || tankInvincibleTimer t > 0 = (t, False)
  | otherwise =
      let !total            = max 0 halves + tankDamageCarry t
          (!whole, !carry)  = total `divMod` 2
          (!hitT, !killed)  = applyDamageWith trueDamage whole t
      in (hitT { tankDamageCarry = if killed then 0 else carry }, killed)

applyTileOnEnter :: Map.Map Position TileEffect -> Tank -> Tank
applyTileOnEnter !tiles !t = case tileAt tiles (tankPos t) of
  Just FireTile    -> t { tankBurnTimer    = fireBurnDuration }
  Just SilenceTile -> t { tankSilenceTimer = silenceDuration }
  _                -> t

spawnPowerUp :: Int -> Position -> OrbType -> GameState -> GameState
spawnPowerUp !oId !pos !oType !st =
  if length (filter (\o -> orbType o `notElem` [StarOrb, PierceOrb]) (powerUps st)) >= 6
     || isWall (currentMap st) pos || not (isInsideGrid pos)
     || any (\o -> orbPos o == pos) (powerUps st)
    then st
    else
      let !newOrb = PowerUp { orbId = oId, orbPos = pos, orbType = oType }
      in st { powerUps = newOrb : powerUps st }

collectPowerUps :: [PowerUp] -> Tank -> ([PowerUp], Tank, Int)
collectPowerUps currentOrbs tank =
  foldl' checkCollect ([], tank, 0) currentOrbs
  where
    checkCollect (!remOrbs, !t, !scoreBonus) orb =
      if isColliding orb t
        then
          let !mainEffect = case orbType orb of
                HealOrb ->
                  t { tankHealth = min (heroMaxHealth (tankClass t)) (tankHealth t + 150) }
                EnergyOrb ->
                  t { tankEnergy = min maxEnergy (tankEnergy t + energyOrbGain) }
                OverchargeOrb ->
                  t { tankEnergy = maxEnergy
                    , tankFireCooldown = 0
                    , tankMoveCooldown = 0
                    }
                ShieldOrb ->
                  t { tankShield = tankShield t + shieldOrbAmount }
                StarOrb ->
                  t { tankInvincibleTimer = max (tankInvincibleTimer t) starDuration
                    , tankStarTimer       = starDuration }
                PierceOrb ->
                  t { tankPierceTimer = pierceDuration }
              !updatedTank = mainEffect
                { tankShield = min maxShield (tankShield mainEffect + orbShieldBonus) }
          in (remOrbs, updatedTank, scoreBonus + orbScore (orbType orb))
        else (orb : remOrbs, t, scoreBonus)

playerNameOf :: GameState -> Int -> String
playerNameOf !st !pId =
  case Map.lookup pId (gsPlayerNames st) of
    Just n | not (null n) -> n
    _ -> case Map.lookup pId (tanks st) of
           Just t | not (null (tankName t)) -> tankName t
           _ | isBotPlayer pId -> "BOT"
             | otherwise       -> "NGƯỜI CHƠI " ++ show pId

setPlayerNames :: [(Int, String)] -> GameState -> GameState
setPlayerNames named !st =
  let !names' = foldl' (\m (pid, n) -> Map.insert pid n m) (gsPlayerNames st) named
      !tanks' = Map.mapWithKey (\pid t -> maybe t (\n -> t { tankName = n }) (lookup pid named)) (tanks st)
  in st { gsPlayerNames = names', tanks = tanks' }

tankIsHasted :: Tank -> Bool
tankIsHasted !t =
  tankHasteTimer t > 0 || (tankClass t == PhantomViolet && tankStealthTimer t > 0)

weakenedDamage :: Tank -> Int -> Int
weakenedDamage !t !dmg = if tankWeakenTimer t > 0 then dmg * 7 `div` 10 else dmg

fireCooldownOf :: Tank -> Int
fireCooldownOf !t =
  let !base   = heroBaseFireCooldown (tankClass t)
      !hasted = if tankHasteTimer t > 0 then max 3 (base `div` 2) else base
  in if tankBlindTimer t > 0 then hasted * 2 else hasted

isCombatCommand :: Command -> Bool
isCombatCommand c = case c of
  Move _       -> True
  Shoot        -> True
  CastUltimate -> True
  _            -> False

heroHeldByOthers :: Int -> TankClass -> Map.Map Int (String, TankClass, Maybe TankClass, Bool) -> Bool
heroHeldByOthers !pId !cls players =
  any (\(other, (_, hov, mLock, _)) -> other /= pId && (hov == cls || mLock == Just cls)) (Map.toList players)

roomStartBlockReason :: Int -> Map.Map Int (String, TankClass, Maybe TankClass, Bool) -> Maybe String
roomStartBlockReason !hostId players
  | Map.size players < 2 = Just "Không thể vào trận! Phòng cần ít nhất 2 người chơi!"
  | not (null unready)   = Just ("Không thể vào trận! Người chơi [" ++ intercalate ", " unready ++ "] chưa sẵn sàng!")
  | otherwise            = Nothing
  where
    unready = [ n | (pid, (n, _, _, rdy)) <- Map.toList players, pid /= hostId, not rdy ]

assignInitialHovers :: Map.Map Int (String, TankClass, Maybe TankClass, Bool)
                    -> Map.Map Int (String, TankClass, Maybe TankClass, Bool)
assignInitialHovers players =
  fst (foldl' pick (Map.empty, filter heroIsBase allHeroes) (Map.toList players))
  where
    pick (acc, pool) (pid, (n, _, _, rdy)) = case pool of
      (h : rest) -> (Map.insert pid (n, h, Nothing, rdy) acc, rest)
      []         -> (Map.insert pid (n, VanguardCyan, Nothing, rdy) acc, [])

uniqueLobbyName :: String -> Map.Map Int (String, TankClass, Maybe TankClass, Bool) -> String
uniqueLobbyName base players =
  let taken = [ n | (n, _, _, _) <- Map.elems players ]
      candidates = base : [ base ++ show k | k <- [2 :: Int ..] ]
  in case filter (`notElem` taken) candidates of
       (n : _) -> n
       []      -> base

updateGameState :: Int -> Command -> GameState -> GameState
updateGameState !pId !cmd !st
  | isCombatCommand cmd && gsRoomPhase st == PhaseMatchOver = st
  | otherwise = applyCommand pId cmd st

applyCommand :: Int -> Command -> GameState -> GameState
applyCommand !pId !cmd !st = case cmd of
  Move dir ->
    case Map.lookup pId (tanks st) of
      Nothing -> st
      Just tank ->
        if tankRespawnTimer tank > 0
          then st
          else
            let !targetPos = movePos dir (tankPos tank)
                !validGrid  = isInsideGrid targetPos && not (isWall (currentMap st) targetPos)
                !collisionOtherTank =
                  any (\t -> tankId t /= pId && tankRespawnTimer t <= 0 && tankPos t == targetPos)
                      (Map.elems (tanks st))

                !canStep = tankMoveCooldown tank <= 0 && tankFreezeTimer tank <= 0
                           && validGrid && not collisionOtherTank

                !inMud = tileAt (effectTiles st) (tankPos tank) == Just MudTile
                !stepCooldown = case (inMud, tankIsHasted tank) of
                                  (True,  True)  -> mudMoveCooldown `div` 2
                                  (True,  False) -> mudMoveCooldown
                                  (False, True)  -> 1
                                  (False, False) -> 2

                !movedTank =
                  if canStep
                    then applyTileOnEnter (effectTiles st)
                           tank { tankPos = targetPos, tankDir = dir, tankMoveCooldown = stepCooldown }
                    else tank { tankDir = dir }

                (!remOrbs, !finalTank, !orbBonusScore) =
                  collectPowerUps (powerUps st) movedTank

                !newTanks  = Map.insert pId finalTank (tanks st)
                !newScores = if orbBonusScore > 0
                               then Map.insertWith (+) pId orbBonusScore (score st)
                               else score st
            in st { tanks = newTanks, powerUps = remOrbs, score = newScores }

  Shoot ->
    case Map.lookup pId (tanks st) of
      Nothing -> st
      Just tank ->
        if tankRespawnTimer tank > 0 || tankFireCooldown tank > 0 || tankEmpTimer tank > 0
          then st
          else
            let !barrelPos      = movePos (tankDir tank) (tankPos tank)
                !tankAfterShoot = tank { tankFireCooldown = fireCooldownOf tank }
                !newTanks       = Map.insert pId tankAfterShoot (tanks st)
            in if isInsideGrid barrelPos && not (isWall (currentMap st) barrelPos)
                 then
                   let !newBullet = Bullet
                         { bulletPos    = barrelPos
                         , bulletDir    = tankDir tank
                         , bulletOwner  = pId
                         , bulletDamage = weakenedDamage tank (heroShotDamageHalf (tankClass tank))
                         , bulletPierce = tankPierceTimer tank > 0
                         , bulletRange  = normalShotRange
                         , bulletSkill  = False
                         }
                   in st { tanks = newTanks, bullets = newBullet : bullets st }
                 else st { tanks = newTanks }

  CastUltimate ->
    case Map.lookup pId (tanks st) of
      Nothing -> st
      Just tank ->
        if tankRespawnTimer tank > 0 || tankEnergy tank < maxEnergy
           || tankSilenceTimer tank > 0 || tankEmpTimer tank > 0
          then st
          else castUltimate pId tank st

  SelectClass newCls ->
    case (gsRoomPhase st, Map.lookup pId (tanks st)) of
      (PhaseNoRoom, Nothing) -> spawnTank pId newCls st
      _                      -> st

  SendChat msg ->
    st { chatLog = take 20 ((playerNameOf st pId, msg) : chatLog st) }

  CreateRoom -> st
  JoinRoom _ -> st
  StartMatch -> st

  CreateRoomCmd code username
    | gsRoomPhase st /= PhaseNoRoom && not (Map.null (gsLobbyPlayers st)) -> st
    | otherwise ->
        st { gsRoomCode       = code
           , gsRoomPhase      = PhaseWaitingLobby
           , gsRoomHost       = pId
           , gsLobbyPlayers   = Map.singleton pId (username, VanguardCyan, Nothing, True)
           , gsPlayerNames    = Map.singleton pId username
           , tanks            = Map.empty
           , bullets          = []
           , score            = Map.empty
           , gsLastKillBanner = Nothing
           , gsLastKillIds    = Nothing
           , gsLastQuit       = Nothing
           , gsNotice         = Just ("Phòng " ++ code ++ " đã được tạo bởi " ++ username ++ "!", noticeTicks)
           , chatLog          = [("HỆ THỐNG", "Phòng " ++ code ++ " đã được tạo bởi " ++ username ++ "!")]
           }

  JoinRoomCmd code username
    | gsRoomCode st /= code || gsRoomPhase st /= PhaseWaitingLobby -> st
    | Map.member pId (gsLobbyPlayers st) || Map.size (gsLobbyPlayers st) >= 3 -> st
    | otherwise ->
        let !uname = uniqueLobbyName username (gsLobbyPlayers st)
            !hov   = case filter (\c -> not (heroHeldByOthers pId c (gsLobbyPlayers st))) (filter heroIsBase allHeroes) of
                       (c : _) -> c
                       []      -> VanguardCyan
        in st { gsLobbyPlayers = Map.insert pId (uname, hov, Nothing, False) (gsLobbyPlayers st)
              , gsPlayerNames  = Map.insert pId uname (gsPlayerNames st)
              , gsNotice       = Just (uname ++ " đã vào phòng! (CHƯA SẴN SÀNG)", noticeTicks)
              , chatLog        = take 20 (("HỆ THỐNG", uname ++ " đã tham gia phòng " ++ code ++ "!") : chatLog st)
              }

  ToggleReadyCmd isReady ->
    case Map.lookup pId (gsLobbyPlayers st) of
      Just (uname, hov, lock, _)
        | gsRoomPhase st == PhaseWaitingLobby && pId /= gsRoomHost st ->
            let !msg = if isReady
                         then "Thông báo: Người chơi " ++ uname ++ " đã SẴN SÀNG!"
                         else "Thông báo: Người chơi " ++ uname ++ " hiện CHƯA SẴN SÀNG!"
            in st { gsLobbyPlayers = Map.insert pId (uname, hov, lock, isReady) (gsLobbyPlayers st)
                  , gsNotice       = Just (msg, noticeTicks)
                  }
      _ -> st

  HoverHeroCmd cls ->
    case (gsRoomPhase st, Map.lookup pId (gsLobbyPlayers st)) of
      (PhaseHeroPicking _, Just (uname, _, Nothing, rdy))
        | not (heroHeldByOthers pId cls (gsLobbyPlayers st)) ->
            st { gsLobbyPlayers = Map.insert pId (uname, cls, Nothing, rdy) (gsLobbyPlayers st) }
      _ -> st

  StartHeroPickCmd
    | pId /= gsRoomHost st || gsRoomPhase st /= PhaseWaitingLobby -> st
    | otherwise ->
        case roomStartBlockReason (gsRoomHost st) (gsLobbyPlayers st) of
          Just reason -> st { gsNotice = Just (reason, noticeTicks) }
          Nothing ->
            st { gsRoomPhase    = PhaseHeroPicking heroPickTicks
               , gsLobbyPlayers = assignInitialHovers (gsLobbyPlayers st)
               , gsNotice       = Just ("Tất cả đã sẵn sàng! Bắt đầu chọn tướng 30 giây!", noticeTicks)
               }

  LockHeroCmd cls ->
    case (gsRoomPhase st, Map.lookup pId (gsLobbyPlayers st)) of
      (PhaseHeroPicking _, Just (uname, _, Nothing, rdy))
        | not (heroHeldByOthers pId cls (gsLobbyPlayers st)) ->
            let !players'  = Map.insert pId (uname, cls, Just cls, rdy) (gsLobbyPlayers st)
                !allLocked = all (\(_, _, mC, _) -> mC /= Nothing) (Map.elems players')
                !st'       = st { gsLobbyPlayers = players' }
            in if allLocked then beginRoomMatch st' else st'
      _ -> st

  SurrenderCmd -> handlePlayerSurrenderOrDisconnect False pId st
  Disconnect   -> handlePlayerSurrenderOrDisconnect True pId st

lockHoveredHeroes :: GameState -> GameState
lockHoveredHeroes !st =
  let lockOne acc (pid, (n, hov, mLock, rdy)) = case mLock of
        Just c  -> Map.insert pid (n, c, Just c, rdy) acc
        Nothing ->
          let free c = not (heroHeldByOthers pid c acc)
              chosen = if free hov then hov else fromMaybe hov (find free allHeroes)
          in Map.insert pid (n, chosen, Just chosen, rdy) acc
  in st { gsLobbyPlayers = foldl' lockOne (gsLobbyPlayers st) (Map.toList (gsLobbyPlayers st)) }

beginRoomMatch :: GameState -> GameState
beginRoomMatch !st =
  let !players = gsLobbyPlayers st
      !seed    = worldSeed st + gameTickCount st
      !newMap  = cycleMap (1 + pseudoRandom seed `mod` (mapCount - 1)) (currentMap st)
      !base    = st { tanks            = Map.empty
                    , bullets          = []
                    , score            = Map.empty
                    , powerUps         = initialOrbs newMap
                    , effectTiles      = Map.empty
                    , currentMap       = newMap
                    , worldSeed        = seed
                    , gsLastKillBanner = Nothing
                    , gsLastKillIds    = Nothing
                    , gsLastQuit       = Nothing
                    }
      chosenOf (_, hov, mLock, _) = fromMaybe hov mLock
      !spawned = foldl' (\acc (pid, entry) -> spawnTank pid (chosenOf entry) acc) base (Map.toList players)
  in spawned { gsRoomPhase    = PhaseInMatch matchDurationTicks
             , gsLobbyPlayers = Map.map (\e@(n, _, _, r) -> (n, chosenOf e, Just (chosenOf e), r)) players
             , gsNotice       = Just ("Trận chiến 3 phút bắt đầu! Bản đồ: " ++ mapName newMap, noticeTicks)
             }

-- Xử lý đầu hàng hoặc mất kết nối: kết thúc trận nếu chỉ còn 1 người, hoặc chuyển quyền chủ phòng ở sảnh
handlePlayerSurrenderOrDisconnect :: Bool -> Int -> GameState -> GameState
handlePlayerSurrenderOrDisconnect !isDisconnect !pId !st =
  let !name       = playerNameOf st pId
      !inMatch    = case gsRoomPhase st of
                      PhaseInMatch _ -> Map.member pId (tanks st)
                      _              -> False
      !players'   = Map.delete pId (gsLobbyPlayers st)
      !tanks'     = Map.delete pId (tanks st)
      !names'     = if isDisconnect && not inMatch then Map.delete pId (gsPlayerNames st) else gsPlayerNames st
      !newHost    = if pId == gsRoomHost st
                      then maybe 0 fst (Map.lookupMin players')
                      else gsRoomHost st
      !players''  = Map.adjust (\(n, h, l, _) -> (n, h, l, True)) newHost players'
      !hostNote   = if newHost /= gsRoomHost st && newHost /= 0
                      then " " ++ playerNameOf st newHost ++ " trở thành chủ phòng."
                      else ""
      !base       = st { tanks          = tanks'
                       , bullets        = filter ((/= pId) . bulletOwner) (bullets st)
                       , gsLobbyPlayers = players''
                       , gsPlayerNames  = names'
                       , gsRoomHost       = newHost
                       }
      !humansLeft = length (filter (not . isBotPlayer) (Map.keys tanks'))
      !roomEmpty  = not (null (gsRoomCode st)) && Map.null players'
  in if roomEmpty
       then
         base { gsRoomPhase = PhaseNoRoom, gsRoomCode = "", gsRoomHost = 0, tanks = Map.empty
              , bullets = [], gsNotice = Nothing, gsLastKillBanner = Nothing, gsLastKillIds = Nothing
              , chatLog = [] }
       else case gsRoomPhase st of
         PhaseInMatch _
           | inMatch && humansLeft <= 1 ->
               base { gsRoomPhase = PhaseMatchOver
                    , bullets     = []
                    , gsLastQuit  = Just name
                    , gsNotice    = Just ("ĐỐI THỦ " ++ name ++ " ĐÃ ĐẦU HÀNG / THOÁT TRẬN!", noticeTicks * 3)
                    }
           | inMatch ->
               base { gsLastQuit = Just name
                    , gsNotice   = Just ("THÔNG BÁO: NGƯỜI CHƠI " ++ name ++ " ĐÃ THOÁT / ĐẦU HÀNG! TRẬN ĐẤU TIẾP TỤC!", noticeTicks)
                    }
         PhaseHeroPicking _
           | Map.size players'' < 2 ->
               base { gsRoomPhase    = PhaseWaitingLobby
                    , gsLobbyPlayers = Map.map (\(n, h, _, r) -> (n, h, Nothing, r)) players''
                    , gsNotice       = Just (name ++ " đã rời phòng! Quay lại phòng chờ." ++ hostNote, noticeTicks)
                    }
         PhaseMatchOver -> base
         _ | Map.member pId (gsLobbyPlayers st) ->
               base { gsNotice = Just ("Người chơi " ++ name ++ " đã rời phòng." ++ hostNote, noticeTicks) }
           | otherwise -> base

creditKill :: Int -> Int -> GameState -> GameState
creditKill !killerId !victimId !st =
  st { score            = Map.insertWith (+) killerId killScore (score st)
     , gsLastKillBanner = Just (playerNameOf st killerId, playerNameOf st victimId, bannerTicks)
     , gsLastKillIds    = Just (killerId, victimId)
     }

ultDamageOn :: Tank -> Tank -> Int
ultDamageOn !caster !target =
  min (ultDamageCap (tankClass target)) (weakenedDamage caster (heroUltDamage (tankClass caster)))

strikeWith :: Bool -> Tank -> (Tank -> Tank) -> Tank -> Tank
strikeWith !trueDmg !caster effect !target
  | tankInvincibleTimer target > 0 = target
  | otherwise =
      let (!hitT, !killed) = applyDamageWith trueDmg (ultDamageOn caster target) target
      in if killed then hitT else effect hitT

isEnemyOf :: Int -> Tank -> Bool
isEnemyOf !casterId !t = tankId t /= casterId && tankRespawnTimer t <= 0

rayCells :: MapId -> Direction -> Int -> Position -> [Position]
rayCells !m !dir !n !from =
  takeWhile (\p -> isInsideGrid p && not (isWall m p)) (take n (drop 1 (iterate (movePos dir) from)))

isFreeCell :: GameState -> Int -> Position -> Bool
isFreeCell !st !selfId !p =
  isInsideGrid p && not (isWall (currentMap st) p)
  && not (any (\t -> tankId t /= selfId && tankRespawnTimer t <= 0 && tankPos t == p) (Map.elems (tanks st)))

shoveTank :: GameState -> Int -> (Int, Int) -> Tank -> Tank
shoveTank !st = shoveTankWith (isFreeCell st)

shoveTankWith :: (Int -> Position -> Bool) -> Int -> (Int, Int) -> Tank -> Tank
shoveTankWith free !n (!dx, !dy) !t = t { tankPos = go n (tankPos t) }
  where
    go 0 p = p
    go k p@(Pos x y) =
      let nxt = Pos (x + dx) (y + dy)
      in if free (tankId t) nxt then go (k - 1 :: Int) nxt else p

dirVector :: Direction -> (Int, Int)
dirVector d = case d of
  North -> (0, 1)
  South -> (0, -1)
  East  -> (1, 0)
  West  -> (-1, 0)

castUltimate :: Int -> Tank -> GameState -> GameState
castUltimate !pId !tank !st =
  let !after   = executeHeroUltimate pId (tankClass tank) (tank { tankEnergy = 0 }) st
      !victims = [ (tid, tankRespawnTimer tA > 0)
                 | (tid, tB) <- Map.toList (tanks st), isEnemyOf pId tB
                 , Just tA <- [Map.lookup tid (tanks after)]
                 , tankRespawnTimer tA > 0 || tankHealth tA < tankHealth tB || tankShield tA < tankShield tB
                 ]
      !after1  = if null victims
                   then after
                   else after { score = Map.insertWith (+) pId (hitScore * length victims) (score after) }
  in foldl' (\s (tid, killed) -> if killed then creditKill pId tid s else s) after1 victims

-- Kiểm tra tâm mục tiêu nằm trong bán kính r + 0.5 ô (nhân 4 hai vế để giữ số nguyên)
inRadius :: Int -> Position -> Position -> Bool
inRadius !r (Pos !cx !cy) (Pos !x !y) =
  let !dx = x - cx
      !dy = y - cy
  in 4 * (dx * dx + dy * dy) <= (2 * r + 1) * (2 * r + 1)

-- Điểm đáp tốc biến của Phantom: dừng trước mặt kẻ địch đầu tiên trên đường nhảy
blinkDestination :: GameState -> Int -> Tank -> Position
blinkDestination !st !k !t =
  let path    = take k (drop 1 (iterate (movePos (tankDir t)) (tankPos t)))
      enemyAt p = any (\e -> isEnemyOf (tankId t) e && tankPos e == p) (Map.elems (tanks st))
      !limit  = length (takeWhile (not . enemyAt) path)
      jumps   = reverse (take limit path)
  in fromMaybe (tankPos t) (find (isFreeCell st (tankId t)) jumps)

volleyLanes :: Direction -> Position -> [Position]
volleyLanes !dir !p = [p, movePos (rotateCW dir) p, movePos (rotateCCW dir) p]

ultLineCells :: MapId -> TankClass -> Direction -> Position -> [Position]
ultLineCells !m !cls !dir !p = case specUltShape (heroSpec cls) of
  Beam r   -> rayCells m dir r p
  Shell r  -> rayCells m dir r p
  Cross r  -> concat [ rayCells m d r p | d <- [North, East, South, West] ]
  Volley r -> concat [ rayCells m dir r lane | lane <- volleyLanes dir p ]
  _        -> []

-- Kiểm tra ô p có nằm trong vùng tác dụng tuyệt kỹ của caster hay không
ultTouches :: GameState -> Tank -> Position -> Bool
ultTouches !st !caster !p = case specUltShape (specOf caster) of
  Radial r       -> inRadius r (tankPos caster) p
  Pull r         -> inRadius r (tankPos caster) p
  BlinkBurst k r -> inRadius r (blinkDestination st k caster) p
  Cross _        -> p `elem` line || inRadius 1 (tankPos caster) p
  _              -> p `elem` line
  where
    line = ultLineCells (currentMap st) (tankClass caster) (tankDir caster) (tankPos caster)

skillShell :: GameState -> Tank -> Int -> Bool -> Position -> [Bullet]
skillShell !st !caster !r !pierce !lane =
  let !dir   = tankDir caster
      !start = movePos dir lane
      !dmg   = 2 * weakenedDamage caster (heroUltDamage (tankClass caster))
  in [ Bullet start dir (tankId caster) dmg pierce (r - 1) True
     | isInsideGrid start, not (isWall (currentMap st) start) ]

-- Thực thi tuyệt kỹ theo lớp tướng (vùng tức thì hoặc phóng đạn kỹ năng)
executeHeroUltimate :: Int -> TankClass -> Tank -> GameState -> GameState
executeHeroUltimate !pId !cls !tank !st = case cls of
  VanguardCyan -> hitArea (\t -> t { tankFreezeTimer = freezeTicks }) tank

  TitanCrimson ->
    st { tanks   = Map.insert pId tank (tanks st)
       , bullets = skillShell st tank (ultRangeOf cls) False (tankPos tank) ++ bullets st }

  ViperEmerald ->
    let !healed = tank { tankHealth    = min (heroMaxHealth ViperEmerald) (tankHealth tank + 130)
                       , tankAcidTimer = 60 }
    in hitArea (\t -> t { tankPoisonTimer = poisonTicks }) healed

  SolarisGold -> hitArea (\t -> t { tankBlindTimer = blindTicks }) (tank { tankLaserTimer = 15 })

  PhantomViolet ->
    let !dest = blinkDestination st 4 tank
        !moved = (applyTileOnEnter (effectTiles st) tank { tankPos = dest })
                   { tankStealthTimer = phantomStealthTicks }
    in hitArea id moved

  StrikerRuby ->
    let !hasteTank = tank { tankHasteTimer = hasteTicks, tankMoveCooldown = 0 }
        !rockets   = concatMap (skillShell st tank 7 False) (volleyLanes (tankDir tank) (tankPos tank))
    in st { tanks = Map.insert pId hasteTank (tanks st), bullets = rockets ++ bullets st }

  AegisOnyx ->
    let !myPos = tankPos tank
        !fortified = tank
          { tankShield         = min (maxShield + aegisShieldGain) (tankShield tank + aegisShieldGain)
          , tankReflectTimer   = reflectTicks
          , tankHexShieldTimer = reflectTicks
          }
        push t = let tp = tankPos t
                 in shoveTank st 2 (signum (posX tp - posX myPos), signum (posY tp - posY myPos)) t
    in hitArea push fortified

  TempestAzure ->
    hitArea (\t -> t { tankEmpTimer = empTicks }) (tank { tankFireCooldown = 0, tankLightningTimer = 20 })

  MirageAmethyst ->
    hitArea id tank { tankStealthTimer = mirageTicks
                    , tankPierceTimer  = max (tankPierceTimer tank) mirageTicks
                    , tankLaserTimer   = 15 }

  EclipseObsidian ->
    let !myPos = tankPos tank
        pull t = let tp = tankPos t
                 in shoveTank st 2 (signum (posX myPos - posX tp), signum (posY myPos - posY tp)) t
    in hitArea (\t -> pull t { tankWeakenTimer = weakenTicks }) (tank { tankBlackHoleTimer = 45 })
  where
    !trueDmg = specUltTrueDmg (heroSpec cls)
    touched t = isEnemyOf pId t && ultTouches st tank (tankPos t)
    hitArea effect casterAfter =
      let !hitTanks = Map.map (\t -> if touched t then strikeWith trueDmg tank effect t else t) (tanks st)
      in st { tanks = Map.insert pId casterAfter hitTanks }

ultWouldHit :: Int -> GameState -> Bool
ultWouldHit !pId !st = case Map.lookup pId (tanks st) of
  Nothing   -> False
  Just tank ->
    any (\t -> isEnemyOf pId t && tankInvincibleTimer t <= 0 && ultTouches st tank (tankPos t))
        (Map.elems (tanks st))

rotateCW :: Direction -> Direction
rotateCW North = East
rotateCW East  = South
rotateCW South = West
rotateCW West  = North

rotateCCW :: Direction -> Direction
rotateCCW North = West
rotateCCW West  = South
rotateCCW South = East
rotateCCW East  = North

oppositeDir :: Direction -> Direction
oppositeDir North = South
oppositeDir South = North
oppositeDir East  = West
oppositeDir West  = East

-- Tiến một nhịp 30 FPS theo giai đoạn của phòng đấu
stepGameTick :: GameState -> GameState
stepGameTick !st0 =
  let !tick' = gameTickCount st0 + 1
      !st    = decayAnnouncements st0
  in case gsRoomPhase st0 of
       PhaseMatchOver    -> st { gameTickCount = tick', bullets = [] }
       PhaseWaitingLobby -> st { gameTickCount = tick' }
       PhaseHeroPicking remTicks
         | remTicks > 1 -> st { gameTickCount = tick', gsRoomPhase = PhaseHeroPicking (remTicks - 1) }
         | otherwise    -> (beginRoomMatch (lockHoveredHeroes st)) { gameTickCount = tick' }
       PhaseInMatch remTicks
         | remTicks > 1 -> simulateWorld st { gsRoomPhase = PhaseInMatch (remTicks - 1) }
         | otherwise    -> st { gameTickCount = tick'
                              , gsRoomPhase   = PhaseMatchOver
                              , bullets       = []
                              , gsNotice      = Just ("HẾT GIỜ! TRẬN ĐẤU KẾT THÚC!", noticeTicks * 3) }
       PhaseNoRoom -> simulateWorld st

decayAnnouncements :: GameState -> GameState
decayAnnouncements !st =
  let !banner' = case gsLastKillBanner st of
        Just (k, v, n) | n > 1 -> Just (k, v, n - 1)
        _                      -> Nothing
      !notice' = case gsNotice st of
        Just (msg, n) | n > 1 -> Just (msg, n - 1)
        _                     -> Nothing
  in st { gsLastKillBanner = banner'
        , gsLastKillIds    = if banner' == Nothing then Nothing else gsLastKillIds st
        , gsNotice         = notice' }

-- Mô phỏng 1 nhịp trận đấu: cập nhật xe song song bằng parMapValues, xử lý đạn bay và sự kiện bản đồ
simulateWorld :: GameState -> GameState
simulateWorld !st =
  let !currentTick = gameTickCount st + 1
      !steppedTanks = parMapValues (stepSingleTank currentTick) (tanks st)
      !respawningIds = [ tankId t | t <- Map.elems (tanks st), tankRespawnTimer t == 1 ]
      placeRespawn !acc !pid =
        case Map.lookup pid acc of
          Nothing -> acc
          Just t  ->
            let !others  = filter (\o -> tankId o /= pid) (Map.elems acc)
                !safePos = findSafeSpawn (currentMap st) (spawnSeed currentTick pid) others
            in Map.insert pid t { tankPos = safePos } acc
      !updatedTanks = foldl' placeRespawn steppedTanks respawningIds

      (!bulletsPreMove, !tanksPreMove, !scoresPreMove, !kills1) =
        resolveBulletCollisions (currentMap st) (bullets st) updatedTanks (score st)

      !movedBullets = [ b { bulletPos = movePos (bulletDir b) (bulletPos b), bulletRange = bulletRange b - 1 }
                      | b <- bulletsPreMove
                      , bulletRange b > 0
                      ]

      !activeBullets = filter (\b -> isInsideGrid (bulletPos b) && not (isWall (currentMap st) (bulletPos b)))
                              movedBullets

      (!survivingBullets, !tanksAfterCombat, !scoresAfterCombat, !kills2) =
        resolveBulletCollisions (currentMap st) activeBullets tanksPreMove scoresPreMove

      !afterCombat = st
        { tanks         = tanksAfterCombat
        , bullets       = survivingBullets
        , score         = scoresAfterCombat
        , gameTickCount = currentTick
        }
      !withKills = foldl' (\s (killer, victim) -> creditKill killer victim s) afterCombat (kills1 ++ kills2)
  in stepWorldEvents withKills
  where
    stepSingleTank !tick !t =
      let !alive      = tankRespawnTimer t <= 0
          !freeze'    = max 0 (tankFreezeTimer t - 1)
          !emp'       = max 0 (tankEmpTimer t - 1)
          !poison'    = max 0 (tankPoisonTimer t - 1)

          !moveCd'    = max 0 (tankMoveCooldown t - 1)
          !fireCd'    = max 0 (tankFireCooldown t - 1)
          !moveCdFinal = if freeze' > 0 then max 2 moveCd' else moveCd'
          !fireCdFinal = if emp' > 0 then max 2 fireCd' else fireCd'

          !tTimers0 = t
            { tankFireCooldown    = fireCdFinal
            , tankMoveCooldown    = moveCdFinal
            , tankInvincibleTimer = max 0 (tankInvincibleTimer t - 1)
            , tankBurnTimer       = if alive then max 0 (tankBurnTimer t - 1) else 0
            , tankSilenceTimer    = if alive then max 0 (tankSilenceTimer t - 1) else 0
            , tankStarTimer       = max 0 (tankStarTimer t - 1)
            , tankPierceTimer     = max 0 (tankPierceTimer t - 1)
            , tankStealthTimer    = max 0 (tankStealthTimer t - 1)
            , tankFreezeTimer     = freeze'
            , tankEmpTimer        = emp'
            , tankHexShieldTimer  = max 0 (tankHexShieldTimer t - 1)
            , tankAcidTimer       = max 0 (tankAcidTimer t - 1)
            , tankBlackHoleTimer  = max 0 (tankBlackHoleTimer t - 1)
            , tankLaserTimer      = max 0 (tankLaserTimer t - 1)
            , tankLightningTimer  = max 0 (tankLightningTimer t - 1)
            , tankArmorBreakTimer = max 0 (tankArmorBreakTimer t - 1)
            , tankPoisonTimer     = poison'
            , tankBlindTimer      = max 0 (tankBlindTimer t - 1)
            , tankHasteTimer      = max 0 (tankHasteTimer t - 1)
            , tankReflectTimer    = max 0 (tankReflectTimer t - 1)
            , tankWeakenTimer     = max 0 (tankWeakenTimer t - 1)
            }
          !tBurn = if alive && tankBurnTimer tTimers0 > 0 && tick `mod` fireTickInterval == 0
                     then fst (applyDamage fireTickDamage tTimers0)
                     else tTimers0
          !tTimers = if alive && poison' > 0 && tankRespawnTimer tBurn <= 0 && tick `mod` poisonTickInterval == 0
                       then fst (applyDamage poisonTickDamage tBurn)
                       else tBurn
      in if tankRespawnTimer t > 1
           then tTimers { tankRespawnTimer = tankRespawnTimer t - 1 }
           else if tankRespawnTimer t == 1
             then
               tTimers { tankHealth          = heroMaxHealth (tankClass t)
                       , tankEnergy          = 0
                       , tankRespawnTimer    = 0
                       , tankInvincibleTimer = 60
                       , tankShield          = 0
                       , tankBurnTimer       = 0
                       , tankSilenceTimer    = 0
                       , tankPierceTimer     = 0
                       , tankStealthTimer    = 0
                       , tankFreezeTimer     = 0
                       , tankEmpTimer        = 0
                       , tankHexShieldTimer  = 0
                       , tankAcidTimer       = 0
                       , tankBlackHoleTimer  = 0
                       , tankLaserTimer      = 0
                       , tankLightningTimer  = 0
                       , tankArmorBreakTimer = 0
                       , tankPoisonTimer     = 0
                       , tankBlindTimer      = 0
                       , tankHasteTimer      = 0
                       , tankReflectTimer    = 0
                       , tankWeakenTimer     = 0
                       , tankDamageCarry     = 0
                       }
             else tTimers

-- Kiểm tra va chạm giữa danh sách đạn và xe tăng bằng foldl' nghiêm ngặt
resolveBulletCollisions
  :: MapId
  -> [Bullet]
  -> Map.Map Int Tank
  -> Map.Map Int Int
  -> ([Bullet], Map.Map Int Tank, Map.Map Int Int, [(Int, Int)])
resolveBulletCollisions mapId rawBullets initialTanks initialScores =
  let (!bs, !ts, !ss, !ks) = foldl' processOneBullet ([], initialTanks, initialScores, []) rawBullets
  in (reverse bs, ts, ss, reverse ks)
  where
    processOneBullet (!bAcc, !tAcc, !sAcc, !kAcc) !b =
      let !bPos     = bulletPos b
          !bPrevPos = movePos (oppositeDir (bulletDir b)) bPos
          !owner    = bulletOwner b
          mVictim   = find (\t -> tankId t /= owner
                               && tankRespawnTimer t <= 0
                               && (tankPos t == bPos || (tankPos t == bPrevPos && tankDir t == oppositeDir (bulletDir b))))
                          (Map.elems tAcc)
      in case mVictim of
           Nothing -> (b : bAcc, tAcc, sAcc, kAcc)
           Just victim
             | tankInvincibleTimer victim > 0 -> (bAcc, tAcc, sAcc, kAcc)
             | bulletSkill b -> skillHit b victim (bAcc, tAcc, sAcc, kAcc)
             | otherwise     -> normalHit b victim (bAcc, tAcc, sAcc, kAcc)

    normalHit !b !victim (!bAcc, !tAcc, !sAcc, !kAcc) =
      let !owner = bulletOwner b
          !dmg   = if tankArmorBreakTimer victim > 0
                     then bulletDamage b * 6 `div` 5
                     else bulletDamage b
          (!hitVictim, !isKilled) = applyHalfDamage (bulletPierce b) dmg victim
          !victimWithEnergy = hitVictim { tankEnergy = min maxEnergy (tankEnergy hitVictim + damagedEnergyGain) }
          !tanksAfterHit = Map.insert (tankId victim) victimWithEnergy tAcc
          (!tanksWithShooter, !reflectKill) =
            case Map.lookup owner tanksAfterHit of
              Just shooter ->
                let !charged = shooter { tankEnergy = min maxEnergy (tankEnergy shooter + hitEnergyGain) }
                    (!reflected, !shooterDied) =
                      if tankReflectTimer victim > 0 && tankRespawnTimer shooter <= 0
                        then applyHalfDamage False (dmg * 3 `div` 10) charged
                        else (charged, False)
                in (Map.insert owner reflected tanksAfterHit, shooterDied)
              Nothing -> (tanksAfterHit, False)
          !newKills = [ (owner, tankId victim) | isKilled ] ++ [ (tankId victim, owner) | reflectKill ]
      in (bAcc, tanksWithShooter, Map.insertWith (+) owner hitScore sAcc, reverse newKills ++ kAcc)

    skillHit !b !victim (!bAcc, !tAcc, !sAcc, !kAcc) =
      let !owner   = bulletOwner b
          !capHalf = 2 * ultDamageCap (tankClass victim)
          (!hitVictim, !isKilled) = applyHalfDamage (bulletPierce b) (min capHalf (bulletDamage b)) victim
          !shooterCls = maybe VanguardCyan tankClass (Map.lookup owner tAcc)
          !afterEffect = if isKilled then hitVictim else skillOnHit shooterCls (bulletDir b) hitVictim
          !victimWithEnergy = afterEffect { tankEnergy = min maxEnergy (tankEnergy afterEffect + damagedEnergyGain) }
      in ( bAcc
         , Map.insert (tankId victim) victimWithEnergy tAcc
         , Map.insertWith (+) owner hitScore sAcc
         , [ (owner, tankId victim) | isKilled ] ++ kAcc )

    skillOnHit cls dir t = case cls of
      TitanCrimson -> shoveTankWith freeCell 2 (dirVector dir) (t { tankArmorBreakTimer = armorBreakTicks })
      _            -> t

    freeCell selfId p = isInsideGrid p && not (isWall mapId p)
      && not (any (\o -> tankId o /= selfId && tankRespawnTimer o <= 0 && tankPos o == p) (Map.elems initialTanks))

-- Dòng số giả ngẫu nhiên vô hạn (lazy stream) phục vụ sinh địa hình theo hạt giống
randomStream :: Int -> [Int]
randomStream !seed = map (`div` 65536) (drop 1 (iterate lcgStep (seed `mod` 2147483648)))
  where
    lcgStep x = (x * 1103515245 + 12345) `mod` 2147483648

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf !n xs = let (h, t) = splitAt n xs in h : chunksOf n t

tileShapes :: [[(Int, Int)]]
tileShapes =
  [ [ (0, 0), (1, 0), (0, 1), (1, 1) ]
  , [ (0, 0), (1, 0), (2, 0) ]
  , [ (0, 0), (0, 1), (0, 2) ]
  , [ (0, 0), (1, 0), (2, 0), (0, 1), (1, 1), (2, 1) ]
  , [ (0, 0), (1, 0), (0, 1) ]
  , [ (0, 0), (1, 1), (2, 0) ]
  ]

nearSpawnCorner :: MapId -> Position -> Bool
nearSpawnCorner !m (Pos x y) =
  any (\(Pos cx cy) -> abs (x - cx) <= 2 && abs (y - cy) <= 2) (mapSpawnCorners m)

-- Sinh các cụm ô hiệu ứng đặc biệt tránh tường và vùng xuất phát ở 4 góc
generateTiles :: MapId -> Int -> Map.Map Position TileEffect
generateTiles !m !seed =
  case randomStream seed of
    []          -> Map.empty
    (r0 : rest) ->
      let nClusters = 7 + r0 `mod` 5
          clusters  = take nClusters (chunksOf 4 rest)
          allowed p = isInsideGrid p && not (isWall m p) && not (nearSpawnCorner m p)
          toCells [rType, rShape, rx, ry] =
            let !eff   = toEnum (rType `mod` 3) :: TileEffect
                !shape = tileShapes !! (rShape `mod` length tileShapes)
                !ox    = 1 + rx `mod` (gridWidth - 3)
                !oy    = 1 + ry `mod` (gridHeight - 3)
            in [ (p, eff) | (dx, dy) <- shape, let p = Pos (ox + dx) (oy + dy), allowed p ]
          toCells _ = []
      in Map.fromList (concatMap toCells clusters)

starSpot :: MapId -> Position
starSpot !m =
  let dist (Pos x y) = abs (2 * x - 31) + abs (2 * y - 21)
      freeCells = [ Pos x y | x <- [8 .. 23], y <- [5 .. 16], not (isWall m (Pos x y)) ]
  in if null freeCells then Pos 15 10 else minimumBy (comparing dist) freeCells

randomFreeCell :: GameState -> Int -> Maybe Position
randomFreeCell !st !seed =
  let m        = currentMap st
      occupied = map tankPos (Map.elems (tanks st)) ++ map orbPos (powerUps st)
      cells    = [ p | x <- [1 .. gridWidth - 2], y <- [1 .. gridHeight - 2], let p = Pos x y
                     , not (isWall m p), not (Map.member p (effectTiles st))
                     , p `notElem` occupied, not (nearSpawnCorner m p) ]
  in if null cells then Nothing else Just (cells !! (pseudoRandom seed `mod` length cells))

-- Cập nhật sự kiện định kỳ trên bản đồ (đổi ô hiệu ứng, sinh ngôi sao, đạn xuyên giáp và ngọc)
stepWorldEvents :: GameState -> GameState
stepWorldEvents !st =
  let !tick = gameTickCount st
      !m    = currentMap st
      !seed = worldSeed st * 7919 + tick
      hasGoldOrb s = any (isGoldOrb . orbType) (powerUps s)

      !st1 = if Map.null (effectTiles st) || tick `mod` tileShuffleInterval == 0
               then st { effectTiles = generateTiles m seed }
               else st

      !st2 = if tick `mod` starInterval == starOffset && not (hasGoldOrb st1)
               then let !spot = starSpot m
                        !star = PowerUp { orbId = 50000 + tick, orbPos = spot, orbType = StarOrb }
                    in st1 { powerUps    = star : filter (\o -> orbPos o /= spot) (powerUps st1)
                           , effectTiles = Map.delete spot (effectTiles st1) }
               else st1

      !st3 = if tick `mod` pierceSpawnInterval == pierceSpawnInterval `div` 2
                && not (any (\o -> orbType o == PierceOrb) (powerUps st2))
               then case randomFreeCell st2 (seed * 31 + 17) of
                      Just p  -> st2 { powerUps = PowerUp (70000 + tick) p PierceOrb : powerUps st2 }
                      Nothing -> st2
               else st2

      !spots = mapOrbSpots m
      !st4 = if tick `mod` orbSpawnInterval == 0 && not (null spots)
               then let idx  = (tick `div` orbSpawnInterval) `mod` length spots
                        (p, baseType) = spots !! idx
                        roll = pseudoRandom (seed * 13 + 7) `mod` 100
                        oType | roll < goldOrbChance && not (hasGoldOrb st3) = OverchargeOrb
                              | isGoldOrb baseType = blueOrbs !! (roll `mod` length blueOrbs)
                              | otherwise          = baseType
                    in spawnPowerUp (1000 + tick `div` orbSpawnInterval) p oType st3
               else st3
  in st4

blueOrbs :: [OrbType]
blueOrbs = [HealOrb, EnergyOrb, ShieldOrb]

initialOrbs :: MapId -> [PowerUp]
initialOrbs !m =
  [ PowerUp i p o | (i, (p, o)) <- zip [1 ..] (take 3 (filter (not . isGoldOrb . snd) (mapOrbSpots m))) ]

forceList :: [a] -> ()
forceList = foldr seq ()

-- Kiểm tra va chạm song song bằng par / pseq (chia để trị trên danh sách vật thể)
parCheckCollisions :: (Collidable a, Collidable b) => [a] -> [b] -> [Bool]
parCheckCollisions [] _ = []
parCheckCollisions [x] targets = [any (isColliding x) targets]
parCheckCollisions xs targets =
  let (leftPart, rightPart) = splitAt (length xs `div` 2) xs
      leftRes  = parCheckCollisions leftPart targets
      rightRes = parCheckCollisions rightPart targets
  in forceList leftRes `par` (forceList rightRes `pseq` (leftRes ++ rightRes))

-- Ánh xạ song song trên các phần tử của Data.Map bằng spark
parMapValues :: (a -> b) -> Map.Map k a -> Map.Map k b
parMapValues f m =
  let ys = map f (Map.elems m)
      sparkAll = foldr (\y acc -> y `par` acc) () ys
  in sparkAll `pseq` (forceList ys `pseq` Map.fromDistinctAscList (zip (Map.keys m) ys))

pickBotHero :: [TankClass] -> TankClass -> Int -> TankClass
pickBotHero unlocked playerHero seed =
  let candidatePool = filter (/= playerHero) (if null unlocked then filter heroIsBase allHeroes else unlocked)
      pool = if null candidatePool then filter (/= playerHero) allHeroes else candidatePool
      idx = pseudoRandom (abs seed) `mod` length pool
  in pool !! idx

makeOfflineStateWithBot :: MapId -> TankClass -> TankClass -> Int -> GameState
makeOfflineStateWithBot !m !playerCls !botCls !seed =
  let !base      = (emptyGameState m) { gameTickCount = seed, worldSeed = seed + 1 }
      !named     = setPlayerNames [(botPlayerId, "BOT " ++ heroShortName botCls)] base
      roster     = [(1, playerCls), (botPlayerId, botCls)]
      !withTanks = foldl' (\ !st (pid, cls) -> spawnTank pid cls st) named roster
  in withTanks { powerUps = initialOrbs m, gameTickCount = 0, gsRoomPhase = PhaseInMatch matchDurationTicks }

makeOfflineState :: MapId -> TankClass -> Int -> GameState
makeOfflineState !m !playerCls !seed =
  makeOfflineStateWithBot m playerCls (pickBotHero [] playerCls seed) seed

makeLocal2PState :: MapId -> TankClass -> TankClass -> Int -> GameState
makeLocal2PState !m !p1Cls !p2Cls !seed =
  let !base      = (emptyGameState m) { gameTickCount = seed, worldSeed = seed + 1 }
      roster     = [(1, p1Cls), (2, p2Cls)]
      !withTanks = foldl' (\ !st (pid, cls) -> spawnTank pid cls st) base roster
  in withTanks { powerUps = initialOrbs m, gameTickCount = 0, gsRoomPhase = PhaseInMatch matchDurationTicks }

matchLeader :: GameState -> Maybe (Int, String, Int)
matchLeader !st =
  let standings = sortBy (comparing (Down . snd))
                    [ (pid, Map.findWithDefault 0 pid (score st)) | pid <- Map.keys (tanks st) ]
  in case standings of
       [(pid, s)] -> Just (pid, playerNameOf st pid, s)
       ((pid, s) : (_, s2) : _) | s > s2 -> Just (pid, playerNameOf st pid, s)
       _ -> Nothing

resolveMatchOutcome :: GameState -> Int -> (String, Int, Bool)
resolveMatchOutcome !st !myId =
  case matchLeader st of
    Just (pid, _, s) | pid == myId -> ("BẠN GIÀNH CHIẾN THẮNG! (" ++ show s ++ " ĐIỂM)", 500, True)
    Just (_, n, s)                 -> (n ++ " GIÀNH CHIẾN THẮNG! (" ++ show s ++ " ĐIỂM)", 200, False)
    Nothing                        -> ("HÒA! KHÔNG CÓ NGƯỜI THẮNG", 200, False)

resolveLocal2PMatchOutcome :: GameState -> (String, Int)
resolveLocal2PMatchOutcome !st =
  case matchLeader st of
    Just (pid, n, s) -> ((if pid == 1 then "P1 " else "P2 ") ++ n ++ " CHIẾN THẮNG! (" ++ show s ++ " ĐIỂM)", 300)
    Nothing          -> ("HÒA NHAU! (" ++ show (Map.findWithDefault 0 1 (score st)) ++ " ĐIỂM)", 300)
