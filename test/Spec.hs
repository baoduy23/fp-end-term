-- test/Spec.hs
-- Bộ kiểm thử tự động và thực nghiệm đánh giá lười / nghiêm ngặt

{-# LANGUAGE BangPatterns #-}

module Main where

import Types
import Hero
import GameLogic
import Protocol
import ParserCombinator
import AI
import Client.Account
import Client.Input (normalizeKeyChar, normalizeKeyCharWith, isImeComposedChar)
import Server.State (initCommandQueue, enqueueCommand, drainCommands, applyBatchCommands, addPlayerWithClass)

import qualified Data.Map.Strict as Map
import Data.List (isInfixOf, nub, sort)
import Data.Maybe (isJust)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (evaluate)
import Control.Monad (forM_, replicateM, replicateM_, when)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hSetEncoding, stdout, utf8)
import System.Timeout (timeout)
import System.Directory (removeFile, doesFileExist)
import Data.Time.Clock (getCurrentTime, diffUTCTime)

data TestCase = TestCase
  { tcName   :: !String
  , tcResult :: !Bool
  }

ioTest :: String -> IO Bool -> IO TestCase
ioTest name act = TestCase name <$> act

runTests :: String -> [TestCase] -> IO (Int, Int)
runTests groupName tests = do
  putStrLn $ "[Kiem thu] " ++ groupName
  results <- mapM runSingle tests
  let passed = length (filter id results)
      failed = length results - passed
  putStrLn $ "  => " ++ show passed ++ " dat, " ++ show failed ++ " that bai.\n"
  return (passed, failed)
  where
    runSingle (TestCase name ok) = do
      putStrLn $ (if ok then "  [PASS] " else "  [FAIL] ") ++ name
      return ok

dummyTank :: Tank
dummyTank = dummyTankOfClass VanguardCyan

dummyBullet :: Bullet
dummyBullet = Bullet
  { bulletPos    = Pos 10 10
  , bulletDir    = North
  , bulletOwner  = 1
  , bulletDamage = 60
  , bulletPierce = False
  , bulletRange  = normalShotRange
  , bulletSkill  = False
  }

dummyPowerUp :: PowerUp
dummyPowerUp = PowerUp
  { orbId   = 101
  , orbPos  = Pos 10 10
  , orbType = HealOrb
  }

arenaState :: GameState
arenaState = (emptyGameState MapFortress) { effectTiles = Map.empty, powerUps = [] }

findLane :: Int -> Position
findLane n = case lanes of
  (p : _) -> p
  []      -> Pos 1 1
  where
    m = currentMap arenaState
    clearRow x y = all (\dx -> not (isWall m (Pos (x + dx) y))) [0 .. n - 1]
    lanes = [ Pos x y | y <- [1 .. gridHeight - 2], x <- [1 .. gridWidth - n - 1]
                      , clearRow x y, clearRow x (y + 1), clearRow x (y - 1) ]

placeTank :: Int -> TankClass -> Position -> Direction -> GameState -> GameState
placeTank pid cls pos dir st =
  let st1 = spawnTank pid cls st
      fixT t = t { tankPos = pos, tankDir = dir, tankInvincibleTimer = 0, tankEnergy = 0
                 , tankFireCooldown = 0, tankMoveCooldown = 0, tankShield = 0 }
  in st1 { tanks = Map.adjust fixT pid (tanks st1) }

duel :: TankClass -> TankClass -> Int -> GameState
duel c1 c2 dist =
  let Pos x y = findLane (dist + 8)
  in placeTank 2 c2 (Pos (x + 1 + dist) y) West (placeTank 1 c1 (Pos (x + 1) y) East arenaState)

firstOr :: a -> [a] -> a
firstOr d xs = case xs of
  (x : _) -> x
  []      -> d

tankOf :: Int -> GameState -> Tank
tankOf pid st = Map.findWithDefault dummyTank pid (tanks st)

hpAndShield :: Tank -> Int
hpAndShield t = tankHealth t + tankShield t

withEnergy :: Int -> Int -> GameState -> GameState
withEnergy pid e st = st { tanks = Map.adjust (\t -> t { tankEnergy = e }) pid (tanks st) }

ultDamageAt :: TankClass -> TankClass -> Int -> Int
ultDamageAt c1 c2 dist =
  let st0    = withEnergy 1 maxEnergy (duel c1 c2 dist)
      st1    = updateGameState 1 CastUltimate st0
      later  = take 30 (iterate stepGameTick st1)
      lost s = hpAndShield (tankOf 2 st0) - hpAndShield (tankOf 2 s)
  in maximum (map lost later)

ultDamageTaken :: TankClass -> TankClass -> Int
ultDamageTaken c1 c2 = ultDamageAt c1 c2 2

oldShotDamage :: TankClass -> Int
oldShotDamage = specShotBase . heroSpec

timeToKill :: Double -> TankClass -> TankClass -> Double
timeToKill dmg attacker target =
  let shots = ceiling (fromIntegral (heroMaxHealth target) / dmg) :: Int
  in fromIntegral ((shots - 1) * heroBaseFireCooldown attacker) / 30.0

gridTests :: [TestCase]
gridTests =
  [ TestCase "isInsideGrid chap nhan goc tren va goc duoi"
      (isInsideGrid (Pos 0 0) && isInsideGrid (Pos (gridWidth - 1) (gridHeight - 1)))
  , TestCase "isInsideGrid tu choi toa do am va vuot gioi han"
      (not (isInsideGrid (Pos (-1) 0)) && not (isInsideGrid (Pos gridWidth 10)))
  , TestCase "movePos North tang y, South giam y"
      (movePos North (Pos 5 5) == Pos 5 6 && movePos South (Pos 5 5) == Pos 5 4)
  , TestCase "movePos East tang x va West giam x"
      (movePos East (Pos 5 5) == Pos 6 5 && movePos West (Pos 5 5) == Pos 4 5)
  , TestCase "chebyshevDist va manhattanDist"
      (chebyshevDist (Pos 0 0) (Pos 3 5) == 5 && manhattanDist (Pos 0 0) (Pos 3 5) == 8)
  , TestCase "oppositeDir doi chieu 4 huong"
      (map oppositeDir [North, South, East, West] == [South, North, West, East])
  , TestCase "rayCells dung lai truoc tuong va mep ban do"
      (all (\p -> isInsideGrid p && not (isWall MapFortress p)) (rayCells MapFortress East 40 (Pos 0 1))
       && length (rayCells MapFortress West 5 (Pos 2 1)) <= 2)
  ]

typeclassTests :: [TestCase]
typeclassTests =
  [ TestCase "Collidable Tank: va cham dung toa do khi con song"
      (collidesWith (dummyTank { tankPos = Pos 7 7 }) (Pos 7 7))
  , TestCase "Collidable Tank: khong va cham khi dang cho hoi sinh"
      (not (collidesWith (dummyTank { tankPos = Pos 7 7, tankRespawnTimer = 30 }) (Pos 7 7)))
  , TestCase "Collidable Bullet va PowerUp: va cham dung toa do"
      (collidesWith dummyBullet (Pos 10 10) && not (collidesWith dummyBullet (Pos 10 11))
       && collidesWith dummyPowerUp (Pos 10 10))
  , TestCase "isColliding da hinh giua Bullet va Tank"
      (isColliding dummyBullet (dummyTank { tankPos = Pos 10 10 }) &&
       not (isColliding dummyBullet (dummyTank { tankPos = Pos 12 10 })))
  , TestCase "GameEntity dinh danh va toa do dung thuc the"
      (entityId dummyTank == 0 && entityPos dummyBullet == Pos 10 10 && entityId dummyPowerUp == 101)
  , TestCase "HasHero: TankClass, Tank va HeroSpec cho cung mot ho so"
      (and [ specOf c == specOf (dummyTankOfClass c) && specOf (specOf c) == specOf c | c <- allHeroes ])
  , TestCase "HasHero: maxHealthOf va ultRangeOf dung chung cho moi instance"
      (and [ maxHealthOf c == maxHealthOf (dummyTankOfClass c) && ultRangeOf c == ultRangeOf (heroSpec c)
           | c <- allHeroes ])
  , TestCase "HeroSpec: specClass khop lop tuong, ten ngan khong trung nhau"
      (all (\c -> specClass (heroSpec c) == c) allHeroes
       && length (nub (map heroShortName allHeroes)) == 10)
  , TestCase "HeroSpec: 5 tuong co ban mien phi, 5 tuong shop gia tang dan"
      (length (filter heroIsBase allHeroes) == 5
       && let shop = map heroCost (filter (not . heroIsBase) allHeroes) in shop == sort shop)
  ]

shootScenario :: Int -> GameState
shootScenario victimHp =
  let st0 = duel TitanCrimson VanguardCyan 3
      st1 = st0 { tanks = Map.adjust (\t -> t { tankHealth = victimHp }) 2 (tanks st0) }
      st2 = updateGameState 1 Shoot st1
      hit s = Map.findWithDefault 0 1 (score s) > 0
  in firstOr st2 (filter hit (take 20 (iterate stepGameTick st2)))

combatTests :: [TestCase]
combatTests =
  [ TestCase "applyDamage: giap ao hap thu truoc khi tru mau"
      (let (hitT, killed) = applyDamage 30 (dummyTank { tankHealth = 100, tankShield = 50 })
       in tankHealth hitT == 100 && tankShield hitT == 20 && not killed)
  , TestCase "applyDamage: phan vuot giap ao tru vao mau"
      (let (hitT, killed) = applyDamage 50 (dummyTank { tankHealth = 100, tankShield = 20 })
       in tankHealth hitT == 70 && tankShield hitT == 0 && not killed)
  , TestCase "applyDamage: mau ve 0 thi bi ha guc, hoi sinh sau 3 giay"
      (let (hitT, killed) = applyDamage 50 (dummyTank { tankHealth = 40, tankShield = 0 })
       in tankHealth hitT == 0 && tankRespawnTimer hitT == 90 && killed)
  , TestCase "applyDamageWith pierce bo qua hoan toan giap ao"
      (let (hitT, _) = applyDamageWith True 50 (dummyTank { tankHealth = 200, tankShield = 150 })
       in tankHealth hitT == 150 && tankShield hitT == 150)
  , TestCase "applyHalfDamage: 2 phat 35.5 mat dung 71 mau (giu phan le 0.5)"
      (let t0 = dummyTank { tankHealth = 500, tankShield = 0 }
           (t1, _) = applyHalfDamage False 71 t0
           (t2, _) = applyHalfDamage False 71 t1
       in tankHealth t1 == 465 && tankDamageCarry t1 == 1 && tankHealth t2 == 429 && tankDamageCarry t2 == 0)
  , TestCase "applyHalfDamage: xe bat tu khong mat mau"
      (let t0 = dummyTank { tankHealth = 500, tankInvincibleTimer = 10 }
       in fst (applyHalfDamage False 71 t0) == t0)
  , TestCase "Ban trung: nguoi ban +15 diem va +9% nang luong, nguoi trung +5%"
      (let st = shootScenario 500
       in Map.findWithDefault 0 1 (score st) == hitScore
          && tankEnergy (tankOf 1 st) == hitEnergyGain
          && tankEnergy (tankOf 2 st) == damagedEnergyGain)
  , TestCase "Ha guc: +150 diem (cong ca diem trung dan), banner ghi ten that"
      (let st0 = setPlayerNames [(1, "HUY"), (2, "BNA")] (duel TitanCrimson VanguardCyan 3)
           st1 = st0 { tanks = Map.adjust (\t -> t { tankHealth = 5 }) 2 (tanks st0) }
           st2 = updateGameState 1 Shoot st1
           killed s = Map.findWithDefault 0 1 (score s) >= killScore
           st' = firstOr st2 (filter killed (take 20 (iterate stepGameTick st2)))
       in Map.findWithDefault 0 1 (score st') == killScore + hitScore
          && fmap (\(k, v, _) -> (k, v)) (gsLastKillBanner st') == Just ("HUY", "BNA")
          && gsLastKillIds st' == Just (1, 2))
  , TestCase "Khong hoi nang luong theo thoi gian (60 nhip dung yen)"
      (let st1 = iterate stepGameTick (duel VanguardCyan TitanCrimson 6) !! 60
       in tankEnergy (tankOf 1 st1) == 0)
  , TestCase "Dang hoi dan thi khong ban duoc phat thu hai"
      (let st1 = updateGameState 1 Shoot (duel VanguardCyan TitanCrimson 6)
           st2 = updateGameState 1 Shoot st1
       in length (bullets st1) == 1 && length (bullets st2) == 1)
  , TestCase "Dan thuong bien mat sau khi bay het tam normalShotRange"
      (let b = dummyBullet { bulletRange = 2 }
           st = arenaState { bullets = [b { bulletPos = Pos 1 1, bulletDir = East }] }
       in null (bullets (iterate stepGameTick st !! 4)))
  ]

balanceTests :: [TestCase]
balanceTests =
  [ TestCase "Dan thuong moi tuong = sat thuong goc + 7.5"
      (and [ heroBulletDamage c == fromIntegral (specShotBase (heroSpec c)) + 7.5 | c <- allHeroes ])
  , TestCase "Bang sat thuong dan thuong 10 tuong (35.5 ... 45.5)"
      (map heroBulletDamage allHeroes == [35.5, 39.5, 35.5, 37.5, 37.5, 39.5, 41.5, 39.5, 42.5, 45.5])
  , TestCase "Vien dan ban ra mang dung sat thuong (nua mau) cua tuong"
      (and [ case bullets (updateGameState 1 Shoot (duel c TitanCrimson 6)) of
               [b] -> bulletDamage b == heroShotDamageHalf c && bulletRange b == normalShotRange
                      && not (bulletSkill b)
               _   -> False
           | c <- allHeroes ])
  , TestCase "Trung dan thuc te: Vanguard mat dung 35 roi 71 mau sau 2 phat"
      (let st0 = duel VanguardCyan TitanCrimson 2
           fire s = iterate stepGameTick (updateGameState 1 Shoot s) !! 3
           s1 = fire st0
           s2 = fire (s1 { tanks = Map.adjust (\t -> t { tankFireCooldown = 0 }) 1 (tanks s1) })
           lost s = hpAndShield (tankOf 2 st0) - hpAndShield (tankOf 2 s)
       in lost s1 == 35 && lost s2 == 71)
  , TestCase "Tuyet ky moi tuong = sat thuong goc + 20"
      (and [ heroUltDamage c == specUltBase (heroSpec c) + 20 | c <- allHeroes ])
  , TestCase "Bang sat thuong tuyet ky (Vanguard 135 ... Eclipse 180)"
      (map heroUltDamage allHeroes == [135, 150, 115, 155, 135, 160, 140, 160, 170, 180])
  , TestCase "Tuyet ky khong vuot tran moi muc tieu (1/3 mau toi da + 20, toi da 185)"
      (and [ ultDamageOn (dummyTankOfClass a) (dummyTankOfClass b) <= ultDamageCap b
           | a <- allHeroes, b <- allHeroes ])
  , TestCase "Muc +20 khong bi tran an mat: ca 10 tuong gay du sat thuong len Vanguard"
      (and [ ultDamageOn (dummyTankOfClass a) (dummyTankOfClass VanguardCyan) == heroUltDamage a
           | a <- allHeroes ])
  , TestCase "Mo ta tuyet ky tren the tuong in dung so sat thuong moi"
      (and [ (show (heroUltDamage c) ++ " ST") `isInfixOf` fst (ultDescription c) | c <- allHeroes ])
  , TestCase "TTK: moi cap tuong ha guc nhanh hon ban cu, khong ai bi ha trong 1.5 giay"
      (and [ timeToKill (heroBulletDamage a) a b < timeToKill (fromIntegral (oldShotDamage a)) a b
             && timeToKill (heroBulletDamage a) a b >= 1.5
           | a <- allHeroes, b <- allHeroes ])
  ]

expectedReach :: TankClass -> Int
expectedReach c = case specUltShape (heroSpec c) of
  BlinkBurst k r -> k + r
  Radial r       -> r
  Beam r         -> r
  Cross r        -> r
  Shell r        -> r
  Volley r       -> r
  Pull r         -> r

hitboxTests :: [TestCase]
hitboxTests =
  [ TestCase "inRadius r dung bang vong tron ban kinh r + 0.5 o (vong ve tren man hinh)"
      (and [ inRadius r (Pos 0 0) (Pos dx dy) == (sqrt (fromIntegral (dx * dx + dy * dy)) <= (fromIntegral r + 0.5 :: Double))
           | r <- [1 .. 5], dx <- [-7 .. 7], dy <- [-7 .. 7] ])
  , TestCase "Ca 10 tuong: tuyet ky trung muc tieu dung sat ben canh (1 o)"
      (and [ ultDamageAt c TitanCrimson 1 > 0 | c <- allHeroes ])
  , TestCase "Ca 10 tuong: tuyet ky trung muc tieu o dung tam toi da"
      (and [ ultDamageAt c TitanCrimson (expectedReach c) > 0 | c <- allHeroes ])
  , TestCase "Ca 10 tuong: muc tieu xa hon tam 1 o thi khong trung"
      (and [ ultDamageAt c TitanCrimson (expectedReach c + 1) == 0 | c <- allHeroes ])
  , TestCase "ultRangeOf khop tam toi da tren mo ta tuyet ky"
      (and [ ultRangeOf c == expectedReach c | c <- allHeroes ])
  , TestCase "ultTouches khop logic: o nao bi danh dau thi xe dung o do bi trung"
      (and [ let st0 = withEnergy 1 maxEnergy (duel c TitanCrimson d)
                 caster = tankOf 1 st0
                 touched = ultTouches st0 caster (tankPos (tankOf 2 st0))
             in touched == (ultDamageAt c TitanCrimson d > 0)
           | c <- allHeroes, specIsInstant c, d <- [1 .. 8] ])
  , TestCase "Tia laze Solaris bi tuong chan (ultLineCells khong xuyen tuong)"
      (all (\p -> not (isWall MapFortress p)) (ultLineCells MapFortress SolarisGold East (Pos 0 1)))
  , TestCase "Mirage: chu thap 4 huong, moi huong toi da 3 o"
      (length (ultLineCells MapFortress MirageAmethyst East (Pos 1 1)) <= 12
       && Pos 4 1 `elem` ultLineCells MapFortress MirageAmethyst East (Pos 1 1))
  , TestCase "Striker: 3 lan hoa tien song song (giua + 2 ben)"
      (sort (volleyLanes East (Pos 5 5)) == sort [Pos 5 5, Pos 5 4, Pos 5 6])
  , TestCase "Titan / Striker ban ra dan tuyet ky that (bulletSkill, co tam bay)"
      (and [ let bs = filter bulletSkill (bullets (updateGameState 1 CastUltimate (withEnergy 1 maxEnergy (duel c TitanCrimson 9))))
             in not (null bs) && all (\b -> bulletRange b == expectedReach c - 1) bs
           | c <- [TitanCrimson, StrikerRuby] ])
  , TestCase "Phantom dung sat dich: dung lai truoc mat dich roi no"
      (let st0 = duel PhantomViolet TitanCrimson 1
       in blinkDestination st0 4 (tankOf 1 st0) == tankPos (tankOf 1 st0))
  , TestCase "Vanguard dong bang 1.8 giay, Tempest EMP 2 giay khoa ban"
      (let fr = updateGameState 1 CastUltimate (withEnergy 1 maxEnergy (duel VanguardCyan TitanCrimson 2))
           em = updateGameState 1 CastUltimate (withEnergy 1 maxEnergy (duel TempestAzure TitanCrimson 2))
           em2 = updateGameState 2 Shoot em
       in tankFreezeTimer (tankOf 2 fr) == freezeTicks && tankEmpTimer (tankOf 2 em) == empTicks
          && length (bullets em2) == length (bullets em))
  , TestCase "Chua day nang luong thi khong tung duoc tuyet ky"
      (let st0 = withEnergy 1 50 (duel SolarisGold TitanCrimson 2)
           st1 = updateGameState 1 CastUltimate st0
       in tankHealth (tankOf 2 st1) == tankHealth (tankOf 2 st0) && tankEnergy (tankOf 1 st1) == 50)
  ]
  where
    specIsInstant c = case specUltShape (heroSpec c) of
      Shell _  -> False
      Volley _ -> False
      _        -> True

roomCreated :: GameState
roomCreated = updateGameState 1 (CreateRoomCmd "1234" "HUY") (emptyGameState MapFortress)

roomJoined :: GameState
roomJoined = updateGameState 2 (JoinRoomCmd "1234" "BNA") roomCreated

roomPicking :: GameState
roomPicking = updateGameState 1 StartHeroPickCmd (updateGameState 2 (ToggleReadyCmd True) roomJoined)

hoverOf :: Int -> GameState -> Maybe TankClass
hoverOf pid st = fmap (\(_, h, _, _) -> h) (Map.lookup pid (gsLobbyPlayers st))

roomInMatch :: GameState
roomInMatch =
  let lockOf pid st = maybe st (\h -> updateGameState pid (LockHeroCmd h) st) (hoverOf pid st)
  in lockOf 2 (lockOf 1 roomPicking)

lobbyTests :: [TestCase]
lobbyTests =
  [ TestCase "CreateRoomCmd: nguoi tao la chu phong, phong o trang thai cho"
      (gsRoomHost roomCreated == 1 && gsRoomPhase roomCreated == PhaseWaitingLobby && gsRoomCode roomCreated == "1234")
  , TestCase "JoinRoomCmd: khach vao phong voi trang thai chua san sang"
      (fmap (\(n, _, _, r) -> (n, r)) (Map.lookup 2 (gsLobbyPlayers roomJoined)) == Just ("BNA", False))
  , TestCase "JoinRoomCmd sai ma phong bi tu choi"
      (not (Map.member 3 (gsLobbyPlayers (updateGameState 3 (JoinRoomCmd "9999" "X") roomJoined))))
  , TestCase "Chu phong khong vao tran khi khach chua san sang (thong bao ghi ten)"
      (let st = updateGameState 1 StartHeroPickCmd roomJoined
       in gsRoomPhase st == PhaseWaitingLobby
          && maybe False (\(m, _) -> "BNA" `isInfixOf` m && "chưa sẵn sàng" `isInfixOf` m) (gsNotice st))
  , TestCase "Mot minh chu phong khong vao tran duoc"
      (gsRoomPhase (updateGameState 1 StartHeroPickCmd roomCreated) == PhaseWaitingLobby)
  , TestCase "ToggleReadyCmd: bat san sang va thong bao toi ca phong"
      (let st = updateGameState 2 (ToggleReadyCmd True) roomJoined
       in fmap (\(_, _, _, r) -> r) (Map.lookup 2 (gsLobbyPlayers st)) == Just True && isJust (gsNotice st))
  , TestCase "Khach bam bat dau bi bo qua, chi chu phong moi bat dau duoc"
      (gsRoomPhase (updateGameState 2 StartHeroPickCmd (updateGameState 2 (ToggleReadyCmd True) roomJoined)) == PhaseWaitingLobby)
  , TestCase "Du nguoi va san sang: chuyen sang chon tuong 30 giay"
      (gsRoomPhase roomPicking == PhaseHeroPicking heroPickTicks)
  , TestCase "HoverHeroCmd: khong tro trung tuong nguoi khac dang giu"
      (case hoverOf 1 roomPicking of
         Just h1 -> let st = updateGameState 2 (HoverHeroCmd h1) roomPicking
                    in hoverOf 2 st == hoverOf 2 roomPicking && hoverOf 1 roomPicking /= hoverOf 2 roomPicking
         Nothing -> False)
  , TestCase "HoverHeroCmd: tro sang tuong con trong duoc cap nhat ngay"
      (case [ c | c <- allHeroes, Just c /= hoverOf 1 roomPicking, Just c /= hoverOf 2 roomPicking ] of
         (free : _) -> hoverOf 2 (updateGameState 2 (HoverHeroCmd free) roomPicking) == Just free
         []         -> False)
  , TestCase "Ca hai khoa tuong: vao tran 3 phut, xe mang ten tai khoan that"
      (gsRoomPhase roomInMatch == PhaseInMatch matchDurationTicks
       && map tankName (Map.elems (tanks roomInMatch)) == ["HUY", "BNA"])
  , TestCase "Dau hang khi con 2 nguoi: nguoi con lai thang, tran ket thuc"
      (let st = updateGameState 2 SurrenderCmd roomInMatch
       in gsRoomPhase st == PhaseMatchOver && gsLastQuit st == Just "BNA" && not (Map.member 2 (tanks st)))
  , TestCase "Rot mang (Disconnect) cung tinh la dau hang"
      (gsRoomPhase (updateGameState 2 Disconnect roomInMatch) == PhaseMatchOver)
  , TestCase "Tran 3 nguoi: 1 nguoi thoat, 2 nguoi con lai danh tiep"
      (let st0 = updateGameState 3 (ToggleReadyCmd True) (updateGameState 3 (JoinRoomCmd "1234" "LAN") (updateGameState 2 (ToggleReadyCmd True) roomJoined))
           st1 = updateGameState 1 StartHeroPickCmd st0
           lockAll s = foldl (\acc pid -> maybe acc (\h -> updateGameState pid (LockHeroCmd h) acc) (hoverOf pid acc)) s [1, 2, 3]
           st2 = lockAll st1
           st3 = updateGameState 3 SurrenderCmd st2
       in gsRoomPhase st2 == PhaseInMatch matchDurationTicks
          && gsRoomPhase st3 == PhaseInMatch matchDurationTicks
          && Map.size (tanks st3) == 2
          && maybe False (\(m, _) -> "TIẾP TỤC" `isInfixOf` m) (gsNotice st3))
  , TestCase "Het tran: dong bang moi hanh dong (ban, di chuyen)"
      (let st = updateGameState 2 SurrenderCmd roomInMatch
           st' = updateGameState 1 Shoot (updateGameState 1 (Move North) st)
       in tanks st' == tanks st && bullets st' == bullets st)
  , TestCase "Bot chon tuong ngau nhien trong kho tuong cua nguoi choi, khong trung"
      (let owned = [VanguardCyan, TitanCrimson, ViperEmerald]
           picks = [ pickBotHero owned VanguardCyan s | s <- [1 .. 60] ]
       in all (`elem` [TitanCrimson, ViperEmerald]) picks && TitanCrimson `elem` picks && ViperEmerald `elem` picks)
  , TestCase "Ten hien thi la ten tai khoan that"
      (playerNameOf roomInMatch 1 == "HUY" && playerNameOf roomInMatch 2 == "BNA")
  , TestCase "Ket qua tran: nguoi diem cao nhat thang, hoa thi khong ai thang"
      (let st = roomInMatch { score = Map.fromList [(1, 300), (2, 150)] }
           tie = roomInMatch { score = Map.fromList [(1, 150), (2, 150)] }
           (_, gold, won) = resolveMatchOutcome st 1
       in won && gold == 500 && matchLeader tie == Nothing)
  ]

roundTrip :: Command -> Bool
roundTrip cmd = decodeCommand (encodeCommand cmd) == Right cmd

parserTests :: [TestCase]
parserTests =
  [ TestCase "directionParser nhan dien 4 huong"
      (map (parse directionParser) ["NORTH", "SOUTH", "EAST", "WEST"] == map Right [North, South, East, West])
  , TestCase "tankClassParser doc dung ca 10 lop tuong"
      (all (\c -> parse tankClassParser (show c) == Right c) allHeroes)
  , TestCase "slashCommandParser nhan dien /help va /surrender"
      (case parse slashCommandParser "/help" of
         Right (SendChat _) -> parse slashCommandParser "/surrender" == Right SurrenderCmd
         _                  -> False)
  , TestCase "commandParser: MOVE, SHOOT, SELECT_CLASS, LOCK_HERO, HOVER_HERO, TOGGLE_READY"
      (parse commandParser "MOVE NORTH" == Right (Move North) &&
       parse commandParser "SHOOT" == Right Shoot &&
       parse commandParser "SELECT_CLASS StrikerRuby" == Right (SelectClass StrikerRuby) &&
       parse commandParser "LOCK_HERO TempestAzure" == Right (LockHeroCmd TempestAzure) &&
       parse commandParser "HOVER_HERO AegisOnyx" == Right (HoverHeroCmd AegisOnyx) &&
       parse commandParser "TOGGLE_READY 1" == Right (ToggleReadyCmd True))
  , TestCase "Ma hoa / giai ma khu hoi moi lenh (ten tieng Viet, chat co dau)"
      (all roundTrip
         [ HoverHeroCmd MirageAmethyst, ToggleReadyCmd True, ToggleReadyCmd False, SurrenderCmd
         , StartHeroPickCmd, LockHeroCmd EclipseObsidian, CreateRoomCmd "4821" "Huy"
         , JoinRoomCmd "4821" "Bảo Duy", SendChat "xin chào cả phòng", Move West, CastUltimate, Shoot ])
  , TestCase "GameState qua mang giu nguyen ten, thong bao, dan va chu phong"
      (let st = roomInMatch { gsNotice = Just ("Thông báo: thử", 30), bullets = [dummyBullet] }
       in decodeGameState (encodeGameState st) == Right st)
  , TestCase "Client ID ma hoa / giai ma khu hoi"
      (all (\i -> decodeClientId (encodeClientId i) == Just i) [1, 2, 42, 999])
  , TestCase "accountKVParser giai ma tep tai khoan day du"
      (let sample = unlines
             [ "callsign: COMMANDER", "password: secret", "gold: 5000"
             , "unlocked: [VanguardCyan,TitanCrimson]", "matches_played: 12"
             , "claimed_m1: True", "claimed_m2: True", "claimed_m3: False" ]
       in case parse accountKVParser sample of
            Right acc -> accCallsign acc == "COMMANDER" && accGold acc == 5000 && accMatchesPlayed acc == 12
                         && accClaimedM1 acc && not (accClaimedM3 acc)
            Left _    -> False)
  , TestCase "Lenh rac bi bao loi cu phap (Left ParseError)"
      (case parse commandParser "UNKNOWN_COMMAND_XYZ" of
         Left (ParseError _ _) -> True
         Right _               -> False)
  , TestCase "Vi tri loi tinh tu dau chuoi va la cho di duoc xa nhat"
      (case parse commandParser "MOVE NORHT" of
         Left err -> errPos err == 8 && "H" `isInfixOf` errMsg err
         Right _  -> False)
  , TestCase "Ky tu du thua sau lenh hop le bi bao loi"
      (case parse commandParser "SHOOT xyz" of
         Left err -> "du thua" `isInfixOf` errMsg err
         Right _  -> False)
  ]

lanTests :: [TestCase]
lanTests =
  [ TestCase "Goi tin LAN co IP + ma phong: khu hoi encode / decode"
      (decodeLanReply (encodeLanReply "192.168.1.15" (Just "4821")) == Right (LanReply (Just "192.168.1.15") (Just "4821")))
  , TestCase "Goi tin LAN khi chua co phong (NONE)"
      (decodeLanReply (encodeLanReply "10.0.0.7" Nothing) == Right (LanReply (Just "10.0.0.7") Nothing))
  , TestCase "Van doc duoc goi tin kieu cu TANK_SERVER_OK:<ma>"
      (decodeLanReply "TANK_SERVER_OK:1234" == Right (LanReply Nothing (Just "1234")))
  , TestCase "Bo qua ky tu xuong dong cuoi goi tin"
      (decodeLanReply "TANK_SERVER_OK 192.168.0.2 0042\r\n" == Right (LanReply (Just "192.168.0.2") (Just "0042")))
  , TestCase "Tu choi IP sai (octet > 255, thieu octet) va ma phong sai do dai"
      (all (\m -> case decodeLanReply m of { Left _ -> True; Right _ -> False })
         [ "TANK_SERVER_OK 300.1.1.1 1234", "TANK_SERVER_OK 10.0.0 1234"
         , "TANK_SERVER_OK 10.0.0.1 12345", "TANK_SERVER_OK 10.0.0.1 12a4", "HELLO", "" ])
  , TestCase "ipv4Parser chap nhan bien 0 va 255"
      (parse ipv4Parser "0.0.0.0" == Right "0.0.0.0" && parse ipv4Parser "255.255.255.255" == Right "255.255.255.255")
  , TestCase "pickLanRoom: lay IP may chu tu khai + ma phong de tu dien form"
      (pickLanRoom [("192.168.1.15", encodeLanReply "192.168.1.15" (Just "4821"))] == Just ("192.168.1.15", "4821"))
  , TestCase "pickLanRoom: bo qua may chu chua co phong va goi tin rac"
      (pickLanRoom [ ("10.0.0.2", encodeLanReply "10.0.0.2" Nothing), ("10.0.0.3", "rac")
                   , ("10.0.0.4", encodeLanReply "10.0.0.4" (Just "7777")) ] == Just ("10.0.0.4", "7777"))
  , TestCase "pickLanRoom: uu tien IP LAN that hon 127.0.0.1"
      (pickLanRoom [ ("127.0.0.1", encodeLanReply "127.0.0.1" (Just "1111"))
                   , ("192.168.1.9", encodeLanReply "192.168.1.9" (Just "1111")) ] == Just ("192.168.1.9", "1111"))
  , TestCase "pickLanRoom: goi tin kieu cu thi lay IP tu dia chi nguoi gui"
      (pickLanRoom [("192.168.1.20", "TANK_SERVER_OK:5555")] == Just ("192.168.1.20", "5555"))
  , TestCase "pickLanRoom: khong co phong nao thi tra ve Nothing"
      (pickLanRoom [] == Nothing && pickLanRoom [("1.2.3.4", encodeLanReply "1.2.3.4" Nothing)] == Nothing)
  ]

p1Keys, p2Keys :: String
p1Keys = "wasdgh"
p2Keys = "12"

imeTests :: [TestCase]
imeTests =
  [ TestCase "Chu hoa / Caps Lock ve chu thuong"
      (map normalizeKeyChar "WASDGH" == "wasdgh")
  , TestCase "Chu u mon (Telex bam W) va ban thay the 'u', '?' deu ve W"
      (all ((== 'w') . normalizeKeyChar) ['ư', 'Ư', '\xB0', 'u', '?'])
  , TestCase "Ban phim VIE: a trang / a mu la phim so 1 / 2 (phim cua P2)"
      (normalizeKeyChar 'ă' == '1' && normalizeKeyChar 'â' == '2')
  , TestCase "Telex sau Backspace: aa -> A, aw -> W, dd -> D"
      (normalizeKeyCharWith True 'â' == 'a' && normalizeKeyCharWith True 'ă' == 'w' && normalizeKeyCharWith True 'đ' == 'd')
  , TestCase "Telex sau Backspace: nguyen am dau sac (phim S) -> S"
      (all ((== 's') . normalizeKeyCharWith True) "áứé")
  , TestCase "Phim thuong G, H, 1, 2 giu nguyen"
      (map normalizeKeyChar "gh12" == "gh12")
  , TestCase "Phim P1 va P2 sau chuan hoa khong de len nhau"
      (all (`notElem` map normalizeKeyChar p2Keys) (map normalizeKeyChar p1Keys)
       && all (`notElem` map normalizeKeyChar p1Keys) (map normalizeKeyChar "ăâ12"))
  , TestCase "isImeComposedChar phan biet ky tu tu bo go va phim thuong"
      (isImeComposedChar 'ư' && not (isImeComposedChar 'w') && not (isImeComposedChar 'g'))
  ]

botDuel :: Direction -> Int -> Int -> GameState
botDuel botFacing dist energy =
  let Pos x y = findLane (dist + 8)
      st = placeTank 1 TitanCrimson (Pos (x + 1 + dist) y) West
             (placeTank botPlayerId VanguardCyan (Pos (x + 1) y) botFacing arenaState)
  in withEnergy botPlayerId energy st

botMove :: GameState -> Maybe Command
botMove st = fst (decideBotAction ((initBotMemory botPlayerId) { botChosenClass = VanguardCyan }) st)

aiTests :: [TestCase]
aiTests =
  [ TestCase "initBotMemory khoi tao dung ID"
      (botId (initBotMemory 999) == 999)
  , TestCase "BFS: di thang khi khong co vat can"
      (bfsNextDirection (Pos 5 5) (Pos 5 8) [] == Just North)
  , TestCase "BFS: vong qua chuong ngai vat"
      (let mDir = bfsNextDirection (Pos 5 5) (Pos 5 8) [Pos 5 6, Pos 5 7]
       in mDir == Just East || mDir == Just West)
  , TestCase "BFS: tra ve Nothing khi da dung tai dich"
      (bfsNextDirection (Pos 5 5) (Pos 5 5) [] == Nothing)
  , TestCase "BFS: bi chan kin 4 phia thi khong co duong"
      (bfsNextDirection (Pos 5 5) (Pos 9 9) [Pos 5 6, Pos 5 4, Pos 6 5, Pos 4 5] == Nothing)
  , TestCase "hasClearLineOfSight: thay dich tren cung hang trong"
      (let st = botDuel East 5 0
       in isJust (hasClearLineOfSight st (tankPos (tankOf botPlayerId st)) (tankPos (tankOf 1 st))))
  , TestCase "Ngam ban: dich trong tam va da nap dan thi ban ngay"
      (botMove (botDuel East 5 0) == Just Shoot)
  , TestCase "Ngam ban: dang nhin huong khac thi xoay nong ve phia dich"
      (botMove (botDuel North 5 0) == Just (Move East))
  , TestCase "Day nang luong va dich trong tam tuyet ky: tung tuyet ky"
      (botMove (botDuel East 2 maxEnergy) == Just CastUltimate)
  , TestCase "Day nang luong nhung dich ngoai tam: khong phi tuyet ky"
      (botMove (botDuel East 9 maxEnergy) /= Just CastUltimate)
  , TestCase "Tran da ket thuc thi Bot dung yen"
      (botMove ((botDuel East 5 0) { gsRoomPhase = PhaseMatchOver }) == Nothing)
  , TestCase "O xa dich: tiep can bang lenh di chuyen"
      (case botMove (botDuel North 14 0) of
         Just (Move _) -> True
         _             -> False)
  ]

parTests :: [TestCase]
parTests =
  [ TestCase "parCheckCollisions voi danh sach rong"
      (null (parCheckCollisions ([] :: [Position]) ([] :: [Position])))
  , TestCase "parCheckCollisions cho ket qua dung tren mau nho"
      (parCheckCollisions [Pos 2 2, Pos 3 3, Pos 10 10, Pos 15 15] [Pos 3 3, Pos 15 15] == [False, True, False, True])
  , TestCase "parCheckCollisions bang ban tuan tu tren 2000 vat the"
      (let xs = [ Pos (i `mod` gridWidth) (i `div` gridWidth `mod` gridHeight) | i <- [0 .. 1999 :: Int] ]
           ts = [ Pos x y | x <- [0, 3 .. gridWidth - 1], y <- [0, 4 .. gridHeight - 1] ]
       in parCheckCollisions xs ts == map (\x -> any (isColliding x) ts) xs)
  , TestCase "parMapValues cho ket qua giong het Map.map"
      (let m = Map.fromList [ (i, i * 3) | i <- [1 .. 500 :: Int] ]
       in parMapValues (\v -> v * v + 1) m == Map.map (\v -> v * v + 1) m)
  , TestCase "Mo phong 300 nhip voi buoc cap nhat xe song song: tran dau van chay dung (2 xe)"
      (let st0 = makeOfflineStateWithBot MapCanyon VanguardCyan TitanCrimson 11
           run = iterate stepGameTick st0 !! 300
       in run == iterate stepGameTick st0 !! 300 && Map.size (tanks run) == 2)
  ]

concurrencyTests :: IO [TestCase]
concurrencyTests = sequence
  [ ioTest "TChan: 8 luong cung day 200 lenh, rut ra du 1600 lenh, giu thu tu tung luong" $ do
      q <- initCommandQueue
      done <- newEmptyMVar
      forM_ [1 .. 8] $ \pid -> forkIO $ do
        forM_ [1 .. 200 :: Int] $ \k -> enqueueCommand q pid (SendChat (show k))
        putMVar done ()
      replicateM_ 8 (takeMVar done)
      cmds <- atomically (drainCommands q)
      let inOrder pid = [ m | (p, SendChat m) <- cmds, p == pid ] == map show [1 .. 200 :: Int]
      return (length cmds == 1600 && all inOrder [1 .. 8])
  , ioTest "TVar: 4 luong them nguoi choi dong thoi, khong mat cap nhat" $ do
      stVar <- newTVarIO arenaState
      done <- newEmptyMVar
      forM_ (zip [1 .. 4] allHeroes) $ \(pid, cls) -> forkIO (addPlayerWithClass stVar pid cls >> putMVar done ())
      replicateM_ 4 (takeMVar done)
      st <- readTVarIO stVar
      return (Map.keys (tanks st) == [1, 2, 3, 4])
  , ioTest "applyBatchCommands: ca goi lenh ap dung trong 1 giao dich" $ do
      stVar <- newTVarIO (duel VanguardCyan TitanCrimson 6)
      atomically (applyBatchCommands stVar [(1, Shoot), (2, Shoot)])
      st <- readTVarIO stVar
      return (length (bullets st) == 2)
  , ioTest "STM retry: luong cho bi chan den khi TVar du dieu kien" $ do
      flag <- newTVarIO (0 :: Int)
      result <- newEmptyMVar
      _ <- forkIO $ do
        v <- atomically (readTVar flag >>= \x -> if x < 3 then retry else return x)
        putMVar result v
      forM_ [1 .. 3 :: Int] $ \_ -> threadDelay 2000 >> atomically (modifyTVar' flag (+ 1))
      r <- timeout 2000000 (takeMVar result)
      return (r == Just 3)
  , ioTest "MVar: 10 luong x 1000 lan cong, tong dung 10000" $ do
      counter <- newMVar (0 :: Int)
      done <- newEmptyMVar
      forM_ [1 .. 10 :: Int] $ \_ -> forkIO $ do
        replicateM_ 1000 (modifyMVar_ counter (\ !n -> return (n + 1)))
        putMVar done ()
      replicateM_ 10 (takeMVar done)
      (== 10000) <$> readMVar counter
  , ioTest "TVar tach rieng moi luong: tong cuoi bang tong tung luong" $ do
      vars <- replicateM 6 (newTVarIO (0 :: Int))
      done <- newEmptyMVar
      forM_ vars $ \v -> forkIO $ do
        replicateM_ 500 (atomically (modifyTVar' v (+ 2)))
        putMVar done ()
      replicateM_ 6 (takeMVar done)
      total <- sum <$> mapM readTVarIO vars
      return (total == 6 * 500 * 2)
  ]

fileTests :: IO [TestCase]
fileTests = sequence
  [ return (TestCase "mkCallsign: nhan ten hop le, tu choi ten rong / qua dai / ky tu la"
      (mkCallsign "Huy_01" == Right "Huy_01"
       && all (\n -> either (const True) (const False) (mkCallsign n))
              ["", "abcdefghijklmnop", "a/b", "..\\x", "Bảo", "a b"]))
  , ioTest "Dang ky moi -> luu -> nap lai: tai khoan giong het (vang, tuong, so tran)" $ do
      let name = "zz_test_rt"
          path = "saves/account_" ++ name ++ ".dat"
      cleanup path
      r <- loginOrRegisterAccount name "pw"
      case r of
        Left _ -> return False
        Right acc -> do
          let acc' = (recordMatchPlayed acc) { accGold = 4321, accUnlocked = [VanguardCyan, EclipseObsidian] }
          saveAccount acc'
          back <- loadAccount name
          cleanup path
          return (back == acc')
  , ioTest "Sai mat khau: tra ve Left, khong mo tai khoan" $ do
      let name = "zz_test_pw"
          path = "saves/account_" ++ name ++ ".dat"
      cleanup path
      _ <- loginOrRegisterAccount name "dung"
      r <- loginOrRegisterAccount name "sai"
      cleanup path
      return (either ("khẩu" `isInfixOf`) (const False) r)
  , ioTest "Tep tai khoan hong: bao loi, khong ghi de tep cu" $ do
      let name = "zz_test_bad"
          path = "saves/account_" ++ name ++ ".dat"
          junk = "day khong phai tep tai khoan"
      _ <- loginOrRegisterAccount name "x"
      writeFile path junk
      r <- loginOrRegisterAccount name "x"
      after <- readFile path
      _ <- evaluate (length after)
      cleanup path
      return (either ("hỏng" `isInfixOf`) (const False) r && after == junk)
  ]
  where
    cleanup path = do
      e <- doesFileExist path
      when e (removeFile path)

lazyTests :: [TestCase]
lazyTests =
  [ TestCase "randomStream la danh sach vo han nhung lay 1000 phan tu van dung lai"
      (length (take 1000 (randomStream 42)) == 1000)
  , TestCase "randomStream: cung hat giong cho cung day so (ham thuan)"
      (take 50 (randomStream 7) == take 50 (randomStream 7) && take 50 (randomStream 7) /= take 50 (randomStream 8))
  , TestCase "foldl' va foldl cho cung ket qua tren 1..100000"
      (foldl' (+) 0 [1 .. 100000 :: Int] == foldl (+) 0 [1 .. 100000 :: Int])
  , TestCase "Tham so khong dung toi thi khong bi tinh (undefined van an toan)"
      (const True (undefined :: Int))
  , TestCase "Lay phan tu dau cua map tren danh sach vo han"
      (take 3 (map (* 2) [1 :: Int ..]) == [2, 4, 6])
  ]

runLazyVsStrictBenchmark :: IO ()
runLazyVsStrictBenchmark = do
  putStrLn "[Thuc nghiem] Lazy (foldl) va strict (foldl') khi cong don so nguyen"
  let timeIt act = do
        t0 <- getCurrentTime
        r  <- evaluate act
        t1 <- getCurrentTime
        return (r, round (realToFrac (diffUTCTime t1 t0) * 1000.0 :: Double) :: Int)
      report n label (r, ms) =
        putStrLn $ "  N = " ++ show n ++ " | " ++ label ++ " | ket qua " ++ show r ++ " | " ++ show ms ++ " ms"
  forM_ [5000000, 15000000 :: Int] $ \n -> do
    s <- timeIt (foldl' (+) (0 :: Int) [1 .. n])
    l <- timeIt (foldl (+) (0 :: Int) [1 .. n])
    report n "strict (foldl')" s
    report n "lazy   (foldl) " l
  putStrLn "  Khi bien dich voi -O, strictness analysis cua GHC bien foldl tren Int thanh vong lap"
  putStrLn "  strict nen hai ban gan bang nhau. Chay trong ghci (-O0) thi foldl tich luy chuoi thunk"
  putStrLn "  tren heap, cham va ton bo nho hon foldl'. Game dung foldl' va BangPatterns de khong"
  putStrLn "  phu thuoc vao toi uu hoa cua compiler.\n"

main :: IO ()
main = do
  hSetEncoding stdout utf8
  putStrLn "[Kiem thu] Tank Battle Arena\n"
  conc  <- concurrencyTests
  files <- fileTests
  results <- sequence
    [ runTests "1. Luoi & toa do" gridTests
    , runTests "2. Type class & va cham" typeclassTests
    , runTests "3. Chien dau, sat thuong & diem" combatTests
    , runTests "4. Can bang sat thuong (+7.5 / +20)" balanceTests
    , runTests "5. Hitbox tuyet ky 10 tuong" hitboxTests
    , runTests "6. Phong cho, chon tuong & dau hang" lobbyTests
    , runTests "7. Parser combinator & giao thuc TCP" parserTests
    , runTests "8. Do tim phong LAN (UDP)" lanTests
    , runTests "9. Bo go tieng Viet & phim P1 / P2" imeTests
    , runTests "10. AI: BFS & ngam ban" aiTests
    , runTests "11. Song song du lieu (par / pseq)" parTests
    , runTests "12. Dong thoi (STM, TChan, MVar)" conc
    , runTests "13. Tep tai khoan (luu / nap / loi)" files
    , runTests "14. Lazy evaluation" lazyTests
    ]
  let totalPassed = sum (map fst results)
      totalFailed = sum (map snd results)
  runLazyVsStrictBenchmark
  putStrLn $ "[Ket qua] " ++ show totalPassed ++ " / " ++ show (totalPassed + totalFailed) ++ " test case dat."
  when (totalFailed > 0) (putStrLn "Co test case khong dat." >> exitFailure)
  putStrLn "Tat ca test case deu dat."
  exitSuccess
