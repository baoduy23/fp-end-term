-- Types.hs
-- Các kiểu dữ liệu đại số (ADT) và lớp kiểu cốt lõi của trò chơi

{-# LANGUAGE BangPatterns #-}

module Types where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

data Direction = North | South | East | West
  deriving (Eq, Show, Read)

-- Tọa độ trên lưới 32x22; thực thi Ord để lưu tường trong Data.Set
data Position = Pos
  { posX :: !Int
  , posY :: !Int
  } deriving (Eq, Ord, Show, Read)

data MapId
  = MapFortress
  | MapCanyon
  | MapCrossfire
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- 10 lớp tướng: 5 tướng cơ bản mở sẵn, 5 tướng mở khóa trong cửa hàng
data TankClass
  = VanguardCyan
  | TitanCrimson
  | ViperEmerald
  | SolarisGold
  | PhantomViolet
  | StrikerRuby
  | AegisOnyx
  | TempestAzure
  | MirageAmethyst
  | EclipseObsidian
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

allHeroes :: [TankClass]
allHeroes = [minBound .. maxBound]

data PlayerAccount = PlayerAccount
  { accCallsign      :: !String
  , accPassword      :: !String
  , accGold          :: !Int
  , accUnlocked      :: ![TankClass]
  , accMatchesPlayed :: !Int
  , accClaimedM1     :: !Bool
  , accClaimedM2     :: !Bool
  , accClaimedM3     :: !Bool
  } deriving (Eq, Show, Read)

accUnlockedHeroes :: PlayerAccount -> [TankClass]
accUnlockedHeroes = accUnlocked

-- Tài khoản mặc định ban đầu: 2000 vàng và 5 tướng cơ bản
defaultAccount :: String -> PlayerAccount
defaultAccount !name = defaultAccountWithPass name "123456"

defaultAccountWithPass :: String -> String -> PlayerAccount
defaultAccountWithPass !name !pass = PlayerAccount
  { accCallsign      = name
  , accPassword      = if null pass then "123456" else pass
  , accGold          = 2000
  , accUnlocked      = [VanguardCyan, TitanCrimson, ViperEmerald, SolarisGold, PhantomViolet]
  , accMatchesPlayed = 0
  , accClaimedM1     = False
  , accClaimedM2     = False
  , accClaimedM3     = False
  }

-- Trạng thái xe tăng trên sân; các bộ đếm thời gian tính theo tick (30 ticks = 1 giây)
data Tank = Tank
  { tankId              :: !Int
  , tankPos             :: !Position
  , tankDir             :: !Direction
  , tankHealth          :: !Int
  , tankEnergy          :: !Int
  , tankFireCooldown    :: !Int
  , tankMoveCooldown    :: !Int
  , tankRespawnTimer    :: !Int
  , tankInvincibleTimer :: !Int
  , tankClass           :: !TankClass
  , tankShield          :: !Int
  , tankBurnTimer       :: !Int
  , tankSilenceTimer    :: !Int
  , tankStarTimer       :: !Int
  , tankPierceTimer     :: !Int
  , tankStealthTimer    :: !Int
  , tankFreezeTimer     :: !Int
  , tankEmpTimer        :: !Int
  , tankHexShieldTimer  :: !Int
  , tankAcidTimer       :: !Int
  , tankBlackHoleTimer  :: !Int
  , tankLaserTimer      :: !Int
  , tankLightningTimer  :: !Int
  , tankName            :: !String
  , tankArmorBreakTimer :: !Int
  , tankPoisonTimer     :: !Int
  , tankBlindTimer      :: !Int
  , tankHasteTimer      :: !Int
  , tankReflectTimer    :: !Int
  , tankWeakenTimer     :: !Int
  , tankDamageCarry     :: !Int        -- Phần lẻ 0.5 máu còn dư từ phát đạn trước (0 hoặc 1)
  } deriving (Eq, Show, Read)

-- Đạn đang bay; bulletDamage lưu theo đơn vị nửa điểm máu (71 = 35.5 máu)
data Bullet = Bullet
  { bulletPos    :: !Position
  , bulletDir    :: !Direction
  , bulletOwner  :: !Int
  , bulletDamage :: !Int
  , bulletPierce :: !Bool
  , bulletRange  :: !Int
  , bulletSkill  :: !Bool
  } deriving (Eq, Show, Read)

normalShotRange :: Int
normalShotRange = 40

data OrbType
  = HealOrb
  | EnergyOrb
  | OverchargeOrb
  | ShieldOrb
  | StarOrb
  | PierceOrb
  deriving (Eq, Show, Read)

-- Ngọc vàng là bùa lợi hiếm, chỉ xuất hiện tối đa 1 viên trên sân tại một thời điểm
isGoldOrb :: OrbType -> Bool
isGoldOrb o = o == OverchargeOrb || o == StarOrb

data TileEffect
  = MudTile
  | SilenceTile
  | FireTile
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data PowerUp = PowerUp
  { orbId   :: !Int
  , orbPos  :: !Position
  , orbType :: !OrbType
  } deriving (Eq, Show, Read)

data RoomPhase
  = PhaseNoRoom
  | PhaseWaitingLobby
  | PhaseHeroPicking !Int
  | PhaseInMatch !Int
  | PhaseMatchOver
  deriving (Eq, Show, Read)

-- Trạng thái toàn cục của phòng và trận đấu, lưu trong TVar trên máy chủ
data GameState = GameState
  { tanks            :: !(Map.Map Int Tank)
  , bullets          :: ![Bullet]
  , score            :: !(Map.Map Int Int)
  , powerUps         :: ![PowerUp]
  , chatLog          :: ![(String, String)]
  , gameTickCount    :: !Int
  , currentMap       :: !MapId
  , effectTiles      :: !(Map.Map Position TileEffect)
  , worldSeed        :: !Int
  , gsRoomCode       :: !String
  , gsRoomPhase      :: !RoomPhase
  , gsLobbyPlayers   :: !(Map.Map Int (String, TankClass, Maybe TankClass, Bool))
  , gsLastKillBanner :: !(Maybe (String, String, Int))
  , gsPlayerNames    :: !(Map.Map Int String)
  , gsRoomHost       :: !Int
  , gsNotice         :: !(Maybe (String, Int))
  , gsLastQuit       :: !(Maybe String)
  , gsLastKillIds    :: !(Maybe (Int, Int))
  } deriving (Eq, Show, Read)

emptyGameState :: MapId -> GameState
emptyGameState !m =
  GameState Map.empty [] Map.empty [] [] 0 m Map.empty 0 "" PhaseNoRoom Map.empty Nothing Map.empty
            0 Nothing Nothing Nothing

-- Thời lượng trận đấu (3 phút = 5400 ticks) và chọn tướng (30 giây = 900 ticks) ở 30 FPS
matchDurationTicks :: Int
matchDurationTicks = 5400

heroPickTicks :: Int
heroPickTicks = 900

bannerTicks :: Int
bannerTicks = 90

noticeTicks :: Int
noticeTicks = 120

botPlayerId :: Int
botPlayerId = 999

botPlayerId2 :: Int
botPlayerId2 = 998

isBotPlayer :: Int -> Bool
isBotPlayer !i = i == botPlayerId || i == botPlayerId2

data Command
  = Move !Direction
  | Shoot
  | CastUltimate
  | SendChat !String
  | SelectClass !TankClass
  | Disconnect
  | CreateRoom
  | JoinRoom !String
  | StartMatch
  | CreateRoomCmd !String !String
  | JoinRoomCmd !String !String
  | StartHeroPickCmd
  | LockHeroCmd !TankClass
  | HoverHeroCmd !TankClass
  | ToggleReadyCmd !Bool
  | SurrenderCmd
  deriving (Eq, Show, Read)

-- Kích thước lưới bản đồ (32x22) và các hằng số cân bằng

gridWidth :: Int
gridWidth = 32

gridHeight :: Int
gridHeight = 22

maxHealth :: Int
maxHealth = 600

maxEnergy :: Int
maxEnergy = 100

maxShield :: Int
maxShield = 150

orbShieldBonus :: Int
orbShieldBonus = 20

shieldOrbAmount :: Int
shieldOrbAmount = 100

mudMoveCooldown :: Int
mudMoveCooldown = 6

fireBurnDuration :: Int
fireBurnDuration = 60

fireTickInterval :: Int
fireTickInterval = 6

fireTickDamage :: Int
fireTickDamage = 5

silenceDuration :: Int
silenceDuration = 45

-- Chu kỳ các sự kiện thế giới theo tick (30 ticks = 1 giây)

tileShuffleInterval :: Int
tileShuffleInterval = 900

starInterval :: Int
starInterval = 900

starOffset :: Int
starOffset = 450

starDuration :: Int
starDuration = 150

pierceSpawnInterval :: Int
pierceSpawnInterval = 600

pierceDuration :: Int
pierceDuration = 300

orbSpawnInterval :: Int
orbSpawnInterval = 120

goldOrbChance :: Int
goldOrbChance = 8

-- Điểm thưởng và mức tích nạp năng lượng

killScore :: Int
killScore = 150

hitScore :: Int
hitScore = 15

orbScore :: OrbType -> Int
orbScore o = if isGoldOrb o then 2 else 1

hitEnergyGain :: Int
hitEnergyGain = 9

damagedEnergyGain :: Int
damagedEnergyGain = 5

energyOrbGain :: Int
energyOrbGain = 15

-- Thời lượng hiệu ứng của các tuyệt kỹ (ticks)

freezeTicks, armorBreakTicks, poisonTicks, blindTicks, phantomStealthTicks :: Int
freezeTicks         = 54
armorBreakTicks     = 90
poisonTicks         = 90
blindTicks          = 75
phantomStealthTicks = 60

hasteTicks, reflectTicks, empTicks, mirageTicks, weakenTicks :: Int
hasteTicks  = 90
reflectTicks = 90
empTicks    = 60
mirageTicks = 90
weakenTicks = 90

poisonTickInterval, poisonTickDamage :: Int
poisonTickInterval = 15
poisonTickDamage   = 10

aegisShieldGain :: Int
aegisShieldGain = 160

-- Hệ thống 3 bản đồ chiến trường

mapName :: MapId -> String
mapName m = case m of
  MapFortress  -> "PHÁO ĐÀI"
  MapCanyon    -> "HẺM NÚI"
  MapCrossfire -> "GIAO TUYẾN"

mapCount :: Int
mapCount = fromEnum (maxBound :: MapId) + 1

-- Dịch sang bản đồ kế tiếp theo modulo; với 1 <= k < mapCount luôn cho bản đồ khác hiện tại
cycleMap :: Int -> MapId -> MapId
cycleMap !k !m = toEnum ((fromEnum m + k) `mod` mapCount)

hLine :: Int -> Int -> Int -> [Position]
hLine !y !x1 !x2 = [ Pos x y | x <- [x1 .. x2] ]

vLine :: Int -> Int -> Int -> [Position]
vLine !x !y1 !y2 = [ Pos x y | y <- [y1 .. y2] ]

block :: Int -> Int -> Int -> Int -> [Position]
block !x !y !w !h = [ Pos (x + i) (y + j) | i <- [0 .. w - 1], j <- [0 .. h - 1] ]

-- Đối xứng 4 góc qua hai trục giữa bàn cờ
mirror4 :: [Position] -> [Position]
mirror4 ps = Set.toList $ Set.fromList $ concat
  [ ps
  , map flipX ps
  , map flipY ps
  , map (flipX . flipY) ps
  ]
  where
    flipX (Pos x y) = Pos (gridWidth - 1 - x) y
    flipY (Pos x y) = Pos x (gridHeight - 1 - y)

mapWalls :: MapId -> [Position]
mapWalls m = case m of
  MapFortress ->
    [ Pos 5 17, Pos 5 16, Pos 5 15, Pos 6 17, Pos 7 17
    , Pos 8 14, Pos 9 14, Pos 9 15
    , Pos 26 17, Pos 26 16, Pos 26 15, Pos 25 17, Pos 24 17
    , Pos 23 14, Pos 22 14, Pos 22 15
    , Pos 5 4, Pos 5 5, Pos 5 6, Pos 6 4, Pos 7 4
    , Pos 8 7, Pos 9 7, Pos 9 6
    , Pos 26 4, Pos 26 5, Pos 26 6, Pos 25 4, Pos 24 4
    , Pos 23 7, Pos 22 7, Pos 22 6
    , Pos 14 10, Pos 14 11, Pos 15 10, Pos 15 11
    , Pos 16 10, Pos 16 11, Pos 17 10, Pos 17 11
    , Pos 15 16, Pos 16 16, Pos 15 17, Pos 16 17
    , Pos 15 4,  Pos 16 4,  Pos 15 5,  Pos 16 5
    , Pos 10 10, Pos 10 11, Pos 21 10, Pos 21 11
    ]

  MapCanyon -> mirror4 $ concat
    [ hLine 6 5 10
    , hLine 6 13 14
    , vLine 5 3 5
    , block 9 9 2 2
    , vLine 14 9 10
    , block 2 10 2 1
    ]

  MapCrossfire -> mirror4 $ concat
    [ vLine 15 3 7
    , hLine 10 5 11
    , block 6 4 2 2
    , [Pos 8 4, Pos 6 6]
    , block 11 14 2 1
    , [Pos 3 8]
    ]

mapWallSet :: MapId -> Set.Set Position
mapWallSet m = case m of
  MapFortress  -> fortressWallSet
  MapCanyon    -> canyonWallSet
  MapCrossfire -> crossfireWallSet

-- Các tập hợp tường tính sẵn một lần (CAF) để tra cứu O(log n)
fortressWallSet, canyonWallSet, crossfireWallSet :: Set.Set Position
fortressWallSet  = Set.fromList (mapWalls MapFortress)
canyonWallSet    = Set.fromList (mapWalls MapCanyon)
crossfireWallSet = Set.fromList (mapWalls MapCrossfire)

mapSpawnCorners :: MapId -> [Position]
mapSpawnCorners _ = [Pos 2 2, Pos 29 2, Pos 2 19, Pos 29 19]

tileName :: TileEffect -> String
tileName t = case t of
  MudTile     -> "XÌNH LẦY"
  SilenceTile -> "PHONG ẤN"
  FireTile    -> "DUNG NHAM"

tileDescription :: TileEffect -> String
tileDescription t = case t of
  MudTile     -> "CHẬM X3"
  SilenceTile -> "KHÓA TUYỆT KỸ"
  FireTile    -> "-25 MÁU/GIÂY"

tileAt :: Map.Map Position TileEffect -> Position -> Maybe TileEffect
tileAt !tiles !p = Map.lookup p tiles

mapOrbSpots :: MapId -> [(Position, OrbType)]
mapOrbSpots m = filter (\(p, _) -> not (Set.member p (mapWallSet m))) candidates
  where
    candidates =
      [ (Pos 15 11, OverchargeOrb), (Pos 16 10, OverchargeOrb)
      , (Pos 10 5,  HealOrb),       (Pos 21 5,  HealOrb)
      , (Pos 10 16, EnergyOrb),     (Pos 21 16, EnergyOrb)
      , (Pos 4 11,  HealOrb),       (Pos 27 11, EnergyOrb)
      , (Pos 15 20, OverchargeOrb), (Pos 16 1,  OverchargeOrb)
      , (Pos 12 8,  HealOrb),       (Pos 19 13, EnergyOrb)
      , (Pos 7 11,  ShieldOrb),     (Pos 24 10, ShieldOrb)
      ]

-- Lớp kiểu biểu diễn đối tượng có tọa độ và kiểm tra va chạm trên lưới
class Collidable a where
  collidesWith :: a -> Position -> Bool
  objectPos    :: a -> Position

instance Collidable Tank where
  collidesWith !t !p = tankPos t == p && tankRespawnTimer t <= 0
  objectPos = tankPos

instance Collidable Bullet where
  collidesWith !b !p = bulletPos b == p
  objectPos = bulletPos

instance Collidable PowerUp where
  collidesWith !o !p = orbPos o == p
  objectPos = orbPos

instance Collidable Position where
  collidesWith !p1 !p2 = p1 == p2
  objectPos p = p

-- Lớp kiểu truy xuất định danh và vị trí của thực thể chiến trường
class GameEntity a where
  entityId  :: a -> Int
  entityPos :: a -> Position

instance GameEntity Tank where
  entityId = tankId
  entityPos = tankPos

instance GameEntity Bullet where
  entityId = bulletOwner
  entityPos = bulletPos

instance GameEntity PowerUp where
  entityId = orbId
  entityPos = orbPos

isColliding :: (Collidable a, Collidable b) => a -> b -> Bool
isColliding !a !b = collidesWith a (objectPos b)
