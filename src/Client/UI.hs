-- Client/UI.hs
-- Giao diện đồ họa Gloss: font vector tự vẽ và các màn hình của trò chơi

{-# LANGUAGE BangPatterns #-}

module Client.UI
  ( AppScreen(..)
  , InputField(..)
  , Particle(..)
  , Shockwave(..)
  , DustParticle(..)
  , KillNotice(..)
  , MatchResult(..)
  , PickCursor(..)
  , playerSlotColor
  , moveHeroCursor
  , matchSecondsLeft
  , GameMode(..)
  , nextClass
  , prevClass
  , heroClassInfo
  , HeroInfo(..)
  , heroShortName
  , makeOfflineState
  , makeLocal2PState
  , drawLoginScreen
  , drawLobbyScreen
  , drawShopScreen
  , drawEventScreen
  , drawRoomScreen
  , drawJoinCodeModal
  , drawPickHeroScreen
  , drawLocal2PPickScreen
  , drawSkillDock
  , drawSkillDockCustom
  , draw3DPlayingScene
  , drawHeroTank3D
  , drawVietnameseText
  , drawTextCenter
  , drawTextRight
  , textWidth
  , LoginButton(..)
  , loginButtonRect
  , loginHitTest
  , LobbyButton(..)
  , lobbyButtonRect
  , lobbyHitTest
  , ShopButton(..)
  , shopHeroes
  , shopHitTest
  , EventButton(..)
  , eventButtonRect
  , eventHitTest
  , RoomButton(..)
  , roomButtonRect
  , roomHitTest
  , JoinCodeButton(..)
  , joinCodeButtonRect
  , joinCodeHitTest
  , PickHeroButton(..)
  , pickHeroHitTest
  , localPickHitTest
  , surrenderHitTest
  , SurrenderModalButton(..)
  , surrenderModalHitTest
  , drawSurrenderModal
  , toScreenPos
  , cellSize
  , targetAngle
  ) where

import Types
import Hero
import Graphics.Gloss
import qualified Data.Map.Strict as Map
import GameLogic (makeOfflineState, makeLocal2PState, playerNameOf, ultLineCells, ultTouches, fireCooldownOf)
import Data.Char (toUpper)
import Data.List (find, sortBy)
import Data.Ord (comparing, Down(..))

cellSize :: Float
cellSize = 26.0

arenaHalfW :: Float
arenaHalfW = fromIntegral gridWidth * cellSize / 2.0

arenaHalfH :: Float
arenaHalfH = fromIntegral gridHeight * cellSize / 2.0

-- Chuyển đổi tọa độ ô lưới (0..31, 0..21) sang hệ tọa độ màn hình Gloss
toScreenPos :: Position -> (Float, Float)
toScreenPos (Pos !x !y) =
  ( fromIntegral x * cellSize - arenaHalfW + cellSize / 2.0
  , fromIntegral y * cellSize - arenaHalfH + cellSize / 2.0
  )

data AppScreen
  = LoginScreen
  | LobbyScreen
  | ShopScreen
  | EventScreen
  | RoomScreen !String
  | JoinCodeScreen
  | PickHeroScreen !Float
  | ConnectingScreen !String
  | PlayingScreen
  deriving (Eq, Show)

data InputField
  = FieldLoginUser
  | FieldLoginPass
  | FieldServerHost
  | FieldServerPort
  | FieldJoinCode
  | FieldChat
  | FieldCallsign
  deriving (Eq, Show)

nextClass :: TankClass -> TankClass
nextClass EclipseObsidian = VanguardCyan
nextClass c               = succ c

prevClass :: TankClass -> TankClass
prevClass VanguardCyan = EclipseObsidian
prevClass c            = pred c

data HeroInfo = HeroInfo
  { heroTitle     :: !String
  , heroRole      :: !String
  , heroUltName   :: !String
  , heroUltDesc1  :: !String
  , heroUltDesc2  :: !String
  , heroShotText  :: !String
  , heroStatHp    :: !Float
  , heroStatSpd   :: !Float
  , heroStatAtk   :: !Float
  , heroStatDef   :: !Float
  , heroPrimaryCol:: !Color
  , heroAccentCol :: !Color
  , heroGlowCol   :: !Color
  }

-- Tổng hợp thông tin hiển thị của tướng từ hồ sơ gốc HeroSpec
heroClassInfo :: TankClass -> HeroInfo
heroClassInfo cls =
  let sp = heroSpec cls
      (desc1, desc2) = ultDescription cls
      (spd, atk, def, primaryC, accentC, glowC) = heroLook cls
  in HeroInfo
       { heroTitle      = specTitle sp
       , heroRole       = specRole sp
       , heroUltName    = specUltName sp
       , heroUltDesc1   = desc1
       , heroUltDesc2   = desc2
       , heroShotText   = "Đạn thường: " ++ showHalf (heroShotDamageHalf cls) ++ " ST / phát"
       , heroStatHp     = fromIntegral (specMaxHp sp) / 700.0
       , heroStatSpd    = spd
       , heroStatAtk    = atk
       , heroStatDef    = def
       , heroPrimaryCol = primaryC
       , heroAccentCol  = accentC
       , heroGlowCol    = glowC
       }

heroLook :: TankClass -> (Float, Float, Float, Color, Color, Color)
heroLook cls = case cls of
  VanguardCyan    -> (0.80, 0.70, 0.80, makeColorI 15 150 215 255, makeColorI 170 240 255 255, makeColorI 0 230 255 200)
  TitanCrimson    -> (0.55, 0.95, 0.90, makeColorI 200 35 45 255, makeColorI 255 160 160 255, makeColorI 255 45 45 200)
  ViperEmerald    -> (0.90, 0.65, 0.85, makeColorI 25 165 75 255, makeColorI 180 255 190 255, makeColorI 40 255 100 200)
  SolarisGold     -> (0.75, 0.85, 0.65, makeColorI 215 155 15 255, makeColorI 255 240 165 255, makeColorI 255 210 20 200)
  PhantomViolet   -> (1.00, 0.85, 0.60, makeColorI 145 45 205 255, makeColorI 230 180 255 255, makeColorI 190 55 255 200)
  StrikerRuby     -> (0.85, 0.90, 0.75, makeColorI 245 45 75 255, makeColorI 255 160 180 255, makeColorI 255 40 80 200)
  AegisOnyx       -> (0.60, 0.75, 1.00, makeColorI 175 195 235 255, makeColorI 220 235 255 255, makeColorI 160 190 230 200)
  TempestAzure    -> (0.95, 0.95, 0.70, makeColorI 0 215 255 255, makeColorI 180 240 255 255, makeColorI 0 230 255 200)
  MirageAmethyst  -> (0.95, 1.00, 0.70, makeColorI 210 90 255 255, makeColorI 245 195 255 255, makeColorI 220 110 255 200)
  EclipseObsidian -> (0.90, 1.00, 0.90, makeColor 0.85 0.45 1.0 1.0, makeColorI 255 160 80 255, makeColorI 255 120 40 200)

data Particle = Particle
  { partX     :: !Float
  , partY     :: !Float
  , partVX    :: !Float
  , partVY    :: !Float
  , partLife  :: !Float
  , partDecay :: !Float
  , partSize  :: !Float
  , partColor :: !Color
  } deriving (Eq, Show)

data Shockwave = Shockwave
  { shockX      :: !Float
  , shockY      :: !Float
  , shockRadius :: !Float
  , shockMaxR   :: !Float
  , shockLife   :: !Float
  , shockColor  :: !Color
  } deriving (Eq, Show)

data GameMode = ModeMatch3Min | ModeEndless
  deriving (Eq, Show)

data DustParticle = DustParticle
  { dustX       :: !Float
  , dustY       :: !Float
  , dustLife    :: !Float
  , dustMaxLife :: !Float
  , dustSize    :: !Float
  } deriving (Eq, Show)

data KillNotice = KillNotice
  { knKiller :: !String
  , knVictim :: !String
  , knTimer  :: !Float
  , knIsMe   :: !Bool
  } deriving (Eq, Show)

data Tone = TNothing | TSac | THuyen | THoi | TNga | TNang deriving (Eq, Show)
data VMod = VNothing | VMu | VTrang | VMoc | VBarD deriving (Eq, Show)

data VnChar = VnChar
  { vcBase :: !Char
  , vcMod  :: !VMod
  , vcTone :: !Tone
  } deriving (Eq, Show)

-- Phân tách ký tự Unicode tiếng Việt thành chữ cái gốc, dấu mũ/móc và dấu thanh
parseVnChar :: Char -> VnChar
parseVnChar c = case c of
  'á' -> VnChar 'a' VNothing TSac; 'à' -> VnChar 'a' VNothing THuyen; 'ả' -> VnChar 'a' VNothing THoi
  'ã' -> VnChar 'a' VNothing TNga; 'ạ' -> VnChar 'a' VNothing TNang
  'ă' -> VnChar 'a' VTrang TNothing; 'ắ' -> VnChar 'a' VTrang TSac; 'ằ' -> VnChar 'a' VTrang THuyen
  'ẳ' -> VnChar 'a' VTrang THoi;   'ẵ' -> VnChar 'a' VTrang TNga; 'ặ' -> VnChar 'a' VTrang TNang
  'â' -> VnChar 'a' VMu TNothing;   'ấ' -> VnChar 'a' VMu TSac;   'ầ' -> VnChar 'a' VMu THuyen
  'ẩ' -> VnChar 'a' VMu THoi;     'ẫ' -> VnChar 'a' VMu TNga;   'ậ' -> VnChar 'a' VMu TNang
  'Á' -> VnChar 'A' VNothing TSac; 'À' -> VnChar 'A' VNothing THuyen; 'Ả' -> VnChar 'A' VNothing THoi
  'Ã' -> VnChar 'A' VNothing TNga; 'Ạ' -> VnChar 'A' VNothing TNang
  'Ă' -> VnChar 'A' VTrang TNothing; 'Ắ' -> VnChar 'A' VTrang TSac; 'Ằ' -> VnChar 'A' VTrang THuyen
  'Ẳ' -> VnChar 'A' VTrang THoi;   'Ẵ' -> VnChar 'A' VTrang TNga; 'Ặ' -> VnChar 'A' VTrang TNang
  'Â' -> VnChar 'A' VMu TNothing;   'Ấ' -> VnChar 'A' VMu TSac;   'Ầ' -> VnChar 'A' VMu THuyen
  'Ẩ' -> VnChar 'A' VMu THoi;     'Ẫ' -> VnChar 'A' VMu TNga;   'Ậ' -> VnChar 'A' VMu TNang

  'é' -> VnChar 'e' VNothing TSac; 'è' -> VnChar 'e' VNothing THuyen; 'ẻ' -> VnChar 'e' VNothing THoi
  'ẽ' -> VnChar 'e' VNothing TNga; 'ẹ' -> VnChar 'e' VNothing TNang
  'ê' -> VnChar 'e' VMu TNothing;   'ế' -> VnChar 'e' VMu TSac;   'ề' -> VnChar 'e' VMu THuyen
  'ể' -> VnChar 'e' VMu THoi;     'ễ' -> VnChar 'e' VMu TNga;   'ệ' -> VnChar 'e' VMu TNang
  'É' -> VnChar 'E' VNothing TSac; 'È' -> VnChar 'E' VNothing THuyen; 'Ẻ' -> VnChar 'E' VNothing THoi
  'Ẽ' -> VnChar 'E' VNothing TNga; 'Ẹ' -> VnChar 'E' VNothing TNang
  'Ê' -> VnChar 'E' VMu TNothing;   'Ế' -> VnChar 'E' VMu TSac;   'Ề' -> VnChar 'E' VMu THuyen
  'Ể' -> VnChar 'E' VMu THoi;     'Ễ' -> VnChar 'E' VMu TNga;   'Ệ' -> VnChar 'E' VMu TNang

  'í' -> VnChar 'i' VNothing TSac; 'ì' -> VnChar 'i' VNothing THuyen; 'ỉ' -> VnChar 'i' VNothing THoi
  'ĩ' -> VnChar 'i' VNothing TNga; 'ị' -> VnChar 'i' VNothing TNang
  'Í' -> VnChar 'I' VNothing TSac; 'Ì' -> VnChar 'I' VNothing THuyen; 'Ỉ' -> VnChar 'I' VNothing THoi
  'Ĩ' -> VnChar 'I' VNothing TNga; 'Ị' -> VnChar 'I' VNothing TNang

  'ó' -> VnChar 'o' VNothing TSac; 'ò' -> VnChar 'o' VNothing THuyen; 'ỏ' -> VnChar 'o' VNothing THoi
  'õ' -> VnChar 'o' VNothing TNga; 'ọ' -> VnChar 'o' VNothing TNang
  'ô' -> VnChar 'o' VMu TNothing;   'ố' -> VnChar 'o' VMu TSac;   'ồ' -> VnChar 'o' VMu THuyen
  'ổ' -> VnChar 'o' VMu THoi;     'ỗ' -> VnChar 'o' VMu TNga;   'ộ' -> VnChar 'o' VMu TNang
  'ơ' -> VnChar 'o' VMoc TNothing;  'ớ' -> VnChar 'o' VMoc TSac;  'ờ' -> VnChar 'o' VMoc THuyen
  'ở' -> VnChar 'o' VMoc THoi;    'ỡ' -> VnChar 'o' VMoc TNga;  'ợ' -> VnChar 'o' VMoc TNang
  'Ó' -> VnChar 'O' VNothing TSac; 'Ò' -> VnChar 'O' VNothing THuyen; 'Ỏ' -> VnChar 'O' VNothing THoi
  'Õ' -> VnChar 'O' VNothing TNga; 'Ọ' -> VnChar 'O' VNothing TNang
  'Ô' -> VnChar 'O' VMu TNothing;   'Ố' -> VnChar 'O' VMu TSac;   'Ồ' -> VnChar 'O' VMu THuyen
  'Ổ' -> VnChar 'O' VMu THoi;     'Ỗ' -> VnChar 'O' VMu TNga;   'Ộ' -> VnChar 'O' VMu TNang
  'Ơ' -> VnChar 'O' VMoc TNothing;  'Ớ' -> VnChar 'O' VMoc TSac;  'Ờ' -> VnChar 'O' VMoc THuyen
  'Ở' -> VnChar 'O' VMoc THoi;    'Ỡ' -> VnChar 'O' VMoc TNga;  'Ợ' -> VnChar 'O' VMoc TNang

  'ú' -> VnChar 'u' VNothing TSac; 'ù' -> VnChar 'u' VNothing THuyen; 'ủ' -> VnChar 'u' VNothing THoi
  'ũ' -> VnChar 'u' VNothing TNga; 'ụ' -> VnChar 'u' VNothing TNang
  'ư' -> VnChar 'u' VMoc TNothing;  'ứ' -> VnChar 'u' VMoc TSac;  'ừ' -> VnChar 'u' VMoc THuyen
  'ử' -> VnChar 'u' VMoc THoi;    'ữ' -> VnChar 'u' VMoc TNga;  'ự' -> VnChar 'u' VMoc TNang
  'Ú' -> VnChar 'U' VNothing TSac; 'Ù' -> VnChar 'U' VNothing THuyen; 'Ủ' -> VnChar 'U' VNothing THoi
  'Ũ' -> VnChar 'U' VNothing TNga; 'Ụ' -> VnChar 'U' VNothing TNang
  'Ư' -> VnChar 'U' VMoc TNothing;  'Ứ' -> VnChar 'U' VMoc TSac;  'Ừ' -> VnChar 'U' VMoc THuyen
  'Ử' -> VnChar 'U' VMoc THoi;    'Ữ' -> VnChar 'U' VMoc TNga;  'Ự' -> VnChar 'U' VMoc TNang

  'ý' -> VnChar 'y' VNothing TSac; 'ỳ' -> VnChar 'y' VNothing THuyen; 'ỷ' -> VnChar 'y' VNothing THoi
  'ỹ' -> VnChar 'y' VNothing TNga; 'ỵ' -> VnChar 'y' VNothing TNang
  'Ý' -> VnChar 'Y' VNothing TSac; 'Ỳ' -> VnChar 'Y' VNothing THuyen; 'Ỷ' -> VnChar 'Y' VNothing THoi
  'Ỹ' -> VnChar 'Y' VNothing TNga; 'Ỵ' -> VnChar 'Y' VNothing TNang

  'đ' -> VnChar 'd' VBarD TNothing
  'Đ' -> VnChar 'D' VBarD TNothing

  other -> VnChar other VNothing TNothing

type Stroke = [(Float, Float)]

data Glyph = Glyph
  { glyphW       :: !Float
  , glyphStrokes :: ![Stroke]
  }

oShape :: Stroke
oShape = [(1.5,0),(0,1.5),(0,5.5),(1.5,7),(2.5,7),(4,5.5),(4,1.5),(2.5,0),(1.5,0)]

pTop :: Stroke
pTop = [(0,0),(0,7),(3,7),(4,6),(4,4.5),(3,3.5),(0,3.5)]

-- Bảng nét vẽ vector (stroke font) cho chữ cái in hoa, chữ số và ký hiệu
glyphTable :: Map.Map Char Glyph
glyphTable = Map.fromList
  [ ('A', Glyph 4 [[(0,0),(0,6),(1,7),(3,7),(4,6),(4,0)], [(0,3.5),(4,3.5)]])
  , ('B', Glyph 4 [pTop, [(3,3.5),(4,2.5),(4,1),(3,0),(0,0)]])
  , ('C', Glyph 4 [[(4,7),(1.5,7),(0,5.5),(0,1.5),(1.5,0),(4,0)]])
  , ('D', Glyph 4 [[(0,0),(0,7),(2.5,7),(4,5.5),(4,1.5),(2.5,0),(0,0)]])
  , ('E', Glyph 4 [[(4,7),(0,7),(0,0),(4,0)], [(0,3.5),(3,3.5)]])
  , ('F', Glyph 4 [[(4,7),(0,7),(0,0)], [(0,3.5),(3,3.5)]])
  , ('G', Glyph 4 [[(4,7),(1.5,7),(0,5.5),(0,1.5),(1.5,0),(4,0),(4,3.5),(2,3.5)]])
  , ('H', Glyph 4 [[(0,0),(0,7)], [(4,0),(4,7)], [(0,3.5),(4,3.5)]])
  , ('I', Glyph 0 [[(0,0),(0,7)]])
  , ('J', Glyph 4 [[(4,7),(4,1),(3,0),(1,0),(0,1),(0,2)]])
  , ('K', Glyph 4 [[(0,0),(0,7)], [(4,7),(1,3.5),(0,3.5)], [(1,3.5),(4,0)]])
  , ('L', Glyph 4 [[(0,7),(0,0),(4,0)]])
  , ('M', Glyph 5 [[(0,0),(0,7),(2.5,4),(5,7),(5,0)]])
  , ('N', Glyph 4 [[(0,0),(0,7),(4,0),(4,7)]])
  , ('O', Glyph 4 [oShape])
  , ('P', Glyph 4 [pTop])
  , ('Q', Glyph 4 [oShape, [(2.5,1.5),(4,0)]])
  , ('R', Glyph 4 [pTop, [(2,3.5),(4,0)]])
  , ('S', Glyph 4 [[(4,7),(1,7),(0,6),(0,4.5),(1,3.5),(3,3.5),(4,2.5),(4,1),(3,0),(0,0)]])
  , ('T', Glyph 4 [[(0,7),(4,7)], [(2,7),(2,0)]])
  , ('U', Glyph 4 [[(0,7),(0,1.5),(1.5,0),(2.5,0),(4,1.5),(4,7)]])
  , ('V', Glyph 4 [[(0,7),(0,3),(2,0),(4,3),(4,7)]])
  , ('W', Glyph 5 [[(0,7),(0,0),(2.5,3),(5,0),(5,7)]])
  , ('X', Glyph 4 [[(0,7),(0,6),(4,1),(4,0)], [(4,7),(4,6),(0,1),(0,0)]])
  , ('Y', Glyph 4 [[(0,7),(0,5),(2,3.5),(4,5),(4,7)], [(2,3.5),(2,0)]])
  , ('Z', Glyph 4 [[(0,7),(4,7),(4,6),(0,1),(0,0),(4,0)]])

  , ('0', Glyph 4 [oShape, [(1,1.5),(3,5.5)]])
  , ('1', Glyph 2 [[(0,5.5),(2,7),(2,0)]])
  , ('2', Glyph 4 [[(0,7),(3,7),(4,6),(4,4.5),(3,3.5),(1,3.5),(0,2.5),(0,0),(4,0)]])
  , ('3', Glyph 4 [[(0,7),(3,7),(4,6),(4,4.5),(3,3.5),(1,3.5)], [(3,3.5),(4,2.5),(4,1),(3,0),(0,0)]])
  , ('4', Glyph 4 [[(3,0),(3,7),(0,2.5),(4,2.5)]])
  , ('5', Glyph 4 [[(4,7),(0,7),(0,4),(3,4),(4,3),(4,1),(3,0),(0,0)]])
  , ('6', Glyph 4 [[(4,7),(1,7),(0,6),(0,1),(1,0),(3,0),(4,1),(4,3),(3,4),(0,4)]])
  , ('7', Glyph 4 [[(0,7),(4,7),(4,6),(1.5,0)]])
  , ('8', Glyph 4 [ [(1,3.5),(0,4.5),(0,6),(1,7),(3,7),(4,6),(4,4.5),(3,3.5),(1,3.5)]
                  , [(1,3.5),(0,2.5),(0,1),(1,0),(3,0),(4,1),(4,2.5),(3,3.5)] ])
  , ('9', Glyph 4 [[(0,0),(3,0),(4,1),(4,6),(3,7),(1,7),(0,6),(0,4),(1,3),(4,3)]])

  , (' ', Glyph 1.6 [])
  , ('.', Glyph 0 [[(0,0)]])
  , (',', Glyph 0.5 [[(0.5,0.5),(0,-1)]])
  , (':', Glyph 0 [[(0,0)], [(0,4)]])
  , (';', Glyph 0.5 [[(0.5,4)], [(0.5,0.5),(0,-1)]])
  , ('!', Glyph 0 [[(0,7),(0,2.5)], [(0,0)]])
  , ('?', Glyph 4 [[(0,6),(1,7),(3,7),(4,6),(4,4.5),(2,3),(2,2)], [(2,0)]])
  , ('-', Glyph 3 [[(0,3.5),(3,3.5)]])
  , ('+', Glyph 4 [[(0,3.5),(4,3.5)], [(2,1.5),(2,5.5)]])
  , ('=', Glyph 4 [[(0,2.5),(4,2.5)], [(0,4.5),(4,4.5)]])
  , ('_', Glyph 4 [[(0,-1),(4,-1)]])
  , ('/', Glyph 3 [[(0,0),(3,7)]])
  , ('\\', Glyph 3 [[(0,7),(3,0)]])
  , ('|', Glyph 0 [[(0,-1),(0,8)]])
  , ('(', Glyph 1.5 [[(1.5,8),(0,6.5),(0,0.5),(1.5,-1)]])
  , (')', Glyph 1.5 [[(0,8),(1.5,6.5),(1.5,0.5),(0,-1)]])
  , ('[', Glyph 1.5 [[(1.5,8),(0,8),(0,-1),(1.5,-1)]])
  , (']', Glyph 1.5 [[(0,8),(1.5,8),(1.5,-1),(0,-1)]])
  , ('<', Glyph 3 [[(3,6),(0,3.5),(3,1)]])
  , ('>', Glyph 3 [[(0,6),(3,3.5),(0,1)]])
  , ('#', Glyph 5 [[(1.5,0),(1.5,7)], [(3.5,0),(3.5,7)], [(0,2.3),(5,2.3)], [(0,4.7),(5,4.7)]])
  , ('%', Glyph 5 [[(0,0),(5,7)], [(0.5,6)], [(1,6)], [(4,1)], [(4.5,1)]])
  , ('*', Glyph 3 [[(1.5,2.5),(1.5,6)], [(0,3.5),(3,5)], [(0,5),(3,3.5)]])
  , ('\'', Glyph 0 [[(0,7),(0,5.5)]])
  , ('"', Glyph 1.5 [[(0,7),(0,5.5)], [(1.5,7),(1.5,5.5)]])
  , ('\x25B2', Glyph 4 [[(0,1),(2,6),(4,1),(0,1)], [(1,2),(2,4.5),(3,2),(1,2)]])
  , ('\x25BC', Glyph 4 [[(0,6),(2,1),(4,6),(0,6)], [(1,5),(2,2.5),(3,5),(1,5)]])
  , ('\x25C4', Glyph 4 [[(4,6),(0,3.5),(4,1),(4,6)], [(3,5),(1,3.5),(3,2),(3,5)]])
  , ('\x25BA', Glyph 4 [[(0,6),(4,3.5),(0,1),(0,6)], [(1,5),(3,3.5),(1,2),(1,5)]])
  , ('&', Glyph 4 [[(4,0),(1,4.5),(1,6),(2,7),(3,6),(3,5),(0,2.5),(0,1),(1,0),(3,0),(4,1.5)]])
  ]

lookupGlyph :: Char -> Glyph
lookupGlyph !c = case Map.lookup (toUpper c) glyphTable of
  Just g  -> g
  Nothing -> Map.findWithDefault (Glyph 4 []) '?' glyphTable

letterGap :: Float
letterGap = 1.8

fontUnit :: Float -> Float
fontUnit !s = s * 108.0 / 7.0

strokeThickness :: Float -> Float
strokeThickness !u = fromIntegral (max 1 (floor u :: Int))

vnGlyphWidth :: VnChar -> Float
vnGlyphWidth !vc = glyphW (lookupGlyph (vcBase vc)) + (if vcMod vc == VMoc then 1.2 else 0.0)

charAdvance :: Float -> Float -> VnChar -> Float
charAdvance !u !t !vc = vnGlyphWidth vc * u + t + letterGap * u

textWidth :: Float -> String -> Float
textWidth !s !str
  | null str  = 0.0
  | otherwise =
      let !u = fontUnit s
          !t = strokeThickness u
          !total = foldl' (\ !acc ch -> acc + charAdvance u t (parseVnChar ch)) 0.0 str
      in fromIntegral (round (total - letterGap * u) :: Int)

-- Làm tròn tọa độ đỉnh về lưới pixel để nét chữ sắc nét
snapPx :: Float -> Float -> Float
snapPx !t !v = fromIntegral (round v :: Int) + (if odd (round t :: Int) then 0.5 else 0.0)

-- Vẽ một nét gấp khúc có độ dày t pixel và điểm nối vuông tại các đỉnh
drawStroke :: Float -> Float -> Stroke -> Picture
drawStroke !u !t !pts =
  let !ps = [ (snapPx t (x * u), snapPx t (y * u)) | (x, y) <- pts ]
      !h  = t / 2.0
      segment (x1, y1) (x2, y2) =
        let !dx = x2 - x1
            !dy = y2 - y1
            !len = sqrt (dx * dx + dy * dy)
        in if len < 0.01
             then blank
             else let !nx = -dy / len * h
                      !ny =  dx / len * h
                  in polygon [(x1 + nx, y1 + ny), (x2 + nx, y2 + ny), (x2 - nx, y2 - ny), (x1 - nx, y1 - ny)]
  in pictures (zipWith segment ps (drop 1 ps) ++ [ translate x y (rectangleSolid t t) | (x, y) <- ps ])

-- Sinh các nét phụ cho dấu mũ, dấu móc, gạch ngang Đ và 5 dấu thanh tiếng Việt
diacriticStrokes :: VnChar -> [Stroke]
diacriticStrokes !vc =
  let !w  = glyphW (lookupGlyph (vcBase vc))
      !cx = w / 2.0
      modStrokes = case vcMod vc of
        VMu    -> [[(cx - 1.6, 8.5), (cx, 10.0), (cx + 1.6, 8.5)]]
        VTrang -> [[(cx - 1.6, 10.0), (cx - 1.6, 8.6), (cx + 1.6, 8.6), (cx + 1.6, 10.0)]]
        VMoc   -> [[(w - 0.4, 7.0), (w + 1.2, 7.0), (w + 1.2, 8.4)]]
        VBarD  -> [[(-1.0, 3.5), (1.5, 3.5)]]
        VNothing -> []
      (!tx, !ty) = case vcMod vc of
        VMu    -> (cx + 2.7, 8.8)
        VTrang -> (cx, 10.8)
        _      -> (cx, 8.6)
      toneStrokes = case vcTone vc of
        TSac   -> [[(tx - 0.6, ty), (tx + 0.8, ty + 1.8)]]
        THuyen -> [[(tx - 0.8, ty + 1.8), (tx + 0.6, ty)]]
        THoi   -> [[(tx - 0.9, ty + 2.0), (tx + 0.9, ty + 2.0), (tx + 0.9, ty + 1.0), (tx, ty + 1.0), (tx, ty)]]
        TNga   -> [[(tx - 1.6, ty + 0.5), (tx - 0.6, ty + 1.6), (tx + 0.6, ty + 0.5), (tx + 1.6, ty + 1.6)]]
        TNang  -> [[(cx, -2.2)]]
        TNothing -> []
  in modStrokes ++ toneStrokes

drawVnGlyph :: Float -> Float -> VnChar -> Picture
drawVnGlyph !u !t !vc =
  translate (t / 2.0) 0.0 $ pictures
    (map (drawStroke u t) (glyphStrokes (lookupGlyph (vcBase vc)) ++ diacriticStrokes vc))

data TextAlign = AlignLeft | AlignCenter | AlignRight
  deriving (Eq, Show)

-- Dàn trang và vẽ chuỗi tiếng Việt theo căn lề trái, giữa hoặc phải
drawTextAligned :: TextAlign -> Float -> Color -> String -> Picture
drawTextAligned !align !s !txtColor !str =
  let !u      = fontUnit s
      !t      = strokeThickness u
      !w      = textWidth s str
      !startX = case align of
                  AlignLeft   -> 0.0
                  AlignCenter -> fromIntegral (round (-w / 2.0) :: Int)
                  AlignRight  -> -w
      place (!accPics, !curX) ch =
        let !vc = parseVnChar ch
            !gx = fromIntegral (round curX :: Int)
        in (translate gx 0.0 (drawVnGlyph u t vc) : accPics, curX + charAdvance u t vc)
      !glyphPics = fst (foldl' place ([], 0.0) str)
  in translate startX 0.0 $ color txtColor (pictures glyphPics)

drawVietnameseText :: Float -> Color -> String -> Picture
drawVietnameseText = drawTextAligned AlignLeft

drawTextCenter :: Float -> Color -> String -> Picture
drawTextCenter = drawTextAligned AlignCenter

drawTextRight :: Float -> Color -> String -> Picture
drawTextRight = drawTextAligned AlignRight

drawTextMid :: Float -> Color -> String -> Picture
drawTextMid !s !col !str =
  translate 0.0 (fromIntegral (round (-fontUnit s * 3.5) :: Int)) (drawTextCenter s col str)

uiCyan, uiGold, uiText, uiDim, uiPanel, uiPanelDark :: Color
uiCyan      = makeColorI 0 225 255 255
uiGold      = makeColorI 255 205 40 255
uiText      = makeColorI 225 236 248 255
uiDim       = makeColorI 130 152 180 255
uiPanel     = makeColorI 13 18 29 240
uiPanelDark = makeColorI 8 11 18 255

drawPanel :: Float -> Float -> Color -> Picture
drawPanel !w !h !accent =
  let !hw = w / 2.0
      !hh = h / 2.0
      !cl = 16.0
      corner sx sy = translate (sx * hw) (sy * hh) $ color accent $ pictures
        [ translate (-sx * cl / 2.0) 0.0 (rectangleSolid cl 2.5)
        , translate 0.0 (-sy * cl / 2.0) (rectangleSolid 2.5 cl)
        ]
  in pictures
       [ translate 4.0 (-5.0) $ color (makeColorI 0 0 0 130) (rectangleSolid w h)
       , color uiPanel (rectangleSolid w h)
       , translate 0.0 (hh - 1.0) $ color (withAlpha 0.10 accent) (rectangleSolid w 2.0)
       , color (withAlpha 0.35 accent) (rectangleWire w h)
       , corner 1.0 1.0, corner (-1.0) 1.0, corner 1.0 (-1.0), corner (-1.0) (-1.0)
       ]

drawPanelTitle :: Color -> String -> Picture
drawPanelTitle !accent !title =
  pictures
    [ translate 2.0 5.0 $ color accent (rectangleSolid 4.0 14.0)
    , translate 12.0 0.0 $ drawVietnameseText 0.098 accent title
    ]

drawKeyCap :: Float -> Float -> Float -> String -> Picture
drawKeyCap !w !h !glow !label =
  let !g       = max 0.0 (min 1.0 glow)
      !press   = -2.0 * g
      !faceCol = mixColors g (1.0 - g) (makeColorI 0 190 240 255) (makeColorI 34 44 62 255)
      !edgeCol = mixColors g (1.0 - g) (makeColorI 200 250 255 255) (makeColorI 85 105 135 255)
      !txtCol  = if g > 0.45 then makeColorI 4 12 20 255 else uiText
      !txtS    = if length label > 2 then 0.072 else 0.095
  in pictures
       [ translate 0.0 (-3.0) $ color (makeColorI 5 7 11 255) (rectangleSolid w h)
       , translate 0.0 press $ pictures
           [ color faceCol (rectangleSolid w h)
           , translate 0.0 (h / 2.0 - 2.0) $ color (makeColorI 255 255 255 (30 + round (60 * g))) (rectangleSolid (w - 4.0) 3.0)
           , color edgeCol (rectangleWire w h)
           , drawTextMid txtS txtCol label
           ]
       ]

drawProgressBar :: Float -> Float -> Float -> Color -> Picture
drawProgressBar !w !h !ratio !barCol =
  let !r     = max 0.0 (min 1.0 ratio)
      !inner = w - 4.0
      !fillW = max 0.0 (inner * r)
  in pictures
       [ color uiPanelDark (rectangleSolid w h)
       , if fillW > 0.5
           then translate ((fillW - inner) / 2.0) 0.0 $ pictures
                  [ color barCol (rectangleSolid fillW (h - 4.0))
                  , translate 0.0 ((h - 4.0) / 4.0) $ color (makeColorI 255 255 255 45) (rectangleSolid fillW ((h - 4.0) / 2.0))
                  ]
           else blank
       , color (makeColorI 70 88 115 255) (rectangleWire w h)
       ]

drawKeyHintRow :: [(String, String)] -> Picture
drawKeyHintRow !items =
  let !descS  = 0.078
      keyW k  = max 26.0 (textWidth 0.072 k + 14.0)
      itemW (k, d) = keyW k + 8.0 + textWidth descS d
      !gap    = 22.0
      !totalW = sum (map itemW items) + gap * fromIntegral (max 0 (length items - 1))
      step (!accPic, !x) it@(k, d) =
        let !kw  = keyW k
            !pic = pictures
              [ translate (x + kw / 2.0) 0.0 (drawKeyCap kw 22.0 0.0 k)
              , translate (x + kw + 8.0) (-4.0) (drawVietnameseText descS uiDim d)
              ]
        in (pictures [accPic, pic], x + itemW it + gap)
  in fst (foldl' step (blank, -totalW / 2.0) items)

targetAngle :: Direction -> Float
targetAngle dir = case dir of
  North -> 0.0
  East  -> 90.0
  South -> 180.0
  West  -> 270.0

drawViperNanoShield :: Float -> Picture
drawViperNanoShield !animTime =
  pictures
    [ rotate (animTime * 75.0) $ pictures
        [ color (makeColorI 40 255 100 180) (thickCircle 19.0 2.8)
        , color (makeColorI 180 255 200 70) (circleSolid 19.0)
        , color (makeColorI 40 255 100 220) $ pictures
            [ rotate (fromIntegral i * 60.0) $ translate 0.0 19.0 (rectangleSolid 4.0 4.0)
            | i <- [0..5 :: Int]
            ]
        ]
    , rotate (-animTime * 45.0) $
        color (makeColorI 20 200 80 120) (thickCircle 23.0 1.5)
    ]

thickLine :: Point -> Point -> Float -> Picture
thickLine (!x1, !y1) (!x2, !y2) !w =
  let !dx = x2 - x1
      !dy = y2 - y1
      !len = sqrt (dx * dx + dy * dy)
  in if len < 0.001
       then circleSolid (w / 2)
       else
         let !deg = atan2 dy dx * 180.0 / pi
             !cx  = (x1 + x2) / 2
             !cy  = (y1 + y2) / 2
         in translate cx cy (rotate (-deg) (rectangleSolid len (max 1.0 w)))

drawHeroCustomVisuals :: TankClass -> Float -> Color -> Color -> Picture
drawHeroCustomVisuals !cls !animTime !_primaryC !glowC = case cls of
  VanguardCyan -> pictures
    [
      translate 0.0 14.0 $ color (makeColorI 120 240 255 240) (polygon [(0, 7), (6, 0), (-6, 0)])
    ,
      translate (-11.0) 7.0 $ color glowC (polygon [(0, 0), (-4, 4), (0, 7)])
    , translate 11.0 7.0 $ color glowC (polygon [(0, 0), (4, 4), (0, 7)])
    ]
  TitanCrimson -> pictures
    [
      translate (-4.0) 13.0 $ pictures
        [ color (makeColorI 20 20 25 255) (rectangleSolid 3.5 16.0)
        , color (makeColorI 220 40 40 255) (rectangleSolid 2.2 15.0)
        ]
    , translate 4.0 13.0 $ pictures
        [ color (makeColorI 20 20 25 255) (rectangleSolid 3.5 16.0)
        , color (makeColorI 220 40 40 255) (rectangleSolid 2.2 15.0)
        ]
    ,
      translate (-12.0) (-2.0) $ color (makeColorI 160 30 30 255) (rectangleSolid 4.0 16.0)
    , translate 12.0 (-2.0) $ color (makeColorI 160 30 30 255) (rectangleSolid 4.0 16.0)
    ]
  ViperEmerald -> pictures
    [
      translate 0.0 13.0 $ color glowC (polygon [(0, 7), (5, 0), (-5, 0)])
    ,
      translate (-7.0) (-9.0) $ color glowC (circleSolid 3.2)
    , translate 7.0 (-9.0) $ color glowC (circleSolid 3.2)
    ]
  SolarisGold -> pictures
    [
      translate (-15.0) 0.0 $ pictures
        [ color uiGold (polygon [(0, -8), (-6, -4), (-6, 6), (0, 8)])
        , color white (thickLine (0, 0) (-5, 0) 1.2)
        ]
    , translate 15.0 0.0 $ pictures
        [ color uiGold (polygon [(0, -8), (6, -4), (6, 6), (0, 8)])
        , color white (thickLine (0, 0) (5, 0) 1.2)
        ]
    ,
      translate 0.0 12.0 $ color glowC (thickCircle 5.0 1.5)
    ]
  PhantomViolet -> pictures
    [
      translate (-12.0) (-4.0) $ color (makeColorI 80 20 120 240) (polygon [(0, 10), (-7, -6), (0, -6)])
    , translate 12.0 (-4.0) $ color (makeColorI 80 20 120 240) (polygon [(0, 10), (7, -6), (0, -6)])
    ,
      color (makeColorI 230 140 255 240) (circleSolid 4.0)
    ]
  StrikerRuby -> pictures
    [
      translate (-4.5) 10.0 $ color (makeColorI 255 40 70 255) (rectangleSolid 2.8 11.0)
    , translate 0.0 13.0 $ color (makeColorI 255 40 70 255) (rectangleSolid 3.2 14.0)
    , translate 4.5 10.0 $ color (makeColorI 255 40 70 255) (rectangleSolid 2.8 11.0)
    ,
      translate 5.0 17.0 $ color (makeColorI 255 0 60 220) (thickLine (0, 0) (0, 24.0) 1.0)
    ]
  AegisOnyx -> pictures
    [
      translate (-13.0) 0.0 $ pictures
        [ color (makeColorI 60 70 85 255) (rectangleSolid 5.0 24.0)
        , color (makeColorI 180 195 210 255) (rectangleWire 5.0 24.0)
        ]
    , translate 13.0 0.0 $ pictures
        [ color (makeColorI 60 70 85 255) (rectangleSolid 5.0 24.0)
        , color (makeColorI 180 195 210 255) (rectangleWire 5.0 24.0)
        ]
    ,
      color (makeColorI 40 48 60 255) (circleSolid 8.5)
    , color (makeColorI 210 225 240 255) (thickCircle 8.5 1.5)
    ]
  TempestAzure -> pictures
    [
      translate (-4.0) 13.0 $ color (makeColorI 0 220 255 255) (thickLine (0, 0) (0, 12.0) 2.2)
    , translate 4.0 13.0 $ color (makeColorI 0 220 255 255) (thickLine (0, 0) (0, 12.0) 2.2)
    ,
      translate 0.0 20.0 $ rotate (sin (animTime * 30.0) * 20.0) $
        color white (thickLine (-4.0, 0) (4.0, 0) 1.5)
    ]
  MirageAmethyst -> pictures
    [
      translate (-14.0) (sin (animTime * 8.0) * 3.0) $
        rotate (animTime * 60.0) $ color (makeColorI 210 100 255 220) (rectangleSolid 6.0 6.0)
    , translate 14.0 (-sin (animTime * 8.0) * 3.0) $
        rotate (-animTime * 60.0) $ color (makeColorI 210 100 255 220) (rectangleSolid 6.0 6.0)
    ]
  EclipseObsidian -> pictures
    [
      rotate (animTime * 50.0) $ pictures
        [ color (makeColorI 15 5 25 220) (thickCircle 18.0 3.5)
        , color (makeColorI 180 50 255 160) (thickCircle 19.5 1.5)
        , color (makeColorI 255 120 40 180) (thickCircle 21.0 1.0)
        ,
          rotate 45.0 $ translate 0.0 19.0 $ color white (circleSolid 1.5)
        , rotate 165.0 $ translate 0.0 19.0 $ color (makeColorI 255 150 50 255) (circleSolid 1.5)
        , rotate 285.0 $ translate 0.0 19.0 $ color (makeColorI 200 60 255 255) (circleSolid 1.5)
        ]
    ,
      color black (circleSolid 6.5)
    , color (makeColorI 140 20 220 255) (thickCircle 6.5 1.5)
    ]

-- Vẽ mô hình xe tăng giả lập 3D cùng các hiệu ứng trạng thái và thanh máu
drawHeroTank3D :: Float -> Float -> Tank -> Picture
drawHeroTank3D !animTime !currentAngle !tank =
  let !hInfo       = heroClassInfo (tankClass tank)
      !primaryC    = heroPrimaryCol hInfo
      !accentC     = heroAccentCol hInfo
      !glowC       = heroGlowCol hInfo
      !isDead      = tankRespawnTimer tank > 0
      !isInvinc    = tankInvincibleTimer tank > 0
      !blinkOn     = floor (animTime * 12.0) `mod` 2 == (0 :: Int)

      !isStealthed = not isDead && tankStealthTimer tank > 0
      !isFrozen    = not isDead && tankFreezeTimer tank > 0
      !isEmped     = not isDead && tankEmpTimer tank > 0
      !hasHexArmor = not isDead && tankHexShieldTimer tank > 0
      !hasAcid     = not isDead && tankAcidTimer tank > 0
      !hasBlackHole = not isDead && tankBlackHoleTimer tank > 0
      !hasLightning = not isDead && tankLightningTimer tank > 0
      !justFired   = not isDead && tankId tank /= 0 && tankFireCooldown tank > 0
                     && tankFireCooldown tank >= fireCooldownOf tank - 1

      !treadOffset = sin (animTime * 18.0) * 2.5

      !bodyColor   = if isDead then makeColorI 45 45 50 255
                     else if isStealthed then withAlpha 0.25 (makeColorI 170 175 190 255)
                     else primaryC
      !turretColor = if isDead then makeColorI 30 30 35 255
                     else if isStealthed then withAlpha 0.25 (makeColorI 140 145 160 255)
                     else accentC
  in pictures
       [
         if hasAcid
           then pictures
                  [ color (withAlpha 0.52 (makeColorI 40 255 70 255)) (circleSolid 24.0)
                  , color (makeColorI 80 255 120 (round (180 + 70 * sin (animTime * 8.0)))) (thickCircle 24.0 2.0)
                  , pictures
                      [ translate (cos (fromIntegral i * 1.57) * 12.0) (6.0 + sin (animTime * 5.0 + fromIntegral i) * 8.0) $
                          drawTextCenter 0.07 (makeColorI 120 255 150 255) "+"
                      | i <- [0 .. 3 :: Int]
                      ]
                  ]
           else blank

       , if not isStealthed
           then translate 2.5 (-3.5) $ color (makeColorI 0 0 0 140) (rectangleSolid 26.0 28.0)
           else blank

       , rotate currentAngle $ pictures
           [
             translate (-11.5) 0.0 $ pictures
               [ color (if isStealthed then withAlpha 0.25 (makeColorI 50 55 65 255) else makeColorI 20 24 30 255) (rectangleSolid 5.5 28.0)
               , color (if isStealthed then withAlpha 0.25 (makeColorI 80 90 100 255) else makeColorI 55 65 78 255) (rectangleWire 5.5 28.0)
               , translate 0.0 treadOffset $ color (makeColorI 75 88 105 180) (rectangleSolid 4.0 2.0)
               ]
           , translate 11.5 0.0 $ pictures
               [ color (if isStealthed then withAlpha 0.25 (makeColorI 50 55 65 255) else makeColorI 20 24 30 255) (rectangleSolid 5.5 28.0)
               , color (if isStealthed then withAlpha 0.25 (makeColorI 80 90 100 255) else makeColorI 55 65 78 255) (rectangleWire 5.5 28.0)
               , translate 0.0 (-treadOffset) $ color (makeColorI 75 88 105 180) (rectangleSolid 4.0 2.0)
               ]

           , color (makeColorI 10 12 16 255) (rectangleSolid 19.0 24.0)
           , color bodyColor (rectangleSolid 17.5 22.0)
           , translate 0.0 2.0 $ color (if isStealthed then withAlpha 0.15 white else makeColorI 255 255 255 70) (rectangleSolid 13.0 12.0)

           , translate 0.0 (-10.0) $ pictures
               [ color (if isStealthed then withAlpha 0.3 glowC else glowC) (rectangleSolid 9.0 2.5)
               , color white (rectangleSolid 5.0 1.2)
               ]

           , translate 0.0 11.0 $ pictures
               [ color (makeColorI 15 18 24 255) (rectangleSolid 4.0 14.0)
               , color turretColor (rectangleSolid 2.8 13.0)
               , translate 0.0 6.5 $ color (if isStealthed then withAlpha 0.3 glowC else glowC) (rectangleSolid 4.5 2.5)
               ]

           , if justFired
               then translate 0.0 20.5 $ pictures
                      [ color (withAlpha 0.55 glowC) (circleSolid 4.5)
                      , color (withAlpha 0.9 accentC) (polygon [(-2.2, 0), (2.2, 0), (0, 5.5)])
                      ]
               else blank

           , color (makeColorI 12 16 22 255) (circleSolid 7.5)
           , color turretColor (circleSolid 6.0)
           , color (if isStealthed then withAlpha 0.3 glowC else glowC) (circleSolid 3.5)
           , color white (circleSolid 1.5)
           ,
             drawHeroCustomVisuals (tankClass tank) animTime primaryC glowC
           ]

       , if hasLightning
           then pictures
                  [ rotate (animTime * 120.0) $ color (makeColorI 80 230 255 220) (thickCircle 24.0 2.0)
                  , rotate (-animTime * 150.0) $ color (makeColorI 200 255 255 240) (thickCircle 18.0 1.5)
                  , pictures
                      [ rotate (fromIntegral i * 45.0 + animTime * 180.0) $
                          translate 0.0 20.0 $ color (makeColorI 120 240 255 255) (rectangleSolid 2.5 8.0)
                      | i <- [0 .. 7 :: Int]
                      ]
                  ]
           else blank

       , if hasBlackHole
           then rotate (animTime * 220.0) $ pictures
                  [ color (makeColorI 15 5 28 240) (circleSolid 26.0)
                  , color (makeColorI 180 30 255 220) (thickCircle 26.0 2.5)
                  , color (makeColorI 255 110 20 200) (thickCircle 30.0 2.0)
                  , color black (circleSolid 14.0)
                  , color (makeColorI 220 180 255 255) (circleSolid 4.5)
                  ]
            else blank

       , if hasHexArmor
           then pictures
                  [ rotate (animTime * 85.0) $ color (makeColorI 255 220 70 230) (thickCircle 22.0 2.5)
                  , rotate (-animTime * 85.0) $ color (makeColorI 240 255 255 220) (thickCircle 26.0 2.0)
                  , color (withAlpha 0.15 (makeColorI 255 220 80 255)) (circleSolid 26.0)
                  ]
           else blank

       , if isFrozen
           then pictures
                  [ color (withAlpha 0.65 (makeColorI 100 220 255 255)) (rectangleSolid 32.0 34.0)
                  , color (makeColorI 210 250 255 255) (rectangleWire 32.0 34.0)
                  , color (withAlpha 0.85 white) (thickCircle 14.0 2.0)
                  , translate (-12.0) 12.0 $ color white (circleSolid 2.5)
                  , translate 12.0 (-12.0) $ color white (circleSolid 2.5)
                  ]
           else blank

       , if isEmped
           then pictures
                  [ color (makeColorI 70 210 255 220) (thickCircle 19.0 2.5)
                  , rotate (animTime * 300.0) $ pictures
                      [ translate 16.0 0.0 $ color (makeColorI 255 255 100 255) (rectangleSolid 4.0 2.0)
                      , translate (-16.0) 0.0 $ color (makeColorI 255 255 100 255) (rectangleSolid 4.0 2.0)
                      , translate 0.0 16.0 $ color (makeColorI 100 240 255 255) (rectangleSolid 2.0 4.0)
                      ]
                  ]
           else blank

       , if not isDead && tankStarTimer tank > 0 then drawStarAura animTime else blank

       , if not isDead && tankPierceTimer tank > 0
           then color (makeColorI 255 60 140 (round (110 + 80 * sin (animTime * 10.0)))) (thickCircle 15.5 2.0)
           else blank

       , if isInvinc && blinkOn && tankStarTimer tank <= 0 && not hasHexArmor
           then if tankClass tank == ViperEmerald
                  then drawViperNanoShield animTime
                  else pictures
                         [ color (makeColorI 0 240 255 140) (thickCircle 17.0 2.5)
                         , color (makeColorI 180 255 255 70) (circleSolid 17.0)
                         ]
           else blank

       , if isDead
           then pictures
                  [ translate 0.0 12.0 $ color (makeColorI 25 25 25 200) (circleSolid 9.0)
                  , translate (-3.0) 18.0 $ color (makeColorI 55 55 55 160) (circleSolid 7.0)
                  , translate 3.0 23.0 $ color (makeColorI 85 85 85 120) (circleSolid 5.0)
                  , drawTextMid 0.1 (makeColorI 255 70 70 255) (show (tankRespawnTimer tank `div` 30 + 1) ++ "s")
                  ]
           else blank

       , if not isDead && tankBurnTimer tank > 0 then drawBurnFlames animTime else blank
       , if not isDead && tankSilenceTimer tank > 0
           then translate 17.0 24.0 (drawSilenceIcon 5.0)
           else blank

       , if not isDead && tankId tank /= 0 && tankShield tank > 0
           then translate 0.0 22.6 $
                  let shW = min 26.0 (max 1.0 (26.0 * fromIntegral (tankShield tank) / fromIntegral (getClassMaxHP (tankClass tank))))
                  in pictures
                       [ color (makeColorI 10 12 16 220) (rectangleSolid 28.0 2.6)
                       , translate ((shW - 26.0) / 2.0) 0.0 $ color (makeColorI 210 240 255 255) (rectangleSolid shW 1.6)
                       ]
           else blank

       , if not isDead && tankId tank /= 0
           then translate 0.0 18.0 $ pictures
                  [ color (makeColorI 10 12 16 220) (rectangleSolid 28.0 4.0)
                  , color (makeColorI 60 70 85 255) (rectangleWire 28.0 4.0)
                  , let hpRatio = min 1.0 (max 0.0 (fromIntegral (tankHealth tank) / fromIntegral (getClassMaxHP (tankClass tank))))
                        hpW     = max 1.0 (26.0 * hpRatio)
                        hpCol   = if hpRatio > 0.5 then makeColorI 40 220 80 255
                                  else if hpRatio > 0.25 then makeColorI 240 200 30 255
                                  else makeColorI 255 40 40 255
                    in translate ((hpW - 26.0) / 2.0) 0.0 $ color hpCol (rectangleSolid hpW 2.6)
                  , translate 0.0 (-3.8) $ pictures
                      [ color (makeColorI 5 8 12 200) (rectangleSolid 28.0 2.2)
                      , let enRatio = fromIntegral (tankEnergy tank) / fromIntegral maxEnergy
                            enW     = max 0.5 (26.0 * enRatio)
                        in translate ((enW - 26.0) / 2.0) 0.0 $
                             color (makeColorI 0 220 255 255) (rectangleSolid enW 1.8)
                      ]
                  ]
           else blank

       , if not isDead && tankPoisonTimer tank > 0
           then pictures
                  [ translate (cos (fromIntegral i * 2.1) * 9.0) (sin (animTime * 4.0 + fromIntegral i) * 6.0 + 4.0) $
                      color (makeColorI 90 255 110 200) (circleSolid 2.4)
                  | i <- [0 .. 2 :: Int] ]
           else blank
       , if not isDead && tankWeakenTimer tank > 0
           then color (makeColorI 150 60 220 (round (90 + 60 * sin (animTime * 6.0)))) (thickCircle 20.0 3.0)
           else blank
       , if not isDead && tankArmorBreakTimer tank > 0
           then rotate (animTime * 60.0) $ color (makeColorI 255 140 40 220) $ pictures
                  [ rotate (fromIntegral i * 90.0) $ translate 0.0 18.0 (rectangleSolid 6.0 2.0) | i <- [0 .. 3 :: Int] ]
           else blank
       , if not isDead && tankBlindTimer tank > 0
           then color (makeColorI 255 240 120 (round (120 + 100 * sin (animTime * 14.0)))) (circleSolid 5.0)
           else blank
       , if not isDead && tankHasteTimer tank > 0
           then rotate currentAngle $ color (makeColorI 255 90 60 200) $ pictures
                  [ translate x (-18.0 - 3.0 * sin (animTime * 30.0 + x)) (rectangleSolid 1.6 7.0) | x <- [-7.0, 0.0, 7.0] ]
           else blank
       , if tankId tank /= 0 && not isDead
           then case tankStatusLabel tank of
                  Just (lbl, col) -> translate 0.0 30.0 $ drawTextCenter 0.078 col lbl
                  Nothing         -> blank
           else blank
       ]

tankStatusLabel :: Tank -> Maybe (String, Color)
tankStatusLabel !t = case filter fst labels of
  ((_, l) : _) -> Just l
  []           -> Nothing
  where
    labels =
      [ (tankFreezeTimer t > 0,     ("ĐÓNG BĂNG!", makeColorI 120 240 255 255))
      , (tankEmpTimer t > 0,        ("EMP: KHÓA BẮN!", makeColorI 255 230 60 255))
      , (tankBlackHoleTimer t > 0,  ("HỐ ĐEN!", makeColorI 255 120 255 255))
      , (tankPoisonTimer t > 0,     ("TRÚNG ĐỘC", makeColorI 110 255 130 255))
      , (tankBlindTimer t > 0,      ("CHÓI MẮT", makeColorI 255 240 120 255))
      , (tankWeakenTimer t > 0,     ("SUY YẾU", makeColorI 200 130 255 255))
      , (tankArmorBreakTimer t > 0, ("PHÁ GIÁP", makeColorI 255 150 60 255))
      , (tankReflectTimer t > 0,    ("PHẢN ĐÒN 30%", uiGold))
      , (tankHasteTimer t > 0,      ("CUỒNG NHIỆT", makeColorI 255 110 90 255))
      , (tankStealthTimer t > 0,    ("TÀNG HÌNH", withAlpha 0.9 white))
      ]

drawStarShape :: Float -> Float -> Picture
drawStarShape !rOuter !rInner =
  let pt i = let !a = pi / 2.0 + fromIntegral (i :: Int) * pi / 5.0
                 !r = if even i then rOuter else rInner
             in (r * cos a, r * sin a)
  in pictures [ polygon [(0.0, 0.0), pt i, pt (i + 1)] | i <- [0 .. 9] ]

drawStarAura :: Float -> Picture
drawStarAura !animTime =
  pictures
    [ color (makeColorI 255 215 40 60) (circleSolid 20.0)
    , color (makeColorI 255 225 80 220) (thickCircle (19.0 + 1.5 * sin (animTime * 8.0)) 2.2)
    , rotate (animTime * 120.0) $ color (makeColorI 255 240 140 255) $ pictures
        [ rotate (fromIntegral i * 72.0) $ translate 0.0 21.0 (drawStarShape 4.0 1.8) | i <- [0 .. 4 :: Int] ]
    ]

drawBurnFlames :: Float -> Picture
drawBurnFlames !animTime =
  pictures
    [ translate (fromIntegral i * 9.0 - 9.0) (-8.0) $
        let !h = 7.0 + 4.0 * sin (animTime * 14.0 + fromIntegral i * 2.1)
        in pictures
             [ color (makeColorI 255 90 20 210) (polygon [(-3.5, 0.0), (3.5, 0.0), (0.0, h)])
             , color (makeColorI 255 220 60 230) (polygon [(-1.8, 0.0), (1.8, 0.0), (0.0, h * 0.55)])
             ]
    | i <- [0 .. 2 :: Int]
    ]

drawSilenceIcon :: Float -> Picture
drawSilenceIcon !r =
  pictures
    [ color (makeColorI 30 10 45 230) (circleSolid (r + 1.0))
    , color (makeColorI 200 110 255 255) (thickCircle r 1.6)
    , color (makeColorI 200 110 255 255) $ rotate 45.0 (rectangleSolid 1.6 (r * 2.0))
    ]

-- Vẽ ô hiệu ứng đặc biệt (bùn lầy, phong ấn, dung nham) theo pha thời gian
drawTileGraphic :: Float -> Float -> Float -> TileEffect -> Picture
drawTileGraphic !animTime !phase !sz !eff =
  let !k = sz / 26.0
  in case eff of
       MudTile -> pictures
         [ color (makeColorI 58 42 24 255) (rectangleSolid sz sz)
         , color (makeColorI 84 62 34 255) (rectangleSolid (sz - 4.0 * k) (sz - 4.0 * k))
         , pictures
             [ translate (bx * k) (by * k) $
                 color (makeColorI 46 33 18 255) (circleSolid ((br + 0.8 * sin (animTime * 2.0 + phase + br)) * k))
             | (bx, by, br) <- [(-5.0, 4.0, 3.2), (5.0, -3.0, 2.6), (2.0, 6.5, 1.6), (-4.0, -6.0, 1.8)]
             ]
         , color (makeColorI 130 100 60 120) (rectangleWire (sz - 1.0) (sz - 1.0))
         ]
       SilenceTile ->
         let !glow = 0.55 + 0.45 * sin (animTime * 3.0 + phase)
         in pictures
              [ color (makeColorI 36 16 56 255) (rectangleSolid sz sz)
              , color (withAlpha (0.25 * glow) (makeColorI 190 90 255 255)) (rectangleSolid (sz - 3.0 * k) (sz - 3.0 * k))
              , color (withAlpha (0.5 + 0.5 * glow) (makeColorI 190 100 255 255)) $ pictures
                  [ thickCircle (7.5 * k) (1.6 * k)
                  , rotate 45.0 (rectangleSolid (1.6 * k) (15.0 * k))
                  ]
              , color (makeColorI 150 80 210 150) (rectangleWire (sz - 1.0) (sz - 1.0))
              ]
       FireTile ->
         let !flick = 0.6 + 0.4 * sin (animTime * 9.0 + phase)
         in pictures
              [ color (makeColorI 80 18 8 255) (rectangleSolid sz sz)
              , color (withAlpha flick (makeColorI 230 80 20 255)) (rectangleSolid (sz - 5.0 * k) (sz - 5.0 * k))
              , pictures
                  [ translate (fx * k) (-7.0 * k) $
                      let !h = (8.0 + 5.0 * sin (animTime * 11.0 + phase + fx)) * k
                      in pictures
                           [ color (makeColorI 255 150 30 230) (polygon [(-3.5 * k, 0.0), (3.5 * k, 0.0), (0.0, h)])
                           , color (makeColorI 255 235 120 240) (polygon [(-1.6 * k, 0.0), (1.6 * k, 0.0), (0.0, h * 0.5)])
                           ]
                  | fx <- [-6.0, 0.0, 6.0]
                  ]
              , color (makeColorI 255 120 40 140) (rectangleWire (sz - 1.0) (sz - 1.0))
              ]

drawEffectTile :: Float -> (Position, TileEffect) -> Picture
drawEffectTile !animTime (!pos, !eff) =
  let (!x, !y) = toScreenPos pos
      !phase   = fromIntegral (posX pos * 7 + posY pos * 13)
  in translate x y (drawTileGraphic animTime phase (cellSize - 1.0) eff)

tileColor :: TileEffect -> Color
tileColor eff = case eff of
  MudTile     -> makeColorI 190 140 80 255
  SilenceTile -> makeColorI 200 110 255 255
  FireTile    -> makeColorI 255 120 40 255

data LobbyButton
  = BtnPlayOffline
  | BtnPlayLocal2P
  | BtnCreateRoom
  | BtnJoinRoom
  | BtnOpenShop
  | BtnOpenEvent
  | BtnTestGold
  | BtnLogout
  | BtnPrevHero
  | BtnNextHero
  | BtnHost
  | BtnDiscoverLan
  | BtnPort
  | BtnCallsign
  | BtnClaimDaily
  deriving (Eq, Show, Enum, Bounded)

lobbyButtonRect :: LobbyButton -> (Float, Float, Float, Float)
lobbyButtonRect btn = case btn of
  BtnPlayOffline -> (-472.0,   83.0, 170.0,  42.0)
  BtnPlayLocal2P -> (-288.0,   83.0, 170.0,  42.0)
  BtnCreateRoom  -> (-475.0,  -95.0, 180.0,  50.0)
  BtnJoinRoom    -> (-285.0,  -95.0, 180.0,  50.0)
  BtnOpenShop    -> ( 210.0,  310.0, 140.0,  40.0)
  BtnOpenEvent   -> ( 370.0,  310.0, 140.0,  40.0)
  BtnTestGold    -> (-9999.0, -9999.0, 0.0,   0.0)
  BtnLogout      -> ( 530.0,  310.0, 130.0,  40.0)
  BtnPrevHero    -> ( -50.0,  -60.0,  36.0,  30.0)
  BtnNextHero    -> ( 120.0,  -60.0,  36.0,  30.0)
  BtnHost        -> (-440.0, -147.0, 230.0,  32.0)
  BtnDiscoverLan -> (-265.0, -147.0, 110.0,  32.0)
  BtnPort        -> (-310.0, -180.0,  60.0,  28.0)
  BtnCallsign    -> (-420.0,  310.0, 380.0,  50.0)
  BtnClaimDaily  -> ( 305.0,  310.0, 150.0,  40.0)

lobbyHitTest :: Float -> Float -> Maybe LobbyButton
lobbyHitTest !mx !my = find inside [minBound .. maxBound]
  where
    inside b = let (cx, cy, w, h) = lobbyButtonRect b
               in abs (mx - cx) <= w / 2.0 && abs (my - cy) <= h / 2.0

data LoginButton
  = BtnLoginUserField
  | BtnLoginPassField
  | BtnLoginSubmit
  deriving (Eq, Show, Enum, Bounded)

loginButtonRect :: LoginButton -> (Float, Float, Float, Float)
loginButtonRect btn = case btn of
  BtnLoginUserField -> (0.0,   45.0, 340.0, 38.0)
  BtnLoginPassField -> (0.0,  -35.0, 340.0, 38.0)
  BtnLoginSubmit    -> (0.0, -105.0, 346.0, 48.0)

loginHitTest :: Float -> Float -> Maybe LoginButton
loginHitTest !mx !my = find inside [minBound .. maxBound]
  where
    inside b = let (cx, cy, w, h) = loginButtonRect b
               in abs (mx - cx) <= w / 2.0 && abs (my - cy) <= h / 2.0

data EventButton
  = BtnClaimM1
  | BtnClaimM2
  | BtnClaimM3
  | BtnEventBack
  deriving (Eq, Show, Enum, Bounded)

eventButtonRect :: EventButton -> (Float, Float, Float, Float)
eventButtonRect btn = case btn of
  BtnClaimM1   -> (260.0,   95.0, 140.0, 38.0)
  BtnClaimM2   -> (260.0,   -5.0, 140.0, 38.0)
  BtnClaimM3   -> (260.0, -105.0, 140.0, 38.0)
  BtnEventBack -> (  0.0, -270.0, 260.0, 36.0)

eventHitTest :: Float -> Float -> Maybe EventButton
eventHitTest !mx !my = find inside [minBound .. maxBound]
  where
    inside b = let (cx, cy, w, h) = eventButtonRect b
               in abs (mx - cx) <= w / 2.0 && abs (my - cy) <= h / 2.0

data JoinCodeButton
  = BtnJoinCodeField
  | BtnJoinCodeHostField
  | BtnJoinCodeDiscoverLan
  | BtnJoinCodeSubmit
  | BtnJoinCodeCancel
  deriving (Eq, Show, Enum, Bounded)

joinCodeButtonRect :: JoinCodeButton -> (Float, Float, Float, Float)
joinCodeButtonRect btn = case btn of
  BtnJoinCodeField       -> (   0.0,   46.0, 260.0, 38.0)
  BtnJoinCodeHostField   -> ( -60.0,  -12.0, 220.0, 38.0)
  BtnJoinCodeDiscoverLan -> ( 115.0,  -12.0, 110.0, 38.0)
  BtnJoinCodeSubmit      -> ( -80.0,  -70.0, 140.0, 38.0)
  BtnJoinCodeCancel      -> (  80.0,  -70.0, 140.0, 38.0)

joinCodeHitTest :: Float -> Float -> Maybe JoinCodeButton
joinCodeHitTest !mx !my = find inside [minBound .. maxBound]
  where
    inside b = let (cx, cy, w, h) = joinCodeButtonRect b
               in abs (mx - cx) <= w / 2.0 && abs (my - cy) <= h / 2.0

shopHeroes :: [TankClass]
shopHeroes = [StrikerRuby, AegisOnyx, TempestAzure, MirageAmethyst, EclipseObsidian]

data ShopButton
  = BtnShopBuy !TankClass
  | BtnShopBack
  | BtnShopTestGold
  deriving (Eq, Show)

shopHitTest :: Float -> Float -> Maybe ShopButton
shopHitTest !mx !my
  | abs mx <= 130.0 && abs (my - (-275.0)) <= 20.0 = Just BtnShopBack
  | abs (mx - 500.0) <= 85.0 && abs (my - 310.0) <= 22.0 = Just BtnShopTestGold
  | otherwise =
      let checkHero (i, cls) =
            let cx = fromIntegral (i - 2) * 230.0
                cy = -180.0
            in if abs (mx - cx) <= 90.0 && abs (my - cy) <= 20.0
                 then Just (BtnShopBuy cls)
                 else Nothing
      in foldr (\it acc -> case checkHero it of Just b -> Just b; Nothing -> acc) Nothing (zip [(0 :: Int)..4] shopHeroes)

data RoomButton
  = BtnRoomStart
  | BtnRoomLeave
  | BtnRoomChatInput
  | BtnRoomChatSend
  deriving (Eq, Show, Enum, Bounded)

roomButtonRect :: RoomButton -> (Float, Float, Float, Float)
roomButtonRect btn = case btn of
  BtnRoomStart     -> ( 380.0, -140.0, 280.0, 42.0)
  BtnRoomLeave     -> ( 380.0, -205.0, 280.0, 34.0)
  BtnRoomChatInput -> (-200.0, -245.0, 510.0, 32.0)
  BtnRoomChatSend  -> ( 100.0, -245.0,  80.0, 32.0)

roomHitTest :: Float -> Float -> Maybe RoomButton
roomHitTest !mx !my = find inside [minBound .. maxBound]
  where
    inside b = let (cx, cy, w, h) = roomButtonRect b
               in abs (mx - cx) <= w / 2.0 && abs (my - cy) <= h / 2.0

data PickHeroButton
  = BtnPickHero !TankClass
  | BtnPickConfirm
  deriving (Eq, Show)

surrenderHitTest :: Float -> Float -> Bool
surrenderHitTest !mx !my =
  my >= 308.0 && my <= 344.0 && mx >= 220.0 && mx <= 370.0

data SurrenderModalButton = BtnSurrenderConfirm | BtnSurrenderCancel deriving (Eq, Show)

surrenderModalHitTest :: Float -> Float -> Maybe SurrenderModalButton
surrenderModalHitTest !mx !my
  | abs mx <= 210.0 && abs (my - (-20.0)) <= 22.0 = Just BtnSurrenderConfirm
  | abs mx <= 210.0 && abs (my - (-72.0)) <= 22.0 = Just BtnSurrenderCancel
  | otherwise = Nothing

drawSurrenderModal :: Float -> Picture
drawSurrenderModal !animTime =
  let !pulse = 0.5 + 0.5 * sin (animTime * 5.0)
  in pictures
    [ color (makeColorI 0 0 0 190) (rectangleSolid 1280.0 720.0)
    , drawPanel 560.0 250.0 (makeColorI 255 70 80 255)
    , translate 0.0 75.0 $ drawTextCenter 0.13 (makeColorI 255 90 90 255) "BẠN CÓ CHẮC CHẮN MUỐN ĐẦU HÀNG KHÔNG?"
    , translate 0.0 40.0 $ drawTextCenter 0.075 uiDim "Xe của bạn sẽ bị loại khỏi trận và tự động về sảnh sau 5 giây."
    , translate 0.0 (-20.0) $ pictures
        [ color (makeColorI 180 30 40 (round (200 + 55 * pulse))) (rectangleSolid 420.0 44.0)
        , color (makeColorI 255 90 100 255) (rectangleWire 420.0 44.0)
        , drawTextMid 0.09 white "[ ĐỒNG Ý ĐẦU HÀNG (Y / CLICK) ]"
        ]
    , translate 0.0 (-72.0) $ pictures
        [ color (makeColorI 30 45 65 255) (rectangleSolid 420.0 44.0)
        , color uiCyan (rectangleWire 420.0 44.0)
        , drawTextMid 0.09 uiCyan "[ HỦY - TIẾP TỤC CHƠI (N / ESC / CLICK) ]"
        ]
    ]

heroGridCells :: [(TankClass, Float, Float)]
heroGridCells =
  [ (cls, -500.0 + fromIntegral (i `mod` 5) * 120.0, if i < 5 then 65.0 else -85.0)
  | (i, cls) <- zip [0 :: Int ..] allHeroes ]

pickHeroHitTest :: PlayerAccount -> [TankClass] -> Float -> Float -> Maybe PickHeroButton
pickHeroHitTest !account !blocked !mx !my
  | abs (mx - 350.0) <= 165.0 && abs (my - (-200.0)) <= 25.0 = Just BtnPickConfirm
  | otherwise =
      case [ cls | (cls, cx, cy) <- heroGridCells, abs (mx - cx) <= 55.0, abs (my - cy) <= 65.0 ] of
        (cls : _) | cls `elem` accUnlockedHeroes account && cls `notElem` blocked -> Just (BtnPickHero cls)
        _ -> Nothing

-- Di chuyển con trỏ chọn tướng trên lưới 5x2, bỏ qua các tướng bị khóa hoặc chưa mở
moveHeroCursor :: [TankClass] -> TankClass -> Direction -> TankClass
moveHeroCursor !valid !cur !dir
  | null valid = cur
  | otherwise = case dir of
      East  -> firstValid (drop 1 (iterate nextClass cur))
      West  -> firstValid (drop 1 (iterate prevClass cur))
      North -> rowJump
      South -> rowJump
  where
    firstValid cands = case filter (`elem` valid) (take 10 cands) of
      (c : _) -> c
      []      -> cur
    idx = fromEnum cur
    rowJump =
      let target = toEnum ((idx + 5) `mod` 10)
      in if target `elem` valid then target else cur

atButton :: LobbyButton -> Picture -> Picture
atButton !b !pic = let (cx, cy, _, _) = lobbyButtonRect b in translate cx cy pic

drawBattlegroundDimmed :: Float -> Picture
drawBattlegroundDimmed animTime =
  pictures
    [ drawCyberBackground animTime
    , color (makeColorI 0 0 0 170) (rectangleSolid 1400.0 800.0)
    , translate 0.0 (sin (animTime * 1.5) * 180.0) $
        color (makeColorI 0 240 255 45) (rectangleSolid 1280.0 2.0)
    ]

drawLoginScreen
  :: Float
  -> String
  -> String
  -> InputField
  -> Maybe String
  -> Picture
drawLoginScreen !animTime !userInput !passInput !activeField !mErr =
  let pulse = 0.5 + 0.5 * sin (animTime * 3.0)
      passDisplay = replicate (length passInput) '*'
  in pictures
       [ drawBattlegroundDimmed animTime
       , translate 0.0 20.0 $ pictures
           [ drawPanel 440.0 380.0 uiCyan
           , translate 0.0 150.0 $ drawTextCenter 0.16 uiCyan "ĐĂNG NHẬP PHI CÔNG"
           , translate 0.0 122.0 $ drawTextCenter 0.075 uiDim "Tự động đăng ký nếu tài khoản mới - Bảo vệ bằng mật khẩu"

           , translate (-170.0) 75.0 $ drawVietnameseText 0.075 uiDim "TÊN ĐĂNG NHẬP (CALLSIGN)"
           , translate 0.0 45.0 $
               drawInputField (activeField == FieldLoginUser) userInput "COMMANDER" animTime 340.0

           , translate (-170.0) (-5.0) $ drawVietnameseText 0.075 uiDim "MẬT KHẨU (PASSWORD)"
           , translate 0.0 (-35.0) $
               drawInputField (activeField == FieldLoginPass) passDisplay "Nhập mật khẩu..." animTime 340.0

           , translate 0.0 (-105.0) $ pictures
               [ color (makeColorI 0 200 255 (round (40 + 40 * pulse))) (rectangleSolid 346.0 48.0)
               , color (makeColorI 0 145 210 255) (rectangleSolid 340.0 42.0)
               , color (makeColorI 180 245 255 255) (rectangleWire 340.0 42.0)
               , drawTextMid 0.11 white "ĐĂNG NHẬP / VÀO SẢNH [ENTER]"
               ]

           , case mErr of
               Just err -> translate 0.0 (-155.0) $ pictures
                 [ color (makeColorI 180 30 30 200) (rectangleSolid 380.0 26.0)
                 , drawTextCenter 0.075 white ("LỖI: " ++ err)
                 ]
               Nothing -> translate 0.0 (-155.0) $
                 drawTextCenter 0.072 uiDim "Nhấn [TAB] để đổi ô  |  Nhấn [ENTER] để đăng nhập"
           ]
       ]

drawLobbyScreen
  :: Float
  -> PlayerAccount
  -> String
  -> String
  -> InputField
  -> TankClass
  -> Maybe String
  -> (String, Float)
  -> Picture
drawLobbyScreen !animTime !account !host !_port !activeField !currentCls !_mErr !toast =
  let hInfo     = heroClassInfo currentCls
      primaryC  = heroPrimaryCol hInfo
      dummyTank = dummyTankOfClass currentCls
      heroIdx   = fromEnum currentCls
      heroCount = fromEnum (maxBound :: TankClass) + 1
      isOwned   = currentCls `elem` accUnlockedHeroes account
      callsign  = accCallsign account
      gold      = accGold account
      matches   = accMatchesPlayed account
  in pictures
       [ drawCyberBackground animTime

       , translate (-420.0) 310.0 $ pictures
           [ color (makeColorI 12 18 28 240) (rectangleSolid 380.0 50.0)
           , color uiCyan (rectangleWire 380.0 50.0)
           , translate (-160.0) 0.0 $ pictures
               [ color (makeColorI 0 200 255 200) (circleSolid 16.0)
               , color (makeColorI 10 20 30 255) (circleSolid 13.0)
               , drawTextMid 0.08 uiCyan "#"
               ]
           , translate (-130.0) 6.0 $ drawVietnameseText 0.11 uiCyan callsign
           , translate (-130.0) (-14.0) $ pictures
               [ drawVietnameseText 0.08 uiGold ("VÀNG: " ++ show gold ++ " G")
               , translate 120.0 0.0 $ drawVietnameseText 0.075 uiText ("TRẬN: " ++ show matches)
               ]
           ]

       , translate 210.0 310.0 $ pictures
           [ color (makeColorI 45 35 15 255) (rectangleSolid 140.0 40.0)
           , color uiGold (rectangleWire 140.0 40.0)
           , drawTextMid 0.088 uiGold "CỬA HÀNG [F3]"
           ]
       , translate 370.0 310.0 $ pictures
           [ color (makeColorI 18 42 28 255) (rectangleSolid 140.0 40.0)
           , color (makeColorI 50 220 120 255) (rectangleWire 140.0 40.0)
           , drawTextMid 0.088 (makeColorI 140 255 180 255) "SỰ KIỆN [F5]"
           ]
       , translate 530.0 310.0 $ pictures
           [ color (makeColorI 30 20 25 240) (rectangleSolid 130.0 40.0)
           , color (makeColorI 220 70 80 200) (rectangleWire 130.0 40.0)
           , drawTextMid 0.085 (makeColorI 255 160 160 255) "ĐĂNG XUẤT"
           ]

       , translate (-380.0) 105.0 $ pictures
           [ drawPanel 390.0 130.0 (makeColorI 0 200 255 255)
           , translate (-170.0) 40.0 $ drawVietnameseText 0.105 uiCyan "ĐẤU NGOẠI TUYẾN (OFFLINE)"
           , translate (-170.0) 18.0 $ drawVietnameseText 0.068 uiDim "Chơi không cần mạng - Đấu Bot AI hoặc 2 Người cùng máy"
           , translate (-92.0) (-22.0) $ pictures
               [ color (makeColorI 0 145 210 255) (rectangleSolid 170.0 42.0)
               , color (makeColorI 180 245 255 255) (rectangleWire 170.0 42.0)
               , drawTextMid 0.082 white "ĐẤU BOT 1v1 [F1]"
               ]
           , translate 92.0 (-22.0) $ pictures
               [ color (makeColorI 40 120 70 255) (rectangleSolid 170.0 42.0)
               , color (makeColorI 120 255 180 255) (rectangleWire 170.0 42.0)
               , drawTextMid 0.082 white "ĐỐI KHÁNG LOCAL [F2]"
               ]
           ]

       , translate (-380.0) (-95.0) $ pictures
           [ drawPanel 390.0 160.0 uiGold
           , translate (-170.0) 58.0 $ drawVietnameseText 0.105 uiGold "ĐẤU MẠNG LAN (LOCAL NETWORK)"
           , translate (-170.0) 36.0 $ drawVietnameseText 0.065 uiDim "Mã phòng 4 số - Tự động dò tìm máy chủ - Kết nối 2 máy"
           , translate (-95.0) (-8.0) $ pictures
               [ color (makeColorI 24 38 56 255) (rectangleSolid 175.0 46.0)
               , color (makeColorI 90 150 215 255) (rectangleWire 175.0 46.0)
               , drawTextMid 0.088 (makeColorI 200 230 255 255) "TẠO PHÒNG"
               ]
           , translate 95.0 (-8.0) $ pictures
               [ color (makeColorI 20 32 48 255) (rectangleSolid 175.0 46.0)
               , color (makeColorI 70 120 175 220) (rectangleWire 175.0 46.0)
               , drawTextMid 0.088 (makeColorI 180 215 245 255) "VÀO PHÒNG"
               ]
            , translate (-60.0) (-52.0) $ pictures
                [ color (makeColorI 12 18 28 255) (rectangleSolid 230.0 32.0)
                , color (if activeField == FieldServerHost then makeColorI 0 230 255 255 else makeColorI 60 90 125 220)
                        (rectangleWire 230.0 32.0)
                , if activeField == FieldServerHost
                    then color (withAlpha 0.12 uiCyan) (rectangleSolid 228.0 30.0)
                    else blank
                , translate 0.0 (-4.0) $
                    drawTextCenter 0.076 (if activeField == FieldServerHost then white else uiDim)
                      ("IP: " ++ host ++ (if activeField == FieldServerHost && floor (animTime * 2.5) `mod` 2 == (0 :: Int) then "|" else ""))
                ]
            , translate 115.0 (-52.0) $ pictures
                [ color (makeColorI 18 36 32 255) (rectangleSolid 110.0 32.0)
                , color (makeColorI 50 220 140 240) (rectangleWire 110.0 32.0)
                , drawTextMid 0.078 (makeColorI 120 255 180 255) "DÒ TÌM LAN"
                ]
           ]

       , translate 195.0 (-20.0) (drawPanel 620.0 470.0 primaryC)
       , translate (-90.0) 170.0 $ drawVietnameseText 0.17 primaryC (heroTitle hInfo)
       , translate (-90.0) 144.0 $ drawVietnameseText 0.085 uiDim (heroRole hInfo)
       , translate 482.0 172.0 $
           drawTextRight 0.08 uiDim ("HERO " ++ show (heroIdx + 1) ++ " / " ++ show heroCount)
       , translate 195.0 128.0 $ color (withAlpha 0.25 primaryC) (rectangleSolid 580.0 1.5)

       , translate 35.0 45.0 $ pictures
           [ color (withAlpha 0.10 primaryC) (circleSolid 78.0)
           , color (makeColorI 9 20 33 255) (circleSolid 66.0)
           , color (heroGlowCol hInfo) (thickCircle 66.0 2.5)
           , rotate (animTime * 40.0) $ color (withAlpha 0.6 primaryC) $ pictures
               [ rotate (fromIntegral i * 30.0) $ translate 0.0 73.0 (rectangleSolid 2.0 6.0)
               | i <- [0 .. 11 :: Int]
               ]
           , scale 2.1 2.1 (drawHeroTank3D animTime (animTime * 45.0) dummyTank)
           ]

       , atButton BtnPrevHero (drawKeyCap 36.0 30.0 0.0 "<")
       , atButton BtnNextHero (drawKeyCap 36.0 30.0 0.0 ">")
       , translate 35.0 (-60.0) $ drawTextMid 0.11 white (heroShortName currentCls)

       , translate 35.0 (-88.0) $
           if isOwned
             then drawTextCenter 0.08 (makeColorI 50 240 120 255) "[ ĐÃ SỞ HỮU ]"
             else drawTextCenter 0.08 uiGold ("CHƯA MỞ KHÓA - GIÁ: " ++ show (heroCost currentCls) ++ " VÀNG")

       , translate 210.0 95.0 $ pictures
           [ translate 0.0 (fromIntegral row * (-26.0)) (drawStatBar lbl val col)
           | (row, (lbl, val, col)) <- zip [0 :: Int ..]
               [ ("Máu (HP)",   heroStatHp hInfo,  makeColorI 40 220 80 255)
               , ("Tốc độ",     heroStatSpd hInfo, makeColorI 0 220 255 255)
               , ("Hỏa lực",    heroStatAtk hInfo, makeColorI 255 70 70 255)
               , ("Phòng thủ",  heroStatDef hInfo, makeColorI 255 200 20 255)
               ]
           ]
       , translate 210.0 (-17.0) $ drawVietnameseText 0.08 (makeColorI 255 140 140 255) (heroShotText hInfo)

       , translate 195.0 (-142.0) $ pictures
           [ color (makeColorI 17 24 38 255) (rectangleSolid 580.0 76.0)
           , translate (-290.0 + 2.0) 0.0 $ color (heroGlowCol hInfo) (rectangleSolid 4.0 76.0)
           , color (withAlpha 0.45 (heroGlowCol hInfo)) (rectangleWire 580.0 76.0)
           , translate (-270.0) 15.0 $
               drawVietnameseText 0.095 uiGold ("TUYỆT KỸ [H]:  " ++ heroUltName hInfo)
           , translate (-270.0) (-7.0) $ drawVietnameseText 0.078 uiText (heroUltDesc1 hInfo)
           , translate (-270.0) (-25.0) $ drawVietnameseText 0.078 uiText (heroUltDesc2 hInfo)
           ]

       , translate 195.0 (-215.0) $
           drawKeyHintRow [("<", ""), (">", "Đổi Hero"), ("F1", "Đấu Bot"), ("F2", "Local 2P"), ("F3", "Cửa hàng"), ("F5", "Sự kiện")]

       , if snd toast > 0.0 then drawToast toast else blank
       ]

drawShopScreen
  :: Float
  -> PlayerAccount
  -> (String, Float)
  -> Picture
drawShopScreen !animTime !account !toast =
  let !gold = accGold account
      !pulse = 0.5 + 0.5 * sin (animTime * 4.0)
      shopList = [StrikerRuby, AegisOnyx, TempestAzure, MirageAmethyst, EclipseObsidian]
      drawShopCard i cls =
        let !cx = fromIntegral (i - 2) * 230.0
            !cy = 10.0
            !hInfo = heroClassInfo cls
            !primaryC = heroPrimaryCol hInfo
            !descS = minimum [0.052, fitScale 0.052 184.0 (heroUltDesc1 hInfo), fitScale 0.052 184.0 (heroUltDesc2 hInfo)]
            !cost = heroCost cls
            !isOwned = cls `elem` accUnlockedHeroes account
            dummyTank = dummyTankOfClass cls
        in translate cx cy $ pictures
             [ drawPanel 215.0 460.0 (if isOwned then uiCyan else primaryC)
             , translate 0.0 195.0 $ drawTextCenter 0.12 (heroPrimaryCol hInfo) (heroShortName cls)
             , translate 0.0 172.0 $ drawTextCenter 0.075 uiDim (heroTitle hInfo)
             , translate 0.0 100.0 $ pictures
                 [ color (withAlpha 0.1 primaryC) (circleSolid 48.0)
                 , color (makeColorI 10 14 22 255) (circleSolid 42.0)
                 , color (heroGlowCol hInfo) (thickCircle 42.0 2.0)
                 , scale 1.4 1.4 (drawHeroTank3D animTime (animTime * 40.0) dummyTank)
                 ]
             , translate (-85.0) 25.0 $ pictures
                 [ translate 0.0 (fromIntegral row * (-18.0)) $ pictures
                     [ drawVietnameseText 0.068 uiDim lbl
                     , translate 95.0 4.0 (drawProgressBar 75.0 8.0 val col)
                     ]
                 | (row, (lbl, val, col)) <- zip [0 :: Int ..]
                     [ ("Máu",     heroStatHp hInfo,  makeColorI 40 220 80 255)
                     , ("Tốc độ",  heroStatSpd hInfo, makeColorI 0 220 255 255)
                     , ("Hỏa lực", heroStatAtk hInfo, makeColorI 255 70 70 255)
                     , ("Giáp",    heroStatDef hInfo, makeColorI 255 200 20 255)
                     ]
                 ]
             , translate 0.0 (-46.0) $ drawTextCenter 0.058 (makeColorI 255 140 140 255) (heroShotText hInfo)
             , translate 0.0 (-85.0) $ pictures
                 [ color (makeColorI 14 20 32 255) (rectangleSolid 200.0 66.0)
                 , color (withAlpha 0.4 (heroGlowCol hInfo)) (rectangleWire 200.0 66.0)
                 , translate 0.0 18.0 $ drawTextCenter 0.078 uiGold (heroUltName hInfo)
                 , translate (-92.0) 0.0 $ drawVietnameseText descS uiText (heroUltDesc1 hInfo)
                 , translate (-92.0) (-16.0) $ drawVietnameseText descS uiText (heroUltDesc2 hInfo)
                 ]
             , translate 0.0 (-138.0) $
                 if isOwned
                   then drawTextCenter 0.08 (makeColorI 50 240 120 255) "[ ĐÃ SỞ HỮU ]"
                   else drawTextCenter 0.09 uiGold (show cost ++ " VÀNG")
             , translate 0.0 (-180.0) $
                 if isOwned
                   then pictures
                          [ color (makeColorI 20 50 35 255) (rectangleSolid 180.0 34.0)
                          , color (makeColorI 50 200 100 200) (rectangleWire 180.0 34.0)
                          , drawTextMid 0.08 (makeColorI 160 255 190 255) "ĐÃ KÍCH HOẠT"
                          ]
                   else pictures
                          [ color (makeColorI 0 200 255 (round (35 + 40 * pulse))) (rectangleSolid 184.0 38.0)
                          , color (makeColorI 190 140 20 255) (rectangleSolid 180.0 34.0)
                          , color (makeColorI 255 230 100 255) (rectangleWire 180.0 34.0)
                          , drawTextMid 0.088 white ("MUA (" ++ show cost ++ " G)")
                          ]
             ]
  in pictures
       [ drawCyberBackground animTime
       , translate (-580.0) 310.0 $ drawVietnameseText 0.17 uiGold "CỬA HÀNG CHIẾN BINH XE TĂNG"
       , translate (-580.0) 285.0 $ drawVietnameseText 0.075 uiDim "Mở khóa tướng mới để đa dạng hóa chiến thuật thi đấu"
       , translate 320.0 310.0 $ pictures
           [ color uiPanel (rectangleSolid 160.0 36.0)
           , color uiGold (rectangleWire 160.0 36.0)
           , drawTextMid 0.095 uiGold ("VÀNG: " ++ show gold ++ " G")
           ]
       , translate 500.0 310.0 $ pictures
           [ color (makeColorI 0 200 255 (round (30 + 30 * pulse))) (rectangleSolid 174.0 40.0)
           , color (makeColorI 20 40 60 255) (rectangleSolid 170.0 36.0)
           , color (makeColorI 0 220 255 255) (rectangleWire 170.0 36.0)
           , drawTextMid 0.082 (makeColorI 160 240 255 255) "+10,000 VÀNG TEST"
           ]
       , pictures (zipWith drawShopCard [(0 :: Int)..4] shopList)
       , translate 0.0 (-275.0) $ pictures
           [ color (makeColorI 25 35 50 255) (rectangleSolid 260.0 34.0)
           , color (makeColorI 100 140 180 255) (rectangleWire 260.0 34.0)
           , drawTextMid 0.09 white "QUAY LẠI SẢNH [ESC]"
           ]
       , if snd toast > 0.0 then drawToast toast else blank
       ]

drawEventScreen :: Float -> PlayerAccount -> (String, Float) -> Picture
drawEventScreen !animTime !account !toast =
  let !gold = accGold account
      !matches = accMatchesPlayed account
      !pulse = 0.5 + 0.5 * sin (animTime * 4.0)

      drawMissionCard :: Int -> String -> String -> Int -> Bool -> Int -> Picture
      drawMissionCard i title desc req claimed rGold =
        let !cy = 95.0 - fromIntegral (i :: Int) * 100.0
            !isDone = matches >= req
            !ratio = min 1.0 (fromIntegral matches / fromIntegral req)
        in translate 0.0 cy $ pictures
             [ drawPanel 740.0 80.0 (if claimed then uiDim else if isDone then makeColorI 50 220 120 255 else uiCyan)
             , translate (-330.0) 0.0 $ pictures
                 [ color (makeColorI 20 30 45 255) (circleSolid 24.0)
                 , color (if isDone then makeColorI 50 220 120 255 else uiCyan) (thickCircle 24.0 2.0)
                 , drawTextMid 0.12 (if isDone then makeColorI 50 220 120 255 else uiCyan) (show req)
                 ]
             , translate (-280.0) 14.0 $ drawVietnameseText 0.10 white title
             , translate (-280.0) (-10.0) $ drawVietnameseText 0.072 uiDim desc
             , translate (-180.0) (-26.0) $
                 drawProgressBar 200.0 7.0 ratio (makeColorI 50 220 120 255)
             , translate (-60.0) (-27.0) $
                 drawTextMid 0.072 uiDim (show (min req matches) ++ "/" ++ show req)
             , translate 140.0 0.0 $ pictures
                 [ drawTextCenter 0.095 uiGold ("+" ++ show rGold ++ " VÀNG")
                 ]
             , translate 260.0 0.0 $
                 if claimed
                   then pictures
                          [ color (makeColorI 30 35 45 255) (rectangleSolid 140.0 36.0)
                          , color uiDim (rectangleWire 140.0 36.0)
                          , drawTextMid 0.075 uiDim "ĐÃ NHẬN"
                          ]
                   else if isDone
                     then pictures
                            [ color (makeColorI 0 200 100 (round (40 + 40 * pulse))) (rectangleSolid 146.0 42.0)
                            , color (makeColorI 30 180 80 255) (rectangleSolid 140.0 36.0)
                            , color (makeColorI 180 255 200 255) (rectangleWire 140.0 36.0)
                            , drawTextMid 0.082 white "NHẬN THƯỞNG"
                            ]
                     else pictures
                            [ color (makeColorI 20 25 35 255) (rectangleSolid 140.0 36.0)
                            , color (makeColorI 60 75 95 200) (rectangleWire 140.0 36.0)
                            , drawTextMid 0.075 uiDim ("CHƯA ĐẠT (" ++ show matches ++ "/" ++ show req ++ ")")
                            ]
             ]
  in pictures
       [ drawCyberBackground animTime
       , translate 0.0 270.0 $ drawTextCenter 0.22 uiGold "TRUNG TÂM SỰ KIỆN CHIẾN TRƯỜNG"
       , translate 0.0 240.0 $ drawTextCenter 0.082 uiDim
           ("Phi công: " ++ accCallsign account ++ "  |  Tổng số trận đã đấu: " ++ show matches ++ " trận  |  Số dư: " ++ show gold ++ " Vàng")
       , drawMissionCard 0 "CHIẾN BINH NHẬP CUỘC" "Hoàn thành 1 trận đấu (Offline hoặc Online)" 1 (accClaimedM1 account) 300
       , drawMissionCard 1 "CHUYÊN GIA TÁC CHIẾN" "Hoàn thành 5 trận đấu" 5 (accClaimedM2 account) 800
       , drawMissionCard 2 "BẬC THẦY ĐẤU TRƯỜNG" "Hoàn thành 10 trận đấu" 10 (accClaimedM3 account) 1500
       , translate 0.0 (-270.0) $ pictures
           [ color (makeColorI 25 35 50 255) (rectangleSolid 260.0 36.0)
           , color (makeColorI 100 140 180 255) (rectangleWire 260.0 36.0)
           , drawTextMid 0.09 white "QUAY LẠI SẢNH [ESC]"
           ]
       , if snd toast > 0.0 then drawToast toast else blank
       ]

drawJoinCodeModal :: Float -> String -> String -> InputField -> Maybe String -> Picture
drawJoinCodeModal !animTime !codeVal !host !activeField !mErr =
  pictures
    [ color (makeColorI 0 0 0 195) (rectangleSolid 1400.0 800.0)
    , drawPanel 450.0 320.0 uiCyan
    , translate 0.0 125.0 $ drawTextCenter 0.15 uiCyan "VÀO PHÒNG TRỰC TUYẾN"
    , translate 0.0 100.0 $ drawTextCenter 0.072 uiDim "Nhập mã phòng 4 số & IP máy chủ LAN do Chủ phòng cung cấp"
    , translate (-120.0) 66.0 $ drawVietnameseText 0.075 uiGold "MÃ PHÒNG (4 SỐ):"
    , translate 0.0 46.0 $
        drawInputField (activeField == FieldJoinCode) codeVal "VD: 7294" animTime 260.0
    , translate (-120.0) 12.0 $ drawVietnameseText 0.075 uiCyan "ĐỊA CHỈ IP MÁY CHỦ (LAN):"
    , translate (-60.0) (-12.0) $
        drawInputField (activeField == FieldServerHost) host "VD: 192.168.1.10" animTime 220.0
    , translate 115.0 (-12.0) $ pictures
        [ color (makeColorI 18 36 32 255) (rectangleSolid 110.0 38.0)
        , color (makeColorI 50 220 140 240) (rectangleWire 110.0 38.0)
        , drawTextMid 0.078 (makeColorI 120 255 180 255) "DÒ TÌM LAN"
        ]
    , translate (-80.0) (-70.0) $ pictures
        [ color (makeColorI 0 145 210 255) (rectangleSolid 140.0 38.0)
        , color (makeColorI 180 245 255 255) (rectangleWire 140.0 38.0)
        , drawTextMid 0.09 white "VÀO PHÒNG"
        ]
    , translate 80.0 (-70.0) $ pictures
        [ color (makeColorI 35 25 30 255) (rectangleSolid 140.0 38.0)
        , color (makeColorI 180 70 80 200) (rectangleWire 140.0 38.0)
        , drawTextMid 0.09 (makeColorI 255 160 160 255) "HỦY [ESC]"
        ]
    , case mErr of
        Just err -> translate 0.0 (-118.0) $ drawTextCenter 0.072 (makeColorI 255 70 70 255) ("Lỗi: " ++ err)
        Nothing  -> translate 0.0 (-118.0) $ drawTextCenter 0.068 uiDim "Bấm [TAB] chuyển ô  |  [F4] sửa IP  |  [ENTER] kết nối"
    ]

drawRoomScreen
  :: Float
  -> String
  -> String
  -> String
  -> Bool
  -> Bool
  -> [(String, Bool)]
  -> [String]
  -> String
  -> InputField
  -> Maybe String
  -> Picture
drawRoomScreen !animTime !roomCode !hostLanIp !myCallsign !isHost !isReady !playersWithReady !chatHistory !chatInput !activeField !mNotice =
  let !pulse = 0.5 + 0.5 * sin (animTime * 4.0)
      slotCount = length playersWithReady
      canStart = slotCount >= 2 && all snd (drop 1 playersWithReady)
      drawSlot i =
        let !cx = fromIntegral (i - 1) * 380.0
            !cy = 130.0
            !mPlayer = if i < slotCount then Just (playersWithReady !! i) else Nothing
        in translate cx cy $ pictures
             [ drawPanel 340.0 180.0 (if mPlayer /= Nothing then (if i == 0 then uiCyan else uiGold) else uiDim)
             , translate 0.0 60.0 $ drawTextCenter 0.09 uiDim ("VỊ TRÍ " ++ show (i + 1) ++ (if i == 0 then " (CHỦ PHÒNG)" else ""))
             , case mPlayer of
                 Just (name, pRdy) -> pictures
                   [ translate 0.0 12.0 $ drawTextCenter 0.16 (if name == myCallsign then uiGold else white) name
                   , translate 0.0 (-35.0) $
                       if i == 0
                         then statusTag 200.0 (makeColorI 20 50 60 255) uiCyan uiCyan "[ CHỦ PHÒNG ]"
                         else if pRdy
                           then statusTag 180.0 (makeColorI 20 60 40 255) (makeColorI 50 220 120 255) (makeColorI 120 255 180 255) "[ ĐÃ SẴN SÀNG ]"
                           else statusTag 180.0 (makeColorI 60 20 20 255) (makeColorI 255 80 80 255) (makeColorI 255 120 120 255) "[ CHƯA SẴN SÀNG ]"
                   ]
                 Nothing -> translate 0.0 (-10.0) $
                   drawTextCenter 0.095 uiDim "ĐANG CHỜ NGƯỜI CHƠI..."
             ]
      statusTag w fillC lineC textC txt = pictures
        [ color fillC (rectangleSolid w 24.0)
        , color lineC (rectangleWire w 24.0)
        , drawTextMid 0.075 textC txt
        ]
      bigButton fillC lineC label = pictures
        [ color (withAlpha (0.25 + 0.2 * pulse) lineC) (rectangleSolid 286.0 46.0)
        , color fillC (rectangleSolid 280.0 40.0)
        , color lineC (rectangleWire 280.0 40.0)
        , drawTextMid 0.09 white label
        ]
  in pictures
       [ drawCyberBackground animTime
       , translate 0.0 275.0 $ pictures
           [ drawTextCenter 0.19 uiGold ("MÃ PHÒNG: [ " ++ roomCode ++ " ]   |   IP LAN: [ " ++ hostLanIp ++ " ]")
           ]
       , translate 0.0 242.0 $ drawTextCenter 0.08 uiDim "Chia sẻ Mã phòng 4 số & IP LAN này để bạn bè trong cùng mạng LAN vào phòng"
       , pictures [drawSlot 0, drawSlot 1, drawSlot 2]
       , case mNotice of
           Just msg -> translate 0.0 15.0 $ pictures
             [ color (makeColorI 10 14 22 235) (rectangleSolid (textWidth 0.085 msg + 40.0) 30.0)
             , color uiGold (rectangleWire (textWidth 0.085 msg + 40.0) 30.0)
             , drawTextMid 0.085 uiGold msg
             ]
           Nothing -> blank
       , translate (-200.0) (-160.0) $ pictures
           [ drawPanel 540.0 220.0 uiDim
           , translate (-250.0) 90.0 $ drawVietnameseText 0.08 uiGold "KÊNH CHAT TRONG PHÒNG"
           , pictures
               [ translate (-250.0) (65.0 - fromIntegral idx * 22.0) $
                   drawVietnameseText 0.075 uiText msg
               | (idx, msg) <- zip [0..5 :: Int] (reverse (take 6 (reverse chatHistory)))
               ]
           , translate 0.0 (-85.0) $
               drawInputField (activeField == FieldChat) chatInput "Nhập tin nhắn rồi nhấn [ENTER]..." animTime 510.0
           ]
       , translate 380.0 (-160.0) $ pictures
           [ translate 0.0 20.0 $
               if isHost
                 then pictures
                        [ bigButton (if canStart then makeColorI 0 160 220 255 else makeColorI 40 55 75 255)
                                    (if canStart then makeColorI 180 240 255 255 else makeColorI 90 110 140 255)
                                    "[ BẮT ĐẦU VÀO TRẬN ]"
                        , translate 0.0 (-34.0) $
                            if canStart
                              then drawTextCenter 0.068 (makeColorI 100 255 180 255) "Tất cả đã sẵn sàng - bấm để chọn tướng"
                              else if slotCount < 2
                                then drawTextCenter 0.068 uiDim "Cần ít nhất 2 người chơi"
                                else drawTextCenter 0.068 (makeColorI 255 100 100 255) "Đang chờ khách bấm SẴN SÀNG..."
                        ]
                 else pictures
                        [ if isReady
                            then bigButton (makeColorI 25 140 70 255) (makeColorI 120 255 180 255) "[ ĐÃ SẴN SÀNG ]"
                            else bigButton (makeColorI 140 25 25 255) (makeColorI 255 120 120 255) "[ CHƯA SẴN SÀNG ]"
                        , translate 0.0 (-34.0) $
                            if isReady
                              then drawTextCenter 0.068 (makeColorI 120 255 180 255) "Chờ chủ phòng bắt đầu - bấm lần nữa để hủy"
                              else drawTextCenter 0.068 (makeColorI 255 120 120 255) "Bấm nút (hoặc ENTER) để báo SẴN SÀNG"
                        ]
           , translate 0.0 (-45.0) $ pictures
               [ color (makeColorI 40 20 25 255) (rectangleSolid 280.0 34.0)
               , color (makeColorI 200 60 70 200) (rectangleWire 280.0 34.0)
               , drawTextMid 0.085 (makeColorI 255 160 160 255) "RỜI PHÒNG [ESC]"
               ]
           ]
       ]

data PickCursor = PickCursor
  { pcLabel  :: !String
  , pcColor  :: !Color
  , pcHero   :: !TankClass
  , pcLocked :: !Bool
  }

playerSlotColor :: Int -> Color
playerSlotColor !i = case i of
  0 -> makeColorI 0 220 255 255
  1 -> makeColorI 255 200 40 255
  _ -> makeColorI 200 90 255 255

drawPickHeroScreen
  :: Float
  -> Float
  -> PlayerAccount
  -> TankClass
  -> [PickCursor]
  -> String
  -> String
  -> Bool
  -> Maybe String
  -> Picture
drawPickHeroScreen !animTime !countdown !account !previewCls !cursors !subTitle !actionTxt !actionDone !mNotice =
  let !pulse = 0.5 + 0.5 * sin (animTime * 6.0)
      !secs = max 0 (ceiling countdown :: Int)
      !isUrgent = secs <= 5
      !timerCol = if isUrgent then makeColorI 255 60 60 (round (200 + 55 * pulse)) else uiCyan
      !hInfo = heroClassInfo previewCls
      !primaryC = heroPrimaryCol hInfo
      dummyTank = dummyTankOfClass previewCls
      pad2 n = if n < 10 then "0" ++ show n else show n
      shortLabel n = if length n > 9 then take 9 n else n

      drawHeroGridCard (cls, cx, cy) =
        let !cHero = heroClassInfo cls
            !isOwned = cls `elem` accUnlockedHeroes account
            !cDummy = dummyTankOfClass cls
            here = [ c | c <- cursors, pcHero c == cls ]
            !isSel = not (null here)
            ring k c =
              let !g = 6.0 + 6.0 * fromIntegral (k :: Int)
                  !a = if pcLocked c then 0.75 else 0.3 + 0.25 * pulse
              in pictures
                   [ color (withAlpha a (pcColor c)) (rectangleSolid (110.0 + g) (130.0 + g))
                   , color (pcColor c) (rectangleWire (110.0 + g) (130.0 + g))
                   ]
            statusLine = case here of
              (c : _) -> drawTextCenter 0.06 (pcColor c)
                           (shortLabel (pcLabel c) ++ (if pcLocked c then " ĐÃ KHÓA" else " ĐANG CHỌN"))
              [] | not isOwned -> drawTextCenter 0.062 uiDim "CHƯA CÓ"
                 | otherwise   -> drawTextCenter 0.062 (makeColorI 100 210 140 255) "CÒN TRỐNG"
        in translate cx cy $ pictures
             ( reverse (zipWith ring [0 ..] here)
             ++ [ color (makeColorI 12 18 28 240) (rectangleSolid 110.0 130.0)
                , color (if isSel then white else if isOwned then heroPrimaryCol cHero else makeColorI 40 45 55 180)
                        (rectangleWire 110.0 130.0)
                , translate 0.0 46.0 $ drawTextCenter 0.07 (if isOwned then heroPrimaryCol cHero else uiDim) (heroShortName cls)
                , translate 0.0 6.0 (drawHeroTank3D animTime (if isSel then animTime * 45.0 else 0.0) cDummy)
                , translate 0.0 (-44.0) statusLine
                , if not isOwned then color (makeColorI 0 0 0 200) (rectangleSolid 110.0 130.0) else blank
                ] )
  in pictures
       [ drawCyberBackground animTime
       , translate 0.0 270.0 $ pictures
           [ color (makeColorI 10 15 25 240) (rectangleSolid 760.0 64.0)
           , color timerCol (rectangleWire 760.0 64.0)
           , translate 0.0 12.0 $ drawTextCenter 0.13 timerCol ("CHỌN TƯỚNG: 00:" ++ pad2 secs ++ "s")
           , translate 0.0 (-14.0) $ drawTextCenter 0.068 uiDim subTitle
           ]
       , pictures (map drawHeroGridCard heroGridCells)
       , translate (-260.0) (-190.0) $ pictures
           [ translate (fromIntegral k * 170.0 - fromIntegral (length cursors - 1) * 85.0) 0.0 $ pictures
               [ color (withAlpha 0.2 (pcColor c)) (rectangleSolid 160.0 26.0)
               , color (pcColor c) (rectangleWire 160.0 26.0)
               , drawTextMid 0.07 (pcColor c) (shortLabel (pcLabel c) ++ (if pcLocked c then ": ĐÃ KHÓA" else ": ĐANG CHỌN"))
               ]
           | (k, c) <- zip [0 :: Int ..] cursors ]
       , translate 350.0 100.0 $ pictures
           [ color (withAlpha 0.1 primaryC) (circleSolid 55.0)
           , color (heroGlowCol hInfo) (thickCircle 50.0 2.2)
           , scale 1.6 1.6 (drawHeroTank3D animTime (animTime * 45.0) dummyTank)
           ]
       , translate 240.0 20.0 $ pictures
           [ translate 0.0 (fromIntegral row * (-22.0)) (drawStatBar lbl val col)
           | (row, (lbl, val, col)) <- zip [0 :: Int ..]
               [ ("Máu",     heroStatHp hInfo,  makeColorI 40 220 80 255)
               , ("Tốc độ",  heroStatSpd hInfo, makeColorI 0 220 255 255)
               , ("Hỏa lực", heroStatAtk hInfo, makeColorI 255 70 70 255)
               , ("Giáp",    heroStatDef hInfo, makeColorI 255 200 20 255)
               ]
           ]
       , translate 240.0 (-78.0) $ drawVietnameseText 0.07 (makeColorI 255 140 140 255) (heroShotText hInfo)
       , translate 350.0 (-130.0) $ pictures
           [ color (makeColorI 15 22 35 255) (rectangleSolid 370.0 74.0)
           , color (withAlpha 0.5 (heroGlowCol hInfo)) (rectangleWire 370.0 74.0)
           , translate 0.0 18.0 $ drawTextCenter 0.085 uiGold ("TUYỆT KỸ: " ++ heroUltName hInfo)
           , translate (-170.0) (-2.0) $ drawVietnameseText 0.065 uiText (heroUltDesc1 hInfo)
           , translate (-170.0) (-20.0) $ drawVietnameseText 0.065 uiText (heroUltDesc2 hInfo)
           ]
       , translate 350.0 (-200.0) $ pictures
           [ color (if actionDone then makeColorI 30 180 80 255 else makeColorI 0 200 255 (round (40 + 40 * pulse)))
                   (rectangleSolid 336.0 46.0)
           , color (if actionDone then makeColorI 20 140 60 255 else makeColorI 0 150 215 255)
                   (rectangleSolid 330.0 40.0)
           , color (if actionDone then makeColorI 120 255 180 255 else makeColorI 170 240 255 255)
                   (rectangleWire 330.0 40.0)
           , drawTextMid 0.078 (if actionDone then makeColorI 220 255 230 255 else white) actionTxt
           ]
       , case mNotice of
           Just msg -> translate 0.0 205.0 $ pictures
             [ color (makeColorI 10 14 22 235) (rectangleSolid (textWidth 0.08 msg + 40.0) 28.0)
             , color uiGold (rectangleWire (textWidth 0.08 msg + 40.0) 28.0)
             , drawTextMid 0.08 uiGold msg
             ]
           Nothing -> blank
       ]

fitScale :: Float -> Float -> String -> Float
fitScale !s !maxW !str =
  let !w = textWidth s str
  in if w <= maxW || w <= 0.0 then s else s * maxW / w

cutToWidth :: Float -> Float -> String -> String
cutToWidth !s !maxW !str =
  case [ pre | n <- [length str, length str - 1 .. 0], let pre = take n str, textWidth s pre <= maxW ] of
    (pre : _) -> pre
    []        -> ""

localHeroGridCells :: [(TankClass, Float, Float)]
localHeroGridCells =
  [ (cls, -212.0 + fromIntegral (i `mod` 5) * 106.0, if i < 5 then 95.0 else -45.0)
  | (i, cls) <- zip [0 :: Int ..] allHeroes ]

localPickHitTest :: PlayerAccount -> [TankClass] -> Float -> Float -> Maybe PickHeroButton
localPickHitTest !account !blocked !mx !my
  | abs mx <= 240.0 && abs (my - (-200.0)) <= 22.0 = Just BtnPickConfirm
  | otherwise =
      case [ cls | (cls, cx, cy) <- localHeroGridCells, abs (mx - cx) <= 48.0, abs (my - cy) <= 58.0 ] of
        (cls : _) | cls `elem` accUnlockedHeroes account && cls `notElem` blocked -> Just (BtnPickHero cls)
        _ -> Nothing

drawLocal2PPickScreen
  :: Float
  -> Float
  -> PlayerAccount
  -> PickCursor
  -> PickCursor
  -> String
  -> Bool
  -> Picture
drawLocal2PPickScreen !animTime !countdown !account !p1 !p2 !actionTxt !bothDone =
  let !pulse = 0.5 + 0.5 * sin (animTime * 6.0)
      !secs = max 0 (ceiling countdown :: Int)
      !timerCol = if secs <= 5 then makeColorI 255 60 60 (round (200 + 55 * pulse)) else uiCyan
      pad2 n = if n < 10 then "0" ++ show n else show n
      cursors = [p1, p2]

      gridCard (cls, cx, cy) =
        let !cHero = heroClassInfo cls
            !isOwned = cls `elem` accUnlockedHeroes account
            here = [ c | c <- cursors, pcHero c == cls ]
            !isSel = not (null here)
            ring k c =
              let !g = 6.0 + 6.0 * fromIntegral (k :: Int)
                  !a = if pcLocked c then 0.75 else 0.3 + 0.25 * pulse
              in pictures
                   [ color (withAlpha a (pcColor c)) (rectangleSolid (96.0 + g) (116.0 + g))
                   , color (pcColor c) (rectangleWire (96.0 + g) (116.0 + g))
                   ]
            nameTxt = heroShortName cls
            statusLine = case here of
              (c : _) -> let t = pcLabel c ++ (if pcLocked c then " KHÓA" else " CHỌN")
                         in drawTextCenter (fitScale 0.06 86.0 t) (pcColor c) t
              [] | not isOwned -> drawTextCenter 0.056 uiDim "CHƯA CÓ"
                 | otherwise   -> drawTextCenter 0.056 (makeColorI 100 210 140 255) "CÒN TRỐNG"
        in translate cx cy $ pictures
             ( reverse (zipWith ring [0 ..] here)
             ++ [ color (makeColorI 12 18 28 240) (rectangleSolid 96.0 116.0)
                , color (if isSel then white else if isOwned then heroPrimaryCol cHero else makeColorI 40 45 55 180)
                        (rectangleWire 96.0 116.0)
                , translate 0.0 41.0 $ drawTextCenter (fitScale 0.064 86.0 nameTxt)
                    (if isOwned then heroPrimaryCol cHero else uiDim) nameTxt
                , translate 0.0 4.0 (scale 1.0 1.0 (drawHeroTank3D animTime (if isSel then animTime * 45.0 else 0.0) (dummyTankOfClass cls)))
                , translate 0.0 (-41.0) statusLine
                , if not isOwned then color (makeColorI 0 0 0 200) (rectangleSolid 96.0 116.0) else blank
                ] )

      statRow (lbl, val, col) =
        pictures
          [ translate (-125.0) (-4.0) $ drawVietnameseText 0.068 uiDim lbl
          , translate 45.0 0.0 (drawProgressBar 150.0 11.0 val col)
          ]

      sidePanel px c keysTxt =
        let !hInfo = heroClassInfo (pcHero c)
            !pc = pcColor c
            ultTitle = "TUYỆT KỸ: " ++ heroUltName hInfo
            d1 = heroUltDesc1 hInfo
            d2 = heroUltDesc2 hInfo
            !descS = minimum [0.06, fitScale 0.06 250.0 d1, fitScale 0.06 250.0 d2]
            statusTxt = if pcLocked c then "ĐÃ KHÓA" else "ĐANG CHỌN"
            !statusA = if pcLocked c then 0.55 else 0.15 + 0.15 * pulse
        in translate px (-20.0) $ pictures
             [ color (makeColorI 10 15 25 235) (rectangleSolid 290.0 500.0)
             , color (withAlpha 0.85 pc) (rectangleWire 290.0 500.0)
             , translate 0.0 225.0 $ pictures
                 [ color (withAlpha 0.18 pc) (rectangleSolid 286.0 46.0)
                 , translate 0.0 6.0 $ drawTextCenter 0.1 pc (pcLabel c)
                 , translate 0.0 (-14.0) $ drawTextCenter (fitScale 0.058 270.0 keysTxt) uiDim keysTxt
                 ]
             , translate 0.0 150.0 $ pictures
                 [ color (withAlpha 0.1 (heroPrimaryCol hInfo)) (circleSolid 48.0)
                 , color (heroGlowCol hInfo) (thickCircle 44.0 2.0)
                 , scale 1.35 1.35 (drawHeroTank3D animTime (animTime * 45.0) (dummyTankOfClass (pcHero c)))
                 ]
             , translate 0.0 88.0 $ drawTextCenter (fitScale 0.09 270.0 (heroTitle hInfo)) (heroPrimaryCol hInfo) (heroTitle hInfo)
             , translate 0.0 66.0 $ drawTextCenter (fitScale 0.062 270.0 (heroRole hInfo)) uiText (heroRole hInfo)
             , pictures
                 [ translate 0.0 (42.0 - fromIntegral row * 21.0) (statRow s)
                 | (row, s) <- zip [0 :: Int ..]
                     [ ("MÁU",     heroStatHp hInfo,  makeColorI 40 220 80 255)
                     , ("TỐC ĐỘ",  heroStatSpd hInfo, makeColorI 0 220 255 255)
                     , ("HỎA LỰC", heroStatAtk hInfo, makeColorI 255 70 70 255)
                     , ("GIÁP",    heroStatDef hInfo, makeColorI 255 200 20 255)
                     ]
                 ]
             , translate 0.0 (-46.0) $ drawTextCenter 0.062 (makeColorI 255 140 140 255) (heroShotText hInfo)
             , translate 0.0 (-96.0) $ pictures
                 [ color (makeColorI 15 22 35 255) (rectangleSolid 266.0 84.0)
                 , color (withAlpha 0.5 (heroGlowCol hInfo)) (rectangleWire 266.0 84.0)
                 , translate 0.0 22.0 $ drawTextCenter (fitScale 0.072 250.0 ultTitle) uiGold ultTitle
                 , translate (-125.0) (-6.0) $ drawVietnameseText descS uiText d1
                 , translate (-125.0) (-26.0) $ drawVietnameseText descS uiText d2
                 ]
             , translate 0.0 (-180.0) $ pictures
                 [ color (withAlpha statusA pc) (rectangleSolid 200.0 32.0)
                 , color pc (rectangleWire 200.0 32.0)
                 , drawTextMid 0.085 (if pcLocked c then white else pc) statusTxt
                 ]
             , translate 0.0 (-222.0) $ drawTextCenter 0.058 uiDim
                 (if pcLocked c then "ĐỢI NGƯỜI CÒN LẠI KHÓA" else "BẤM PHÍM KHÓA ĐỂ CHỐT TƯỚNG")
             ]
  in pictures
       [ drawCyberBackground animTime
       , translate 0.0 285.0 $ pictures
           [ color (makeColorI 10 15 25 240) (rectangleSolid 600.0 60.0)
           , color timerCol (rectangleWire 600.0 60.0)
           , translate 0.0 10.0 $ drawTextCenter 0.12 timerCol ("CHỌN TƯỚNG: 00:" ++ pad2 secs ++ "s")
           , translate 0.0 (-15.0) $ drawTextCenter 0.062 uiDim "ĐỐI KHÁNG 2 NGƯỜI - KHÔNG CHỌN TRÙNG TƯỚNG"
           ]
       , pictures (map gridCard localHeroGridCells)
       , translate 0.0 (-140.0) $ pictures
           [ translate (-110.0) 0.0 $ pictures
               [ color (pcColor p1) (rectangleSolid 14.0 14.0)
               , translate 14.0 (-4.0) $ drawVietnameseText 0.06 uiDim "VIỀN XANH = P1" ]
           , translate 20.0 0.0 $ pictures
               [ color (pcColor p2) (rectangleSolid 14.0 14.0)
               , translate 14.0 (-4.0) $ drawVietnameseText 0.06 uiDim "VIỀN VÀNG = P2" ]
           ]
       , translate 0.0 (-200.0) $ pictures
           [ color (if bothDone then makeColorI 30 180 80 255 else makeColorI 0 200 255 (round (40 + 40 * pulse)))
                   (rectangleSolid 486.0 46.0)
           , color (if bothDone then makeColorI 20 140 60 255 else makeColorI 0 150 215 255)
                   (rectangleSolid 480.0 40.0)
           , color (if bothDone then makeColorI 120 255 180 255 else makeColorI 170 240 255 255)
                   (rectangleWire 480.0 40.0)
           , drawTextMid (fitScale 0.078 460.0 actionTxt) (if bothDone then makeColorI 220 255 230 255 else white) actionTxt
           ]
       , sidePanel (-480.0) p1 "W A S D: DỜI  |  G / H: KHÓA"
       , sidePanel 480.0 p2 "MŨI TÊN: DỜI  |  1 / 2: KHÓA"
       ]

drawInputField :: Bool -> String -> String -> Float -> Float -> Picture
drawInputField !isActive !val !placeholder !animTime !w =
  let blinkCursor = isActive && (floor (animTime * 3.0) `mod` 2 == (0 :: Int))
      showHolder  = null val
      displayText = if showHolder then placeholder else val
      textColor   = if showHolder then makeColorI 90 110 140 255 else white
      borderColor = if isActive then uiCyan else makeColorI 60 80 110 220
      txtS        = 0.1
      leftX       = -w / 2.0 + 12.0
  in pictures
       [ if isActive then color (withAlpha 0.18 uiCyan) (rectangleSolid (w + 6.0) 40.0) else blank
       , color (makeColorI 9 13 21 255) (rectangleSolid w 34.0)
       , color borderColor (rectangleWire w 34.0)
       , if isActive then translate (-w / 2.0 + 1.5) 0.0 $ color uiCyan (rectangleSolid 3.0 34.0) else blank
       , translate leftX (-5.0) $ drawVietnameseText txtS textColor displayText
       , if blinkCursor
           then let cursorX = leftX + (if showHolder then 0.0 else textWidth txtS val) + 3.0
                in translate cursorX 0.0 $ color uiCyan (rectangleSolid 2.0 16.0)
           else blank
       ]

drawStatBar :: String -> Float -> Color -> Picture
drawStatBar !label !ratio !barCol =
  pictures
    [ translate 0.0 (-4.0) $ drawVietnameseText 0.08 uiDim label
    , translate 190.0 0.0 (drawProgressBar 160.0 12.0 ratio barCol)
    ]

drawCyberBackground :: Float -> Picture
drawCyberBackground !animTime =
  pictures
    [ color (makeColorI 6 9 15 255) (rectangleSolid 1400.0 800.0)
    , color (makeColorI 0 120 200 10) (circleSolid 520.0)
    , color (makeColorI 0 120 200 10) (circleSolid 360.0)
    , pictures
        [ translate x 0.0 $ color (makeColorI 0 180 255 16) (rectangleSolid 1.0 800.0)
        | i <- [-20 .. 20 :: Int]
        , let x = fromIntegral i * 35.0
        ]
    , pictures
        [ translate 0.0 y $ color (makeColorI 0 180 255 16) (rectangleSolid 1400.0 1.0)
        | j <- [-12 .. 12 :: Int]
        , let y = fromIntegral j * 35.0 + fromIntegral (floor (animTime * 15.0) `mod` (35 :: Int) :: Int)
        ]
    ]

-- Dựng toàn bộ khung hình chiến trường 32x22 và giao diện HUD trong trận
draw3DPlayingScene
  :: Float
  -> Int
  -> Bool
  -> Int
  -> GameState
  -> Map.Map Int (Float, Float)
  -> Map.Map Int Float
  -> [Particle]
  -> [Shockwave]
  -> [DustParticle]
  -> Bool
  -> Map.Map String Float
  -> Float
  -> (String, Float)
  -> [KillNotice]
  -> Maybe MatchResult
  -> Float
  -> Picture
draw3DPlayingScene !animTime !myId !isLocal2P !gold !st !visualTanks !visualAngles !particles !shockwaves !dustParticles !isMuted !keyGlow !mapBanner !toast !killFeed !mResult !exitCountdown =
  pictures
    [ drawArenaFloor animTime
    , pictures (map (drawEffectTile animTime) (Map.toList (effectTiles st)))
    , pictures (map drawDustParticle dustParticles)
    , pictures (map (drawPowerUp3D animTime) (powerUps st))
    , pictures (map drawWall3D (mapWalls (currentMap st)))
    , drawSkillAreas animTime st
    , pictures (map (drawBullet3D animTime (Map.map tankClass (tanks st))) (bullets st))

    , pictures
        [ let (!sx, !sy) = Map.findWithDefault (toScreenPos (tankPos t)) (tankId t) visualTanks
              !curAngle  = Map.findWithDefault (targetAngle (tankDir t)) (tankId t) visualAngles
              !isMe      = tankId t == myId && not isLocal2P
              !isAlive   = tankRespawnTimer t <= 0
              !hidden    = tankStealthTimer t > 0 && not isMe && not isLocal2P
              !prefix    | isLocal2P && tankId t == 1 = "P1: "
                         | isLocal2P && tankId t == 2 = "P2: "
                         | isMe                       = "BẠN: "
                         | otherwise                  = ""
              !tagCol    | isLocal2P && tankId t == 1 = uiCyan
                         | isLocal2P && tankId t == 2 = uiGold
                         | isMe                       = makeColorI 0 255 180 255
                         | otherwise                  = makeColorI 255 110 110 255
          in translate sx sy $ pictures
               [ if (isMe || isLocal2P) && isAlive then drawMyTankMarker animTime else blank
               , drawHeroTank3D animTime curAngle t
               , if isAlive && not hidden
                   then drawPlayerTag animTime (isMe || isLocal2P) tagCol prefix (playerNameOf st (tankId t))
                   else blank
               ]
        | t <- Map.elems (tanks st)
        ]

    , pictures (map drawParticle particles)
    , pictures (map (drawShockwave animTime) shockwaves)
    , drawInGameHUD animTime myId isLocal2P gold st isMuted keyGlow
    , drawKillFeed animTime killFeed
    , maybe blank (drawKillBanner animTime) (killBannerFor myId isLocal2P st)
    , if mapBanner > 0.0 then drawMapBanner mapBanner (currentMap st) else blank
    , if snd toast > 0.0 && mapBanner <= 0.0 then drawToast toast else blank
    , case (mResult, gsNotice st) of
        (Nothing, Just notice) -> drawNoticeBar (-240.0) notice
        _                      -> blank
    , case mResult of
        Just res -> drawMatchSummary animTime res exitCountdown
        Nothing  -> blank
    ]

drawToast :: (String, Float) -> Picture
drawToast (!msg, !remaining) =
  let !a = max 0.0 (min 1.0 (remaining / 0.5))
      !w = textWidth 0.11 msg + 40.0
  in translate 0.0 250.0 $ pictures
       [ color (withAlpha (0.85 * a) uiPanelDark) (rectangleSolid w 34.0)
       , color (withAlpha a uiGold) (rectangleWire w 34.0)
       , drawTextMid 0.11 (withAlpha a uiGold) msg
       ]

drawMapBanner :: Float -> MapId -> Picture
drawMapBanner !remaining !m =
  let !a     = max 0.0 (min 1.0 (remaining / 0.6))
      fade c = withAlpha a c
  in translate 0.0 120.0 $ pictures
       [ color (withAlpha (0.82 * a) uiPanelDark) (rectangleSolid 520.0 118.0)
       , translate 0.0 58.0 $ color (fade uiCyan) (rectangleSolid 520.0 2.0)
       , translate 0.0 (-58.0) $ color (fade uiCyan) (rectangleSolid 520.0 2.0)
       , translate 0.0 30.0 $ drawTextCenter 0.09 (fade uiDim) "BẢN ĐỒ TRẬN NÀY"
       , translate 0.0 (-14.0) $ drawTextCenter 0.3 (fade uiGold) (mapName m)
       , translate 0.0 (-42.0) $ drawTextCenter 0.08 (fade uiText) "XUẤT PHÁT NGẪU NHIÊN TẠI 1 TRONG 4 GÓC BẢN ĐỒ"
       ]

drawPlayerTag :: Float -> Bool -> Color -> String -> String -> Picture
drawPlayerTag !animTime !withArrow !accent !prefix !name =
  let !pulse = 0.5 + 0.5 * sin (animTime * 6.0)
      !s = 0.065
      !tagText = prefix ++ name
      !w = textWidth s tagText + 12.0
      !badgeY = 44.0
  in pictures
       [ if withArrow
           then translate 0.0 36.0 $ color (withAlpha (0.75 + 0.25 * pulse) accent) $
                  polygon [(-4.0, 4.0), (4.0, 4.0), (0.0, 0.0)]
           else blank
       , translate 0.0 badgeY $ pictures
           [ color (makeColorI 6 12 18 220) (rectangleSolid w 14.0)
           , color (withAlpha (0.6 + 0.35 * pulse) accent) (rectangleWire w 14.0)
           , translate (-w / 2.0 + 6.0) (-3.2) $ pictures
               [ drawVietnameseText s (makeColorI 255 210 50 255) prefix
               , translate (textWidth s prefix) 0.0 $ drawVietnameseText s (makeColorI 225 240 250 255) name
               ]
           ]
       ]

drawMyTankMarker :: Float -> Picture
drawMyTankMarker !animTime =
  let !r = 16.5 + 1.2 * sin (animTime * 5.0)
      !pulse = 0.5 + 0.5 * sin (animTime * 6.0)
  in pictures
       [ color (makeColorI 0 255 180 (round (35 + 25 * pulse))) (circleSolid r)
       , color (makeColorI 0 255 180 180) (thickCircle r 1.5)
       , rotate (animTime * 35.0) $ pictures
           [ rotate (fromIntegral i * 90.0) $
               translate 0.0 (r + 1.0) (rectangleSolid 2.0 4.0)
           | i <- [0 .. 3 :: Int]
           ]
       ]

drawArenaFloor :: Float -> Picture
drawArenaFloor !animTime =
  let !aw = arenaHalfW * 2.0
      !ah = arenaHalfH * 2.0
      !glowA = 0.55 + 0.25 * sin (animTime * 2.0)
      cornerMark sx sy = translate (sx * (arenaHalfW + 5.0)) (sy * (arenaHalfH + 5.0)) $
        color uiCyan $ pictures
          [ translate (-sx * 11.0) 0.0 (rectangleSolid 22.0 3.0)
          , translate 0.0 (-sy * 11.0) (rectangleSolid 3.0 22.0)
          ]
  in pictures
       [
         color (makeColorI 5 7 12 255) (rectangleSolid 1400.0 800.0)
       , pictures
           [ translate (fromIntegral i * 40.0) 0.0 $ color (makeColorI 0 160 255 9) (rectangleSolid 1.0 800.0)
           | i <- [-17 .. 17 :: Int] ]
       , pictures
           [ translate 0.0 (fromIntegral j * 40.0) $ color (makeColorI 0 160 255 9) (rectangleSolid 1400.0 1.0)
           | j <- [-10 .. 10 :: Int] ]

       , translate 5.0 (-7.0) $ color (makeColorI 0 0 0 160) (rectangleSolid aw ah)
       , color (makeColorI 15 20 30 255) (rectangleSolid aw ah)

       , pictures
           [ translate x y $ color (makeColorI 20 27 40 255) (rectangleSolid cellSize cellSize)
           | ix <- [0 .. gridWidth - 1]
           , iy <- [0 .. gridHeight - 1]
           , even (ix + iy)
           , let (x, y) = toScreenPos (Pos ix iy)
           ]

       , pictures
           [ translate (-arenaHalfW + fromIntegral i * cellSize) 0.0 $
               color (makeColorI 40 52 72 110) (rectangleSolid 1.0 ah)
           | i <- [1 .. gridWidth - 1] ]
       , pictures
           [ translate 0.0 (-arenaHalfH + fromIntegral j * cellSize) $
               color (makeColorI 40 52 72 110) (rectangleSolid aw 1.0)
           | j <- [1 .. gridHeight - 1] ]

       , color (withAlpha (glowA * 0.35) uiCyan) (rectangleWire (aw + 8.0) (ah + 8.0))
       , color (withAlpha glowA uiCyan) (rectangleWire (aw + 2.0) (ah + 2.0))
       , cornerMark 1.0 1.0, cornerMark (-1.0) 1.0, cornerMark 1.0 (-1.0), cornerMark (-1.0) (-1.0)
       ]

drawWall3D :: Position -> Picture
drawWall3D !pos =
  let (!x, !y) = toScreenPos pos
      !w = cellSize - 2.0
      !lift = 4.0
  in translate x y $ pictures
       [ translate 3.0 (-4.0) $ color (makeColorI 0 0 0 150) (rectangleSolid w w)
       , translate 0.0 (-lift / 2.0) $ color (makeColorI 24 30 42 255) (rectangleSolid w w)
       , translate 0.0 (lift / 2.0) $ pictures
           [ color (makeColorI 62 76 98 255) (rectangleSolid w (w - lift))
           , translate 0.0 ((w - lift) / 2.0 - 1.5) $
               color (makeColorI 150 175 210 160) (rectangleSolid (w - 2.0) 2.0)
           , color (makeColorI 95 115 145 200) (rectangleWire (w - 5.0) (w - lift - 5.0))
           , pictures
               [ translate (sx * (w / 2.0 - 4.5)) (sy * ((w - lift) / 2.0 - 4.5)) $
                   color (makeColorI 28 36 50 255) (circleSolid 1.6)
               | sx <- [-1.0, 1.0], sy <- [-1.0, 1.0] ]
           ]
       ]
drawBullet3D :: Float -> Map.Map Int TankClass -> Bullet -> Picture
drawBullet3D !animTime !owners !b =
  let (!x, !y)  = toScreenPos (bulletPos b)
      !cls      = Map.findWithDefault VanguardCyan (bulletOwner b) owners
      !dirAngle = targetAngle (bulletDir b)
      !body | bulletSkill b = drawSkillShell animTime cls
            | otherwise     = drawShotStyle animTime (specShotStyle (heroSpec cls))
      !pierceRing = if bulletPierce b && not (bulletSkill b)
                      then color (makeColorI 255 70 170 170) (thickCircle 5.5 1.2)
                      else blank
  in translate x y $ rotate dirAngle $ pictures [body, pierceRing]

drawShotStyle :: Float -> ShotStyle -> Picture
drawShotStyle !animTime !style = case style of
  ShotIce ->
    pictures
      [ translate 0.0 (-5.0) $ color (makeColorI 120 220 255 90) (rectangleSolid 2.0 6.0)
      , color (makeColorI 90 200 255 230) (polygon [(0, 5), (2.6, 0), (0, -4), (-2.6, 0)])
      , color (makeColorI 230 250 255 255) (polygon [(0, 3), (1.2, 0), (0, -2), (-1.2, 0)])
      ]
  ShotHeavy ->
    pictures
      [ translate 0.0 (-5.5) $ color (makeColorI 255 90 30 110) (rectangleSolid 3.0 5.0)
      , color (makeColorI 200 30 25 255) (circleSolid 4.2)
      , color (makeColorI 255 150 60 255) (circleSolid 2.6)
      , color (makeColorI 255 235 180 255) (circleSolid 1.1)
      ]
  ShotNeedle ->
    pictures
      [ translate 0.0 (-5.0) $ color (makeColorI 60 255 120 90) (rectangleSolid 1.4 6.0)
      , color (makeColorI 40 220 90 255) (rectangleSolid 1.6 8.0)
      , translate 0.0 4.5 $ color (makeColorI 200 255 200 255) (polygon [(-1.2, 0), (1.2, 0), (0, 2.5)])
      ]
  ShotSolar ->
    pictures
      [ color (makeColorI 255 220 60 70) (circleSolid 4.5)
      , color (makeColorI 255 215 40 255) (circleSolid 2.8)
      , color white (circleSolid 1.3)
      ]
  ShotVoid ->
    let !r = 3.0 + 0.5 * sin (animTime * 20.0)
    in pictures
         [ color (makeColorI 170 60 255 120) (thickCircle r 1.4)
         , color (makeColorI 60 10 90 255) (circleSolid 2.2)
         , color (makeColorI 220 170 255 255) (circleSolid 0.9)
         ]
  ShotRocket ->
    pictures
      [ translate 0.0 (-5.0) $ color (makeColorI 255 140 30 200) (rectangleSolid 2.2 (3.0 + sin (animTime * 40.0)))
      , color (makeColorI 60 60 70 255) (rectangleSolid 2.8 6.0)
      , translate 0.0 3.2 $ color (makeColorI 245 45 75 255) (polygon [(-1.4, 0), (1.4, 0), (0, 2.2)])
      ]
  ShotQuartz ->
    rotate 45.0 $ pictures
      [ color (makeColorI 120 135 170 255) (rectangleSolid 4.6 4.6)
      , color (makeColorI 225 235 255 255) (rectangleWire 4.6 4.6)
      , color (makeColorI 245 250 255 255) (rectangleSolid 1.6 1.6)
      ]
  ShotArc ->
    pictures
      [ color (makeColorI 80 230 255 230) (line [(0, -5), (1.6, -2), (-1.6, 1), (0, 4)])
      , color (makeColorI 220 255 255 255) (circleSolid 1.5)
      ]
  ShotPrism ->
    pictures
      [ color (makeColorI 255 120 220 200) (polygon [(0, 4.5), (3.0, -2.5), (-3.0, -2.5)])
      , color (makeColorI 140 220 255 220) (polygon [(0, 2.5), (1.6, -1.2), (-1.6, -1.2)])
      , color white (circleSolid 0.8)
      ]
  ShotDark ->
    pictures
      [ color (makeColorI 255 130 40 220) (thickCircle 3.4 1.2)
      , color (makeColorI 20 8 30 255) (circleSolid 2.8)
      , color (makeColorI 180 90 255 255) (circleSolid 0.9)
      ]

drawSkillShell :: Float -> TankClass -> Picture
drawSkillShell !animTime !cls = case cls of
  TitanCrimson ->
    pictures
      [ translate 0.0 (-8.0) $ color (makeColorI 255 140 30 170) (rectangleSolid 3.5 6.0)
      , color (makeColorI 255 90 20 190) (thickCircle 7.0 1.8)
      , color (makeColorI 220 45 15 255) (circleSolid 5.5)
      , color (makeColorI 255 200 40 255) (circleSolid 3.2)
      , color white (circleSolid 1.4)
      ]
  _ ->
    let !flame = 4.0 + sin (animTime * 40.0) * 1.5
    in pictures
         [ translate 0.0 (-7.0) $ pictures
             [ color (makeColorI 255 110 20 220) (rectangleSolid 3.0 flame)
             , color (makeColorI 255 230 60 255) (rectangleSolid 1.5 (flame * 0.6))
             ]
         , color (makeColorI 45 48 55 255) (rectangleSolid 4.0 9.0)
         , color (makeColorI 240 30 50 255) (rectangleSolid 2.6 7.0)
         , translate 0.0 5.5 $ color (makeColorI 255 230 30 255) (circleSolid 2.0)
         ]

-- Vẽ vùng tác dụng của tuyệt kỹ khớp với tập ô tính sát thương trong GameLogic
drawSkillAreas :: Float -> GameState -> Picture
drawSkillAreas !animTime !st = pictures (map areaOf (Map.elems (tanks st)))
  where
    m = currentMap st
    ringPx r = (fromIntegral r + 0.5) * cellSize
    areaOf t
      | tankRespawnTimer t > 0 = blank
      | otherwise = case tankClass t of
          SolarisGold | tankLaserTimer t > 0 ->
            let a = fromIntegral (tankLaserTimer t) / 15.0
            in cellsPicture (withAlpha (0.75 * a) (makeColorI 255 220 40 255)) (withAlpha a white)
                 (ultLineCells m SolarisGold (tankDir t) (tankPos t)) (tankDir t)
          MirageAmethyst | tankLaserTimer t > 0 ->
            let a = fromIntegral (tankLaserTimer t) / 15.0
                (cx, cy) = toScreenPos (tankPos t)
            in pictures
                 [ pictures [ cellsPicture (withAlpha (0.7 * a) (makeColorI 225 80 255 255)) (withAlpha a (makeColorI 255 220 255 255))
                                (ultLineCells m MirageAmethyst d (tankPos t)) d
                            | d <- [North, East, South, West] ]
                 , translate cx cy $ color (withAlpha (0.6 * a) (makeColorI 225 80 255 255)) (thickCircle (ringPx (1 :: Int)) 1.5)
                 ]
          TempestAzure | tankLightningTimer t > 0 ->
            let (cx, cy) = toScreenPos (tankPos t)
                a = fromIntegral (tankLightningTimer t) / 20.0
                targets = [ o | o <- Map.elems (tanks st), tankId o /= tankId t, tankRespawnTimer o <= 0
                              , ultTouches st t (tankPos o) ]
            in pictures
                 ( translate cx cy (color (withAlpha (0.55 * a) (makeColorI 80 220 255 255)) (thickCircle (ringPx (ultRangeOf t)) 1.5))
                 : [ lightningBolt animTime (cx, cy) (toScreenPos (tankPos o)) a | o <- targets ] )
          EclipseObsidian | tankBlackHoleTimer t > 0 ->
            let (cx, cy) = toScreenPos (tankPos t)
                a = fromIntegral (tankBlackHoleTimer t) / 45.0
                r = ringPx (ultRangeOf t)
            in translate cx cy $ rotate (animTime * 90.0) $ pictures
                 [ color (withAlpha (0.18 * a) (makeColorI 90 20 140 255)) (circleSolid r)
                 , color (withAlpha (0.6 * a) (makeColorI 190 60 255 255)) (thickCircle r 1.5)
                 , pictures [ rotate (fromIntegral i * 60.0) $ translate 0.0 (r * 0.6) $
                                color (withAlpha (0.5 * a) (makeColorI 255 130 40 255)) (rectangleSolid 1.5 (r * 0.3))
                            | i <- [0 .. 5 :: Int] ]
                 ]
          _ -> blank

    cellsPicture outer inner cells d = pictures
      [ translate px py $ rotate (targetAngle d) $ pictures
          [ color outer (rectangleSolid 8.0 cellSize)
          , color inner (rectangleSolid 3.0 cellSize) ]
      | c <- cells, let (px, py) = toScreenPos c ]

lightningBolt :: Float -> (Float, Float) -> (Float, Float) -> Float -> Picture
lightningBolt !animTime (!x1, !y1) (!x2, !y2) !a =
  let n = 6 :: Int
      jitter k = 5.0 * sin (animTime * 50.0 + fromIntegral k * 2.3)
      dx = x2 - x1
      dy = y2 - y1
      len = max 1.0 (sqrt (dx * dx + dy * dy))
      (nx, ny) = (-dy / len, dx / len)
      pts = [ ( x1 + dx * f + nx * j, y1 + dy * f + ny * j )
            | k <- [0 .. n]
            , let f = fromIntegral k / fromIntegral n
                  j = if k == 0 || k == n then 0.0 else jitter k ]
  in pictures
       [ color (withAlpha (0.45 * a) (makeColorI 80 220 255 255)) (line pts)
       , color (withAlpha (0.9 * a) (makeColorI 220 255 255 255)) (line pts) ]

drawPowerUp3D :: Float -> PowerUp -> Picture
drawPowerUp3D !animTime !orb =
  let (!x, !y) = toScreenPos (orbPos orb)
      !bounceY = y + sin (animTime * 4.0 + fromIntegral (orbId orb)) * 3.0
      !orbCol = case orbType orb of
        HealOrb       -> makeColorI 40 240 90 255
        EnergyOrb     -> makeColorI 0 220 255 255
        OverchargeOrb -> makeColorI 255 210 20 255
        ShieldOrb     -> makeColorI 200 235 255 255
        StarOrb       -> makeColorI 255 215 40 255
        PierceOrb     -> makeColorI 255 60 140 255
      !frame = case orbType orb of
        ShieldOrb -> color (makeColorI 160 220 255 200) $ lineLoop
                       [ (9.5 * cos a, 9.5 * sin a) | i <- [0 .. 5 :: Int], let a = fromIntegral i * pi / 3.0 ]
        _         -> color (makeColorI 255 255 255 60) (rectangleWire 16.0 16.0)
  in case orbType orb of
       StarOrb -> translate x bounceY $ pictures
         [ color (makeColorI 255 220 60 40) (circleSolid 16.0)
         , rotate (animTime * 40.0) $ color (makeColorI 255 230 100 120) $ pictures
             [ rotate (fromIntegral i * 45.0) (rectangleSolid 1.5 30.0) | i <- [0 .. 3 :: Int] ]
         , rotate (animTime * (-90.0)) $ pictures
             [ color (makeColorI 140 90 0 255) (drawStarShape 12.5 5.6)
             , color orbCol (drawStarShape 11.0 4.8)
             , color (makeColorI 255 250 200 255) (drawStarShape 5.0 2.2)
             ]
         ]
       PierceOrb -> translate x bounceY $ pictures
         [ color (makeColorI 255 60 140 45) (circleSolid 14.0)
         , rotate 45.0 $ pictures
             [ color (makeColorI 70 10 35 255) (rectangleSolid 15.0 15.0)
             , color orbCol (rectangleWire 15.0 15.0)
             ]
         , rotate (animTime * 90.0) $ color (makeColorI 255 150 200 200) $ pictures
             [ rotate (fromIntegral i * 90.0) $ translate 0.0 13.0 (rectangleSolid 1.5 4.0) | i <- [0 .. 3 :: Int] ]
         , translate 0.0 (-1.5) $ pictures
             [ color orbCol (rectangleSolid 5.0 10.0)
             , translate 0.0 5.0 $ color orbCol (polygon [(-2.5, 0.0), (2.5, 0.0), (0.0, 5.0)])
             , color white (rectangleSolid 2.0 8.0)
             , translate 0.0 (-6.0) $ color (makeColorI 255 200 80 255) (rectangleSolid 6.0 2.0)
             ]
         ]
       _ -> translate x bounceY $ pictures
           [ rotate (animTime * 60.0) $ pictures
               [ frame
               , color orbCol (thickCircle 9.0 1.8)
               ]
           , color orbCol (circleSolid 7.0)
           , color white (circleSolid 3.0)
           ]

drawParticle :: Particle -> Picture
drawParticle !p =
  let a = max 0.0 (min 1.0 (partLife p))
  in translate (partX p) (partY p) $
       color (partColor p) (circleSolid (partSize p * a))

drawShockwave :: Float -> Shockwave -> Picture
drawShockwave !animTime !sw =
  let a = max 0.0 (min 1.0 (shockLife sw))
      r = shockRadius sw
      col = shockColor sw
  in translate (shockX sw) (shockY sw) $ pictures
       [
         color col (thickCircle r (max 1.2 (3.2 * a)))
       ,
         if r > 18.0 && a > 0.15
           then rotate (animTime * 45.0) $ pictures
                  [ rotate (fromIntegral i * 60.0) $
                      translate 0.0 r $ color col (rectangleSolid 3.0 7.0)
                  | i <- [0..5 :: Int]
                  ]
           else blank
       ]

drawDustParticle :: DustParticle -> Picture
drawDustParticle !dp =
  let !progress = max 0.0 (min 1.0 (dustLife dp / max 0.01 (dustMaxLife dp)))
      !alpha    = progress * 0.45
      !sz       = dustSize dp + (1.0 - progress) * 3.5
  in translate (dustX dp) (dustY dp) $
       color (makeColorI 170 185 200 (round (alpha * 255.0))) (circleSolid sz)

data BannerKind = BannerMine | BannerVictim | BannerField
  deriving (Eq, Show)

killBannerFor :: Int -> Bool -> GameState -> Maybe (String, BannerKind, Int)
killBannerFor !myId !isLocal2P !st = case gsLastKillBanner st of
  Nothing -> Nothing
  Just (kName, vName, ticks) -> Just $ case gsLastKillIds st of
    Just (kId, _) | not isLocal2P && kId == myId ->
      ("[ BẠN (" ++ kName ++ ") ĐÃ TIÊU DIỆT " ++ vName ++ "! +" ++ show killScore ++ " ĐIỂM ]", BannerMine, ticks)
    Just (_, vId) | not isLocal2P && vId == myId ->
      ("[ BẠN ĐÃ BỊ " ++ kName ++ " TIÊU DIỆT! HỒI SINH SAU 3S ]", BannerVictim, ticks)
    _ -> ("[ CHIẾN TRƯỜNG: " ++ kName ++ " ĐÃ TIÊU DIỆT " ++ vName ++ "! ]", BannerField, ticks)

drawKillBanner :: Float -> (String, BannerKind, Int) -> Picture
drawKillBanner !animTime (!title, !kind, !ticks) =
  let !a = max 0.0 (min 1.0 (fromIntegral ticks / 15.0))
      !pulse = 0.5 + 0.5 * sin (animTime * 10.0)
      !bannerW = max 420.0 (textWidth 0.105 title + 50.0)
      !bannerH = 44.0
      !baseCol = case kind of
        BannerMine   -> makeColorI 255 215 40 255
        BannerVictim -> makeColorI 255 60 60 255
        BannerField  -> makeColorI 0 220 255 255
      !borderCol = withAlpha (0.75 + 0.25 * pulse) baseCol
      !textCol = case kind of
        BannerMine   -> makeColorI 255 235 90 255
        BannerVictim -> makeColorI 255 130 130 255
        BannerField  -> makeColorI 190 245 255 255
  in translate 0.0 185.0 $ pictures
       [ translate 3.0 (-4.0) $ color (makeColorI 0 0 0 (round (140 * a))) (rectangleSolid bannerW bannerH)
       , color (makeColorI 10 14 22 (round (235 * a))) (rectangleSolid bannerW bannerH)
       , color (withAlpha a borderCol) (rectangleWire bannerW bannerH)
       , color (withAlpha (a * 0.4) borderCol) (rectangleWire (bannerW + 4.0) (bannerH + 4.0))
       , drawTextMid 0.105 (withAlpha a textCol) title
       ]

drawNoticeBar :: Float -> (String, Int) -> Picture
drawNoticeBar !y (!msg, !ticks) =
  let !a = max 0.0 (min 1.0 (fromIntegral ticks / 15.0))
      !w = textWidth 0.09 msg + 40.0
  in translate 0.0 y $ pictures
       [ color (withAlpha (0.88 * a) uiPanelDark) (rectangleSolid w 32.0)
       , color (withAlpha a (makeColorI 255 150 60 255)) (rectangleWire w 32.0)
       , drawTextMid 0.09 (withAlpha a (makeColorI 255 200 120 255)) msg
       ]

drawKillFeed :: Float -> [KillNotice] -> Picture
drawKillFeed !_ !feed =
  let rows = take 3 feed
      drawRow i kn =
        let !y = 95.0 - fromIntegral i * 24.0
            !a = max 0.0 (min 1.0 (knTimer kn / 0.5))
            !col = if knIsMe kn then makeColorI 255 210 50 255 else makeColorI 230 140 140 255
            !txt = knKiller kn ++ " > " ++ knVictim kn
        in translate rightColX y $ pictures
             [ color (makeColorI 8 12 18 (round (180 * a))) (rectangleSolid (sideColW - 12.0) 20.0)
             , color (makeColorI 40 60 85 (round (160 * a))) (rectangleWire (sideColW - 12.0) 20.0)
             , drawTextMid 0.07 (withAlpha a col) txt
             ]
  in pictures (zipWith drawRow [0 :: Int ..] rows)

data MatchResult = MatchResult
  { mrHeadline    :: !String
  , mrHeadline2   :: !String
  , mrDetail      :: !String
  , mrWinner      :: !String
  , mrWinnerScore :: !Int
  , mrGold        :: !Int
  , mrSurrender   :: !Bool
  } deriving (Eq, Show)

drawMatchSummary :: Float -> MatchResult -> Float -> Picture
drawMatchSummary !animTime !res !countdown =
  let !pulse = 0.5 + 0.5 * sin (animTime * 5.0)
      !boxW  = 860.0
      !boxH  = 270.0
      !textW = boxW - 80.0
      !isSurr = mrSurrender res
      !themeCol = if isSurr then makeColorI 255 60 70 255 else uiGold
      !tenths = max 0 (ceiling (countdown * 10.0) :: Int)
      !secTxt = show (tenths `div` 10) ++ "." ++ show (tenths `mod` 10) ++ "s"
      !countTxt = if isSurr
                    then "TỰ ĐỘNG THOÁT SAU: " ++ secTxt
                    else "TỰ ĐỘNG QUAY VỀ SẢNH SAU: " ++ show (max 0 (ceiling countdown :: Int)) ++ "s..."
      !titleS = minimum [0.13, fitScale 0.13 textW (mrHeadline res), fitScale 0.13 textW (mrHeadline2 res)]
      winnerTxt = if null (mrWinner res)
                    then "KHÔNG CÓ NGƯỜI THẮNG"
                    else "NGƯỜI THẮNG: " ++ mrWinner res ++ "   |   TỔNG ĐIỂM: " ++ show (mrWinnerScore res)
      fitted s col str = drawTextCenter (fitScale s textW str) col str
  in pictures
       [ color (makeColorI 0 0 0 120) (rectangleSolid 1280.0 720.0)
       , translate 0.0 30.0 $ pictures
           [ translate 5.0 (-7.0) $ color (makeColorI 0 0 0 190) (rectangleSolid boxW boxH)
           , color (makeColorI 10 16 26 250) (rectangleSolid boxW boxH)
           , color (withAlpha (0.75 + 0.25 * pulse) themeCol) (rectangleWire boxW boxH)
           , color (withAlpha 0.35 themeCol) (rectangleWire (boxW + 8.0) (boxH + 8.0))
           , translate 0.0 106.0 $ pictures [ color themeCol (drawStarShape 16.0 7.0), color white (circleSolid 4.0) ]
           , translate 0.0 68.0 $ drawTextCenter titleS themeCol (mrHeadline res)
           , translate 0.0 40.0 $ drawTextCenter titleS themeCol (mrHeadline2 res)
           , translate 0.0 15.0 $ fitted 0.08 uiText (mrDetail res)
           , if isSurr then blank else translate 0.0 (-14.0) $ fitted 0.095 white winnerTxt
           , translate 0.0 (-50.0) $ pictures
               [ color (if isSurr then makeColorI 45 20 25 255 else makeColorI 25 50 35 255) (rectangleSolid 400.0 28.0)
               , color (if isSurr then makeColorI 255 80 90 200 else makeColorI 60 240 120 220) (rectangleWire 400.0 28.0)
               , drawTextMid 0.088 (if isSurr then makeColorI 255 180 180 255 else makeColorI 120 255 180 255)
                   (if mrGold res > 0 then "VÀNG NHẬN ĐƯỢC: +" ++ show (mrGold res) ++ " VÀNG" else "KHÔNG NHẬN ĐƯỢC VÀNG THƯỞNG")
               ]
           , translate 0.0 (-88.0) $ drawTextCenter 0.1 uiCyan countTxt
           , translate 0.0 (-112.0) $ drawTextCenter 0.068 uiDim "[ESC] VỀ SẢNH NGAY"
           ]
       ]

leftColX, rightColX, sideColW :: Float
leftColX  = -528.0
rightColX = 528.0
sideColW  = 204.0

matchSecondsLeft :: GameState -> Float
matchSecondsLeft !st = case gsRoomPhase st of
  PhaseInMatch t -> fromIntegral t / 30.0
  _              -> 0.0

drawInGameHUD
  :: Float
  -> Int
  -> Bool
  -> Int
  -> GameState
  -> Bool
  -> Map.Map String Float
  -> Picture
drawInGameHUD !animTime !myId !isLocal2P !gold !st !isMuted !keyGlow =
  let p1TankOpt  = Map.lookup 1 (tanks st)
      p2TankOpt  = Map.lookup 2 (tanks st)
      myTankOpt  = Map.lookup myId (tanks st)
  in if isLocal2P
       then pictures
              [ translate 0.0 326.0 (drawTopBar myId isLocal2P gold st isMuted)
              , drawLeaderboard myId isLocal2P st
              , translate rightColX 200.0 (drawMinimap animTime st myId)
              , drawPlayerColumn animTime leftColX st p1TankOpt "HERO NGƯỜI CHƠI 1 (P1)" uiCyan "H"
              , drawPlayerKeys leftColX uiCyan "PHÍM CHƠI P1" ["W", "A", "S", "D"] "G" "H" keyGlow
              , drawPlayerColumn animTime rightColX st p2TankOpt "HERO NGƯỜI CHƠI 2 (P2)" uiGold "2"
              , drawPlayerKeys rightColX uiGold "PHÍM CHƠI P2" ["\x25B2", "\x25C4", "\x25BC", "\x25BA"] "1" "2" keyGlow
              ]
       else pictures
              [ translate 0.0 326.0 (drawTopBar myId isLocal2P gold st isMuted)
              , drawLeaderboard myId isLocal2P st
              , drawHeroCard animTime (effectTiles st) myTankOpt
              , drawTileLegend animTime
              , translate rightColX 200.0 (drawMinimap animTime st myId)
              , translate rightColX (-95.0) (drawControlsPanel keyGlow)
              , case myTankOpt of
                  Just myTank -> translate 0.0 (-323.0) (drawSkillDockCustom animTime myTank "H" 720.0)
                  Nothing     -> translate 0.0 (-323.0) $
                                   drawTextMid 0.1 uiDim (if Map.null (tanks st)
                                                            then "ĐANG CHỜ MÁY CHỦ GỬI DỮ LIỆU TRẬN ĐẤU..."
                                                            else "BẠN ĐÃ RỜI TRẬN ĐẤU")
              ]

drawBadge :: Color -> String -> (Picture, Float)
drawBadge !col !label =
  let !s = 0.078
      !w = textWidth s label + 22.0
      !pic = pictures
        [ color (withAlpha 0.16 col) (rectangleSolid w 22.0)
        , color (withAlpha 0.75 col) (rectangleWire w 22.0)
        , drawTextMid s col label
        ]
  in (pic, w)

drawTopBar
  :: Int
  -> Bool
  -> Int
  -> GameState
  -> Bool
  -> Picture
drawTopBar !myId !isLocal2P !gold !st !isMuted =
  let (soundPic, soundW) = drawBadge (if isMuted then makeColorI 255 80 80 255 else makeColorI 120 220 255 255)
                                     (if isMuted then "ÂM: TẮT" else "ÂM: BẬT")
      (mapPic, mapW)     = drawBadge uiGold ("BẢN ĐỒ: " ++ mapName (currentMap st))
      (goldPic, goldW)   = drawBadge uiGold ("VÀNG: " ++ show gold ++ " G")
      (surrenderPic, surrenderW) = drawBadge (makeColorI 255 70 80 255) "[F9] ĐẦU HÀNG"

      !mSecs = max 0 (ceiling (matchSecondsLeft st) :: Int)
      pad2 n = if n < 10 then "0" ++ show n else show n
      (timerPic, timerW) =
        drawBadge (if mSecs <= 30 then makeColorI 255 60 60 255 else makeColorI 0 240 220 255)
                  ("THỜI GIAN: " ++ pad2 (mSecs `div` 60) ++ ":" ++ pad2 (mSecs `mod` 60))

      !gap = 12.0
      rightGroup = [(soundPic, soundW), (mapPic, mapW), (surrenderPic, surrenderW), (goldPic, goldW)]
      placeRight _ [] = []
      placeRight edge ((pic, w) : rest) = (translate (edge - w / 2.0) 0.0 pic) : placeRight (edge - w - gap) rest
      !rightStart = 612.0 - sum (map snd rightGroup) - gap * fromIntegral (length rightGroup - 1)
      !timerX = min 0.0 (rightStart - 24.0 - timerW / 2.0)

      !label   = if isLocal2P then "ĐỐI KHÁNG" else "PHI CÔNG"
      !nameTxt = if isLocal2P
                   then playerNameOf st 1 ++ " VS " ++ playerNameOf st 2
                   else playerNameOf st myId
      !nameX   = -604.0 + textWidth 0.078 label + 10.0
      !nameS   = fitScale 0.1 (timerX - timerW / 2.0 - 24.0 - nameX) nameTxt
  in pictures
       ( [ translate 3.0 (-4.0) $ color (makeColorI 0 0 0 120) (rectangleSolid 1256.0 40.0)
         , color uiPanel (rectangleSolid 1256.0 40.0)
         , translate 0.0 (-19.0) $ color (withAlpha 0.6 uiCyan) (rectangleSolid 1256.0 2.0)
         , translate (-616.0) 0.0 $ color uiCyan (rectangleSolid 4.0 22.0)
         , translate (-604.0) (-4.0) $ drawVietnameseText 0.078 uiDim label
         , translate nameX (fromIntegral (round (-fontUnit nameS * 3.5) :: Int)) $ drawVietnameseText nameS white nameTxt
         , translate timerX 0.0 timerPic
         ]
       ++ placeRight 612.0 rightGroup )

displayName :: Int -> Bool -> GameState -> Int -> String
displayName !myId !isLocal2P !st !tid
  | isLocal2P && tid == 1 = "P1 " ++ playerNameOf st tid
  | isLocal2P && tid == 2 = "P2 " ++ playerNameOf st tid
  | tid == myId           = playerNameOf st tid ++ " (BẠN)"
  | otherwise             = playerNameOf st tid

drawLeaderboard :: Int -> Bool -> GameState -> Picture
drawLeaderboard !myId !isLocal2P !st =
  let !panelTop = 286.0
      !panelH   = 168.0
      ranked    = take 4 $ sortBy (comparing (Down . snd))
                    [ (tid, Map.findWithDefault 0 tid (score st)) | tid <- Map.keys (tanks st) ]
      medalCol i = case i of
        0 -> makeColorI 255 200 40 255
        1 -> makeColorI 200 210 225 255
        2 -> makeColorI 210 130 60 255
        _ -> makeColorI 70 85 110 255
      rowPic (i, (tid, sc)) =
        let !y      = panelTop - 58.0 - fromIntegral i * 26.0
            !isMe   = tid == myId && not isLocal2P
            !isDead = maybe False (\t -> tankRespawnTimer t > 0) (Map.lookup tid (tanks st))
            nameC   | isDead    = makeColorI 110 120 135 255
                    | isMe      = makeColorI 90 255 140 255
                    | otherwise = uiText
            fullName = displayName myId isLocal2P st tid
            scoreTxt = show sc
            !avail   = 147.0 - textWidth 0.082 scoreTxt
            !nameS   = max 0.056 (fitScale 0.074 avail fullName)
            shown    = cutToWidth nameS avail fullName
        in translate leftColX y $ pictures
             [ if isMe then color (makeColorI 40 255 120 28) (rectangleSolid (sideColW - 12.0) 22.0) else blank
             , if isMe then translate (-(sideColW - 12.0) / 2.0 + 1.5) 0.0 $
                              color (makeColorI 40 255 120 255) (rectangleSolid 3.0 22.0) else blank
             , translate (-80.0) 0.0 $ pictures
                 [ color (medalCol i) (circleSolid 9.0)
                 , drawTextMid 0.07 (if i < 3 then makeColorI 20 20 25 255 else uiText) (show (i + 1))
                 ]
             , translate (-63.0) (-4.0) $ drawVietnameseText nameS nameC shown
             , translate 92.0 (-4.0) $ drawTextRight 0.082 uiGold scoreTxt
             ]
  in pictures
       [ translate leftColX (panelTop - panelH / 2.0) (drawPanel sideColW panelH uiGold)
       , translate (leftColX - sideColW / 2.0 + 10.0) (panelTop - 24.0) (drawPanelTitle uiGold "BẢNG XẾP HẠNG")
       , if null ranked
           then translate leftColX (panelTop - 80.0) $ drawTextMid 0.08 uiDim "Chưa có dữ liệu"
           else pictures (map rowPic (zip [0 :: Int ..] ranked))
       ]

activeEffects :: Map.Map Position TileEffect -> Tank -> [(String, Color)]
activeEffects !tiles !t
  | tankRespawnTimer t > 0 = []
  | otherwise = concat
      [ [ ("BẤT TỬ " ++ secs (tankStarTimer t), uiGold)        | tankStarTimer t > 0 ]
      , [ ("XUYÊN GIÁP " ++ secs (tankPierceTimer t), makeColorI 255 90 160 255) | tankPierceTimer t > 0 ]
      , [ ("ĐÓNG BĂNG " ++ secs (tankFreezeTimer t), makeColorI 120 240 255 255) | tankFreezeTimer t > 0 ]
      , [ ("EMP " ++ secs (tankEmpTimer t), makeColorI 255 230 60 255) | tankEmpTimer t > 0 ]
      , [ ("ĐỘC " ++ secs (tankPoisonTimer t), makeColorI 110 255 130 255) | tankPoisonTimer t > 0 ]
      , [ ("CHÓI MẮT " ++ secs (tankBlindTimer t), makeColorI 255 240 120 255) | tankBlindTimer t > 0 ]
      , [ ("SUY YẾU " ++ secs (tankWeakenTimer t), makeColorI 200 130 255 255) | tankWeakenTimer t > 0 ]
      , [ ("PHÁ GIÁP " ++ secs (tankArmorBreakTimer t), makeColorI 255 150 60 255) | tankArmorBreakTimer t > 0 ]
      , [ ("PHẢN ĐÒN " ++ secs (tankReflectTimer t), uiGold) | tankReflectTimer t > 0 ]
      , [ ("CUỒNG NHIỆT " ++ secs (tankHasteTimer t), makeColorI 255 110 90 255) | tankHasteTimer t > 0 ]
      , [ ("TÀNG HÌNH " ++ secs (tankStealthTimer t), makeColorI 220 220 235 255) | tankStealthTimer t > 0 ]
      , [ ("BỎNG", tileColor FireTile)              | tankBurnTimer t > 0 ]
      , [ ("KHÓA TUYỆT KỸ", tileColor SilenceTile)  | tankSilenceTimer t > 0 ]
      , [ ("CHẬM", tileColor MudTile)               | tileAt tiles (tankPos t) == Just MudTile ]
      , [ ("GIÁP +" ++ show (tankShield t), makeColorI 200 235 255 255) | tankShield t > 0 ]
      ]
  where
    secs ticks = show ((ticks + 29) `div` 30) ++ "s"

layoutBadgeRows :: Float -> [(Picture, Float)] -> [[(Picture, Float)]]
layoutBadgeRows !maxW = foldr place []
  where
    rowW r = sum (map snd r) + 6.0 * fromIntegral (max 0 (length r - 1))
    place b [] = [[b]]
    place b (r : rs)
      | rowW (b : r) <= maxW = (b : r) : rs
      | otherwise            = [b] : r : rs

drawBadgeRow :: [(Picture, Float)] -> Picture
drawBadgeRow !row =
  let !total = sum (map snd row) + 6.0 * fromIntegral (max 0 (length row - 1))
      step (!acc, !x) (pic, w) = (translate (x + w / 2.0) 0.0 pic : acc, x + w + 6.0)
  in pictures (fst (foldl' step ([], -total / 2.0) row))

ultStatusOf :: String -> Tank -> (String, Color)
ultStatusOf !keyLabel !t
  | tankRespawnTimer t > 0   = ("HỒI SINH SAU " ++ show (tankRespawnTimer t `div` 30 + 1) ++ "s", makeColorI 255 80 80 255)
  | tankEnergy t >= maxEnergy && (tankSilenceTimer t > 0 || tankEmpTimer t > 0)
                             = ("TUYỆT KỸ BỊ KHÓA", tileColor SilenceTile)
  | tankEnergy t >= maxEnergy = ("SẴN SÀNG - BẤM [" ++ keyLabel ++ "]", uiGold)
  | otherwise                = ("ĐANG NẠP: " ++ show (tankEnergy t) ++ "%", makeColorI 0 210 255 255)

drawPlayerColumn :: Float -> Float -> GameState -> Maybe Tank -> String -> Color -> String -> Picture
drawPlayerColumn !animTime !colX !st !mTank !panelTitle !titleCol !keyLabel =
  let !panelTop = 110.0
      !panelH   = 310.0
      !cy       = panelTop - panelH / 2.0
  in pictures
       [ translate colX cy $ drawPanel sideColW panelH
           (maybe titleCol (heroPrimaryCol . heroClassInfo . tankClass) mTank)
       , translate (colX - sideColW / 2.0 + 10.0) (panelTop - 24.0) (drawPanelTitle titleCol panelTitle)
       , case mTank of
           Nothing -> translate colX cy $ drawTextMid 0.08 uiDim "Chưa vào trận"
           Just t  ->
             let hInfo = heroClassInfo (tankClass t)
                 maxHp = getClassMaxHP (tankClass t)
                 hpRatio = min 1.0 (max 0.0 (fromIntegral (tankHealth t) / fromIntegral maxHp)) :: Float
                 enRatio = fromIntegral (tankEnergy t) / fromIntegral maxEnergy :: Float
                 hpCol | hpRatio > 0.5  = makeColorI 40 220 80 255
                       | hpRatio > 0.25 = makeColorI 240 200 30 255
                       | otherwise      = makeColorI 255 45 45 255
                 isUltReady = tankEnergy t >= maxEnergy
                 (ultTxt, ultCol) = ultStatusOf keyLabel t
                 shieldTxt = if tankShield t > 0 then " +" ++ show (tankShield t) else ""
                 badgeRows = layoutBadgeRows (sideColW - 14.0)
                               [ drawBadge c lbl | (lbl, c) <- activeEffects (effectTiles st) t ]
                 statRow y lbl ratio col valTxt = translate 0.0 y $ pictures
                   [ translate (-92.0) (-4.0) $ drawVietnameseText 0.06 uiDim lbl
                   , translate 20.0 0.0 (drawProgressBar 72.0 11.0 ratio col)
                   , translate 94.0 (-4.0) $ drawTextRight 0.066 white valTxt
                   ]
             in translate colX panelTop $ pictures
                  ( [ translate 0.0 (-44.0) $ drawTextCenter 0.085 white (playerNameOf st (tankId t))
                    , translate 0.0 (-84.0) $ pictures
                        [ color (makeColorI 9 20 33 255) (circleSolid 26.0)
                        , color (heroGlowCol hInfo) (thickCircle 26.0 2.0)
                        , scale 0.9 0.9 (drawHeroTank3D animTime (animTime * 40.0) (dummyTankOfClass (tankClass t)))
                        ]
                    , translate 0.0 (-128.0) $ drawTextCenter 0.082 (heroPrimaryCol hInfo) (heroTitle hInfo)
                    , translate 0.0 (-148.0) $ drawTextCenter 0.066 uiGold ("[" ++ keyLabel ++ "] " ++ heroUltName hInfo)
                    , translate 0.0 (-170.0) $ pictures
                        [ if isUltReady then color (withAlpha (0.2 + 0.15 * sin (animTime * 8.0)) uiGold) (rectangleSolid (sideColW - 16.0) 20.0) else blank
                        , drawTextMid 0.07 ultCol ultTxt
                        ]
                    , statRow (-196.0) "MÁU" hpRatio hpCol (show (tankHealth t) ++ shieldTxt)
                    , statRow (-220.0) "NĂNG LƯỢNG" enRatio (if isUltReady then uiGold else makeColorI 0 210 255 255)
                        (show (tankEnergy t) ++ "%")
                    ]
                    ++ [ translate 0.0 (-250.0 - fromIntegral r * 24.0) (drawBadgeRow row)
                       | (r, row) <- zip [0 :: Int ..] (take 2 badgeRows) ] )
       ]

drawPlayerKeys :: Float -> Color -> String -> [String] -> String -> String -> Map.Map String Float -> Picture
drawPlayerKeys !colX !accent !title !moveKeys !shootKey !ultKey !keyGlow =
  let !panelTop = -208.0
      !panelH   = 144.0
      glowOf k = Map.findWithDefault 0.0 k keyGlow
      key w k = drawKeyCap w 22.0 (glowOf k) k
      (kUp, kLeft, kDown, kRight) = case moveKeys of
        [a, b, c, d] -> (a, b, c, d)
        _            -> ("", "", "", "")
  in translate colX (panelTop - panelH / 2.0) $ pictures
       [ drawPanel sideColW panelH accent
       , translate (-sideColW / 2.0 + 10.0) (panelH / 2.0 - 22.0) (drawPanelTitle accent title)
       , translate (-50.0) 22.0 (key 24.0 kUp)
       , translate (-78.0) (-4.0) (key 24.0 kLeft)
       , translate (-50.0) (-4.0) (key 24.0 kDown)
       , translate (-22.0) (-4.0) (key 24.0 kRight)
       , translate (-50.0) (-28.0) $ drawTextCenter 0.06 uiDim "DI CHUYỂN"
       , translate 0.0 10.0 $ color (makeColorI 50 65 90 200) (rectangleSolid 1.0 60.0)
       , translate 30.0 9.0 (key 30.0 shootKey)
       , translate 30.0 (-16.0) $ drawTextCenter 0.06 uiDim "BẮN"
       , translate 76.0 9.0 (key 30.0 ultKey)
       , translate 76.0 (-16.0) $ drawTextCenter 0.06 uiDim "TUYỆT KỸ"
       , translate 0.0 (-52.0) $ drawTextCenter 0.058 (makeColorI 255 110 110 255) "[F9] ĐẦU HÀNG   [M] ÂM THANH"
       ]

drawHeroCard :: Float -> Map.Map Position TileEffect -> Maybe Tank -> Picture
drawHeroCard !animTime !tiles !myTankOpt =
  let !panelTop = 108.0
      !panelH   = 210.0
      !cy       = panelTop - panelH / 2.0
  in pictures
       [ translate leftColX cy $ drawPanel sideColW panelH
           (maybe uiCyan (heroPrimaryCol . heroClassInfo . tankClass) myTankOpt)
       , translate (leftColX - sideColW / 2.0 + 10.0) (panelTop - 24.0) (drawPanelTitle uiCyan "HERO CỦA BẠN")
       , case myTankOpt of
           Nothing -> translate leftColX cy $ drawTextMid 0.08 uiDim "Chưa vào trận"
           Just t  ->
             let hInfo = heroClassInfo (tankClass t)
                 (statusTxt, statusCol) = ultStatusOf "H" t
                 badgeRows = layoutBadgeRows (sideColW - 14.0)
                               (drawBadge statusCol statusTxt : [ drawBadge c lbl | (lbl, c) <- activeEffects tiles t ])
             in translate leftColX 0.0 $ pictures
                  ( [ translate 0.0 45.0 $ pictures
                        [ color (makeColorI 9 20 33 255) (circleSolid 31.0)
                        , color (heroGlowCol hInfo) (thickCircle 31.0 2.0)
                        , scale 1.05 1.05 (drawHeroTank3D animTime (animTime * 40.0) (dummyTankOfClass (tankClass t)))
                        ]
                    , translate 0.0 (-2.0) $ drawTextCenter 0.095 (heroPrimaryCol hInfo) (heroTitle hInfo)
                    , translate 0.0 (-23.0) $ drawTextCenter 0.074 uiGold ("[H] " ++ heroUltName hInfo)
                    ]
                    ++ [ translate 0.0 (-44.0 - fromIntegral r * 23.0) (drawBadgeRow row)
                       | (r, row) <- zip [0 :: Int ..] (take 3 badgeRows) ] )
       ]

drawTileLegend :: Float -> Picture
drawTileLegend !animTime =
  let !panelTop = -110.0
      !panelH   = 188.0
      rowAt i swatch name nameCol desc =
        translate leftColX (panelTop - 46.0 - fromIntegral (i :: Int) * 21.0) $ pictures
          [ translate (-84.0) 0.0 swatch
          , translate (-68.0) (-4.0) $ drawVietnameseText 0.072 nameCol name
          , translate 94.0 (-4.0) $ drawTextRight 0.066 uiDim desc
          ]
      tileRow i eff = rowAt i (drawTileGraphic animTime (fromIntegral i) 16.0 eff)
                                (tileName eff) (tileColor eff) (tileDescription eff)
      shieldSwatch = pictures
        [ color (makeColorI 9 20 33 255) (rectangleSolid 16.0 16.0)
        , color (makeColorI 160 220 255 220) $ lineLoop
            [ (6.5 * cos a, 6.5 * sin a) | k <- [0 .. 5 :: Int], let a = fromIntegral k * pi / 3.0 ]
        , color (makeColorI 200 235 255 255) (circleSolid 3.5)
        ]
      starSwatch   = color uiGold (drawStarShape 8.0 3.6)
      pierceSwatch = pictures
        [ rotate 45.0 $ color (makeColorI 255 60 140 255) (rectangleWire 11.0 11.0)
        , color white (rectangleSolid 2.0 7.0)
        , translate 0.0 3.5 $ color white (polygon [(-2.0, 0.0), (2.0, 0.0), (0.0, 3.0)]) ]
  in pictures
       [ translate leftColX (panelTop - panelH / 2.0) (drawPanel sideColW panelH (makeColorI 255 140 60 255))
       , translate (leftColX - sideColW / 2.0 + 10.0) (panelTop - 24.0)
           (drawPanelTitle (makeColorI 255 160 80 255) "Ô & VẬT PHẨM")
       , tileRow 0 MudTile
       , tileRow 1 SilenceTile
       , tileRow 2 FireTile
       , rowAt 3 shieldSwatch "NGỌC KHIÊN" (makeColorI 200 235 255 255) ("+" ++ show shieldOrbAmount ++ " GIÁP")
       , rowAt 4 starSwatch "NGÔI SAO" uiGold "BẤT TỬ 5s"
       , rowAt 5 pierceSwatch "ĐẠN XUYÊN GIÁP" (makeColorI 255 90 160 255) "10s"
       , translate leftColX (panelTop - panelH + 12.0) $
           drawTextCenter 0.062 uiDim ("MỌI NGỌC +" ++ show orbShieldBonus ++ " GIÁP ẢO")
       ]

drawMinimap :: Float -> GameState -> Int -> Picture
drawMinimap !animTime !st !myId =
  let !panelH = 172.0
      !mw = 184.0
      !mh = mw * fromIntegral gridHeight / fromIntegral gridWidth
      !cellW = mw / fromIntegral gridWidth
      !mapY  = -10.0
      toMini (Pos x y) = ( (fromIntegral x + 0.5) * cellW - mw / 2.0
                         , (fromIntegral y + 0.5) * cellW - mh / 2.0 + mapY )
      !pulseR = 4.0 + 2.5 * (0.5 + 0.5 * sin (animTime * 6.0))
  in pictures
       [ drawPanel sideColW panelH uiCyan
       , translate (-sideColW / 2.0 + 10.0) (panelH / 2.0 - 24.0) (drawPanelTitle uiCyan "RADAR CHIẾN THUẬT")
       , translate 0.0 mapY $ pictures
           [ color uiPanelDark (rectangleSolid mw mh)
           , color (makeColorI 0 200 255 25) (rectangleSolid mw 1.0)
           , color (makeColorI 0 200 255 25) (rectangleSolid 1.0 mh)
           , color (makeColorI 0 200 255 110) (rectangleWire mw mh)
           ]
       , pictures
           [ uncurry translate (toMini p) $ color (withAlpha 0.75 (tileColor eff)) (rectangleSolid (cellW - 0.8) (cellW - 0.8))
           | (p, eff) <- Map.toList (effectTiles st) ]
       , pictures
           [ uncurry translate (toMini w) $ color (makeColorI 75 92 118 255) (rectangleSolid (cellW - 0.8) (cellW - 0.8))
           | w <- mapWalls (currentMap st) ]
       , pictures
           [ uncurry translate (toMini (orbPos o)) $ case orbType o of
               StarOrb   -> color uiGold (drawStarShape (5.0 + pulseR * 0.4) 2.4)
               PierceOrb -> color (makeColorI 255 60 140 255) (rectangleSolid 4.0 4.0)
               _         -> color (makeColorI 255 220 30 255) (circleSolid 2.2)
           | o <- powerUps st ]
       , pictures
           [ uncurry translate (toMini (tankPos t)) $
               if tankId t == myId
                 then pictures [ color (makeColorI 40 255 120 120) (thickCircle pulseR 1.2)
                               , color (makeColorI 40 255 120 255) (circleSolid 3.2) ]
                 else color (if tankRespawnTimer t > 0 then makeColorI 110 110 120 255 else makeColorI 255 60 60 255)
                            (circleSolid 2.8)
           | t <- Map.elems (tanks st) ]
       ]

drawControlsPanel :: Map.Map String Float -> Picture
drawControlsPanel !keyGlow =
  let !panelH = 382.0
      glowOf k = Map.findWithDefault 0.0 k keyGlow
      key w k = drawKeyCap w 30.0 (glowOf k) k
      !leftX = -sideColW / 2.0 + 14.0
      actionRow y w k desc = translate 0.0 y $ pictures
        [ translate (leftX + w / 2.0) 0.0 (key w k)
        , translate (leftX + w + 12.0) (-4.0) (drawVietnameseText 0.08 uiText desc)
        ]
  in pictures
       [ drawPanel sideColW panelH uiCyan
       , translate (-sideColW / 2.0 + 10.0) (panelH / 2.0 - 24.0) (drawPanelTitle uiCyan "ĐIỀU KHIỂN")
       , translate 0.0 120.0 (key 38.0 "W")
       , translate (-42.0) 82.0 (key 38.0 "A")
       , translate 0.0 82.0 (key 38.0 "S")
       , translate 42.0 82.0 (key 38.0 "D")
       , translate 0.0 50.0 $ drawTextCenter 0.08 uiText "DI CHUYỂN"
       , translate 0.0 30.0 $ color (makeColorI 50 65 90 200) (rectangleSolid (sideColW - 28.0) 1.0)
       , actionRow (-2.0)   38.0 "G"  "BẮN ĐẠN"
       , actionRow (-40.0)  38.0 "H"  "TUYỆT KỸ"
       , actionRow (-78.0)  38.0 "M"  "BẬT / TẮT ÂM"
       , actionRow (-116.0) 46.0 "F9" "ĐẦU HÀNG"
       , translate 0.0 (-144.0) $ color (makeColorI 50 65 90 200) (rectangleSolid (sideColW - 28.0) 1.0)
       , translate 0.0 (-160.0) $ drawTextCenter 0.07 (makeColorI 255 90 90 255) "TRẬN 3 PHÚT: CẤM ESC"
       , translate 0.0 (-176.0) $ drawTextCenter 0.066 uiDim "Bộ gõ tiếng Việt vẫn bấm được"
       ]

drawSkillDockCustom :: Float -> Tank -> String -> Float -> Picture
drawSkillDockCustom !animTime !tank !keyLabel !dockW =
  let hInfo     = heroClassInfo (tankClass tank)
      isReady   = tankEnergy tank >= maxEnergy
      isSilenced = tankSilenceTimer tank > 0 || tankEmpTimer tank > 0
      enRatio   = fromIntegral (tankEnergy tank) / fromIntegral maxEnergy :: Float
      hpRatio   = min 1.0 (max 0.0 (fromIntegral (tankHealth tank) / fromIntegral (getClassMaxHP (tankClass tank)))) :: Float
      glowPulse = 0.5 + 0.5 * sin (animTime * 8.0)
      hpCol | hpRatio > 0.5  = makeColorI 40 220 80 255
            | hpRatio > 0.25 = makeColorI 240 200 30 255
            | otherwise      = makeColorI 255 45 45 255
      enCol = if isReady then uiGold else makeColorI 0 210 255 255
      shieldVal = tankShield tank
      shieldTxt = if shieldVal > 0 then "+" ++ show shieldVal else ""
      dockH = 60.0
      isCompact = dockW < 500.0
      barW = if isCompact then 96.0 else 160.0
      circleX = -dockW / 2.0 + 35.0
      textX = -dockW / 2.0 + 72.0
      dividerX = if isCompact then 10.0 else -12.0
      statLabelX = if isCompact then 24.0 else 6.0
      barCenterX = dockW / 2.0 - (if isCompact then 72.0 else 88.0)
      statRightX = dockW / 2.0 - 10.0
  in pictures
       [ translate 3.0 (-4.0) $ color (makeColorI 0 0 0 130) (rectangleSolid dockW dockH)
       , color uiPanel (rectangleSolid dockW dockH)
       , color (if isReady then withAlpha (0.5 + 0.5 * glowPulse) uiGold else withAlpha 0.45 uiCyan)
               (rectangleWire dockW dockH)
       , translate 0.0 (dockH / 2.0 - 1.0) $ color (withAlpha 0.7 (heroPrimaryCol hInfo)) (rectangleSolid dockW 2.0)

       , translate circleX 0.0 $ pictures
           [ color (makeColorI 18 26 40 255) (circleSolid 22.0)
           , color (makeColorI 40 52 72 255) (thickCircle 24.5 3.5)
           , if enRatio >= 0.999
               then color enCol (thickCircle 24.5 3.5)
               else if enRatio > 0.0
                 then color enCol (thickArc 90.0 (90.0 + 360.0 * enRatio) 24.5 3.5)
                 else blank
           , if isReady then color (withAlpha (0.35 * glowPulse) uiGold) (circleSolid 22.0) else blank
           , drawTextMid 0.13 (if isReady then uiGold else white) keyLabel
           , if isSilenced then pictures [ color (makeColorI 40 10 60 170) (circleSolid 22.0), drawSilenceIcon 15.0 ] else blank
           ]

       , translate textX 6.0 $
           drawVietnameseText (if isCompact then 0.078 else 0.1) (heroPrimaryCol hInfo) (heroUltName hInfo)
       , translate textX (-16.0) $
           if isSilenced
             then drawVietnameseText (if isCompact then 0.065 else 0.078) (tileColor SilenceTile) ("KHÓA [" ++ keyLabel ++ "]")
             else drawVietnameseText (if isCompact then 0.065 else 0.078) (if isReady then uiGold else uiDim)
                    (if isReady then (">>> NHẤN [" ++ keyLabel ++ "] <<<") else ("Nạp: " ++ show (tankEnergy tank) ++ "%"))

       , translate dividerX 0.0 $
           color (makeColorI 50 65 90 220) (rectangleSolid 1.0 (dockH - 16.0))

       , translate statLabelX 7.0 $
           drawVietnameseText (if isCompact then 0.065 else 0.075) uiDim "HP"
       , translate barCenterX 11.0 (drawProgressBar barW 13.0 hpRatio hpCol)
       , translate statRightX 7.0 $
           drawTextRight (if isCompact then 0.068 else 0.08) white (show (tankHealth tank) ++ shieldTxt ++ (if isCompact then "" else "/" ++ show (getClassMaxHP (tankClass tank))))

       , translate statLabelX (-17.0) $
           drawVietnameseText (if isCompact then 0.065 else 0.075) uiDim "EN"
       , translate barCenterX (-13.0) (drawProgressBar barW 13.0 enRatio enCol)
       , translate statRightX (-17.0) $
           drawTextRight (if isCompact then 0.068 else 0.08) enCol (show (tankEnergy tank) ++ "%")
       ]

drawSkillDock :: Float -> Tank -> Picture
drawSkillDock !animTime !tank = drawSkillDockCustom animTime tank "H" 720.0
