-- Client/Main.hs
-- Điểm khởi chạy client: vòng lặp Gloss playIO, xử lý sự kiện và đồng bộ với máy chủ

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import GHC.Conc (setNumCapabilities, getNumProcessors)

import Types
import Hero
import GameLogic
  ( updateGameState
  , stepGameTick
  , makeOfflineStateWithBot
  , makeLocal2PState
  , pickBotHero
  , setPlayerNames
  , handlePlayerSurrenderOrDisconnect
  , resolveMatchOutcome
  , resolveLocal2PMatchOutcome
  , matchLeader
  , playerNameOf
  )
import Protocol (encodeCommand, decodeGameState, decodeClientId, lanDiscoverMessage, pickLanRoom, decodeLanReply, LanReply(..))
import Server.Network (receiveLinesLoop, getLocalLanIp, extractIpString)
import Server.Launcher (startEmbeddedServer, defaultUdpPort)
import AI (BotMemory(..), initBotMemory, decideBotAction)
import Client.Account
  ( loadAccount
  , loginOrRegisterAccount
  , saveAccount
  , buyHero
  , addTestGold
  , recordMatchPlayed
  , claimQuest
  )
import Client.Input (normalizeKeyChar, normalizeKeyCharWith, isImeComposedChar, disableWindowsIme)
import Client.UI
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
  , nextClass
  , prevClass
  , heroClassInfo
  , cellSize
  , HeroInfo(..)
  , drawLoginScreen
  , drawLobbyScreen
  , drawShopScreen
  , drawEventScreen
  , drawRoomScreen
  , drawJoinCodeModal
  , drawPickHeroScreen
  , draw3DPlayingScene
  , drawTextCenter
  , toScreenPos
  , targetAngle
  , LoginButton(..)
  , loginHitTest
  , LobbyButton(..)
  , lobbyHitTest
  , ShopButton(..)
  , shopHitTest
  , EventButton(..)
  , eventHitTest
  , RoomButton(..)
  , roomHitTest
  , JoinCodeButton(..)
  , joinCodeHitTest
  , PickHeroButton(..)
  , pickHeroHitTest
  , localPickHitTest
  , drawLocal2PPickScreen
  , surrenderHitTest
  , SurrenderModalButton(..)
  , surrenderModalHitTest
  , drawSurrenderModal
  )
import Client.Audio
  ( SoundEvent(..)
  , initAudioSystem
  , playSound
  , playSoundAt
  , startBgmLoop
  , setBgmMute
  )
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Network.Socket
  ( Socket, AddrInfo(..), SocketType(Stream, Datagram), defaultHints
  , getAddrInfo, socket, connect, close, HostName, ServiceName
  , Family(AF_INET), SockAddr(..), SocketOption(Broadcast)
  , setSocketOption, defaultProtocol
  )
import Network.Socket.ByteString (sendAll, sendTo, recvFrom)
import System.Timeout (timeout)
import System.Process (spawnCommand)
import qualified Data.ByteString.Char8 as BC
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (TVar, newTVarIO, readTVarIO, writeTVar, atomically, modifyTVar')
import Control.Exception (SomeException, catch, finally)
import Control.Monad (when, unless, void)
import System.Info (os)
import System.Environment (getArgs)
import System.Random (randomRIO)
import System.IO (hSetEncoding, hSetBuffering, stdout, stderr, utf8, BufferMode(LineBuffering))
import qualified Data.Map.Strict as Map
import Data.Char (isDigit, isAlphaNum, isAscii, isPrint)
import Data.Fixed (mod')
import Data.List (sortOn, nub)
import Data.Maybe (isJust, fromMaybe)

data LanResult
  = LanFound !String !String
  | LanHostOnly !String
  | LanNotFound

data PendingNet = PendingCreate !String | PendingJoin !String

data ClientWorld = ClientWorld
  { worldScreen         :: !AppScreen
  , worldAccount        :: !PlayerAccount
  , worldServerHost     :: !String
  , worldServerPort     :: !String
  , worldHostLanIp      :: !String
  , worldActiveField    :: !InputField
  , worldClass          :: !TankClass
  , worldLoginError     :: !(Maybe String)
  , worldInputUser      :: !String
  , worldInputPass      :: !String
  , worldJoinCodeInput  :: !String
  , worldAnimTime       :: !Float
  , worldMyId           :: !(TVar Int)
  , worldStateVar       :: !(TVar GameState)
  , worldPrevState      :: !GameState
  , worldVisualTanks    :: !(Map.Map Int (Float, Float))
  , worldVisualAngles   :: !(Map.Map Int Float)
  , worldParticles      :: ![Particle]
  , worldShockwaves     :: ![Shockwave]
  , worldDustParticles  :: ![DustParticle]
  , worldScreenShake    :: !Float
  , worldKillFeed       :: ![KillNotice]
  , worldResult         :: !(Maybe MatchResult)
  , worldExitTimer      :: !Float
  , worldRoomCode       :: !String
  , worldChatInput      :: !String
  , worldPickTimer      :: !Float
  , worldSocket         :: !(Maybe Socket)
  , worldIsOnline       :: !Bool
  , worldConnLost       :: !(TVar Bool)
  , worldJoined         :: !Bool
  , worldJoinWait       :: !Float
  , worldIsMuted        :: !Bool
  , worldKeyGlow        :: !(Map.Map String Float)
  , worldTickAcc        :: !Float
  , worldMapBanner      :: !Float
  , worldLastMap        :: !MapId
  , worldToast          :: !(String, Float)
  , worldHeldDirs       :: ![Direction]
  , worldMoveTimer      :: !Float
  , worldHeldDirsP2     :: ![Direction]
  , worldMoveTimerP2    :: !Float
  , worldBotMemories    :: !(Map.Map Int BotMemory)
  , worldBotTimer       :: !Float
  , worldIsLocal2P      :: !Bool
  , worldP2Hover        :: !TankClass
  , worldP1Locked       :: !(Maybe TankClass)
  , worldP2Locked       :: !(Maybe TankClass)
  , worldLanVar         :: !(TVar (Maybe LanResult))
  , worldLanBusy        :: !Bool
  , worldNetVar         :: !(TVar (Maybe (Either String Socket)))
  , worldPendingNet     :: !(Maybe PendingNet)
  , worldSurrenderModal :: !Bool
  , worldImeBackspace   :: !Bool
  }

holdMoveInterval :: Float
holdMoveInterval = 0.15

botThinkInterval :: Float
botThinkInterval = 0.2

pickSeconds, resultSeconds, surrenderSeconds :: Float
pickSeconds      = 30.0
resultSeconds    = 10.0
surrenderSeconds = 5.0

wasdDir :: Char -> Maybe Direction
wasdDir c = case c of
  'w' -> Just North
  's' -> Just South
  'a' -> Just West
  'd' -> Just East
  _   -> Nothing

arrowDir :: SpecialKey -> Maybe Direction
arrowDir k = case k of
  KeyUp    -> Just North
  KeyDown  -> Just South
  KeyLeft  -> Just West
  KeyRight -> Just East
  _        -> Nothing

main :: IO ()
main = do
  cores <- getNumProcessors
  setNumCapabilities cores
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  putStrLn $ "[CLIENT] So capability: " ++ show cores

  args <- getArgs
  disableWindowsIme
  putStrLn "[CLIENT] Khoi dong Tank Battle..."

  initAudioSystem
  startBgmLoop False SndBgmMenu
  autoConfigureFirewall

  started <- startEmbeddedServer
  unless started $
    putStrLn "[CLIENT] Tren may da co may chu Tank Battle o cong 9123, dung chung may chu do."

  localLanIp <- getLocalLanIp
  putStrLn $ "[CLIENT] IP LAN: " ++ localLanIp

  initialAccount <- loadAccount "COMMANDER"

  idVar   <- newTVarIO 1
  let !startState = makeOfflineStateWithBot MapFortress VanguardCyan TitanCrimson 0
  stVar   <- newTVarIO startState
  lanVar  <- newTVarIO Nothing
  netVar  <- newTVarIO Nothing
  lostVar <- newTVarIO False

  let world0 = ClientWorld
        { worldScreen         = LoginScreen
        , worldAccount        = initialAccount
        , worldServerHost     = "127.0.0.1"
        , worldServerPort     = "9123"
        , worldHostLanIp      = localLanIp
        , worldActiveField    = FieldLoginUser
        , worldClass          = VanguardCyan
        , worldLoginError     = Nothing
        , worldInputUser      = "COMMANDER"
        , worldInputPass      = "123456"
        , worldJoinCodeInput  = ""
        , worldAnimTime       = 0.0
        , worldMyId           = idVar
        , worldStateVar       = stVar
        , worldPrevState      = startState
        , worldVisualTanks    = Map.empty
        , worldVisualAngles   = Map.empty
        , worldParticles      = []
        , worldShockwaves     = []
        , worldDustParticles  = []
        , worldScreenShake    = 0.0
        , worldKillFeed       = []
        , worldResult         = Nothing
        , worldExitTimer      = 0.0
        , worldRoomCode       = ""
        , worldChatInput      = ""
        , worldPickTimer      = pickSeconds
        , worldSocket         = Nothing
        , worldIsOnline       = False
        , worldConnLost       = lostVar
        , worldJoined         = False
        , worldJoinWait       = 0.0
        , worldIsMuted        = False
        , worldKeyGlow        = Map.empty
        , worldTickAcc        = 0.0
        , worldMapBanner      = 0.0
        , worldLastMap        = MapFortress
        , worldToast          = ("", 0.0)
        , worldHeldDirs       = []
        , worldMoveTimer      = 0.0
        , worldHeldDirsP2     = []
        , worldMoveTimerP2    = 0.0
        , worldBotMemories    = Map.empty
        , worldBotTimer       = botThinkInterval
        , worldIsLocal2P      = False
        , worldP2Hover        = TitanCrimson
        , worldP1Locked       = Nothing
        , worldP2Locked       = Nothing
        , worldLanVar         = lanVar
        , worldLanBusy        = False
        , worldNetVar         = netVar
        , worldPendingNet     = Nothing
        , worldSurrenderModal = False
        , worldImeBackspace   = False
        }

  initialWorld <- if "--offline" `elem` args then startBotMatch world0 else return world0

  playIO
    (InWindow "Multiplayer Tactical 3D Hero Tank Battle Arena" (1280, 720) (80, 40))
    black
    60
    initialWorld
    renderApp
    handleAppEvent
    stepAppWorld

roomPlayers :: GameState -> [(Int, (String, TankClass, Maybe TankClass, Bool))]
roomPlayers st = sortOn (\(pid, _) -> (pid /= gsRoomHost st, pid)) (Map.toList (gsLobbyPlayers st))

heroesHeldByOthers :: Int -> GameState -> [TankClass]
heroesHeldByOthers myId st =
  concat [ maybe [hov] (: [hov]) lk | (pid, (_, hov, lk, _)) <- Map.toList (gsLobbyPlayers st), pid /= myId ]

myServerPick :: Int -> GameState -> Maybe (TankClass, Bool)
myServerPick myId st = case Map.lookup myId (gsLobbyPlayers st) of
  Just (_, hov, lk, _) -> Just (fromMaybe hov lk, isJust lk)
  Nothing              -> Nothing

renderApp :: ClientWorld -> IO Picture
renderApp world = case worldScreen world of
  LoginScreen ->
    return $ drawLoginScreen (worldAnimTime world) (worldInputUser world) (worldInputPass world)
                             (worldActiveField world) (worldLoginError world)

  LobbyScreen ->
    return $ drawLobbyScreen (worldAnimTime world) (worldAccount world) (worldServerHost world)
                             (worldServerPort world) (worldActiveField world) (worldClass world)
                             (worldLoginError world) (worldToast world)

  ShopScreen  -> return $ drawShopScreen (worldAnimTime world) (worldAccount world) (worldToast world)
  EventScreen -> return $ drawEventScreen (worldAnimTime world) (worldAccount world) (worldToast world)

  RoomScreen code -> do
    myId <- readTVarIO (worldMyId world)
    st   <- readTVarIO (worldStateVar world)
    let players = roomPlayers st
        isReady = case Map.lookup myId (gsLobbyPlayers st) of
                    Just (_, _, _, r) -> r
                    Nothing           -> False
        chat    = [ sender ++ ": " ++ msg | (sender, msg) <- reverse (chatLog st) ]
        notice  = case gsNotice st of
                    Just (msg, _) -> Just msg
                    Nothing | snd (worldToast world) > 0.0 -> Just (fst (worldToast world))
                            | otherwise -> Nothing
    return $ drawRoomScreen (worldAnimTime world) code (worldHostLanIp world)
                            (accCallsign (worldAccount world)) (gsRoomHost st == myId) isReady
                            [ (n, r) | (_, (n, _, _, r)) <- players ]
                            chat (worldChatInput world) (worldActiveField world) notice

  JoinCodeScreen ->
    return $ pictures
      [ drawJoinCodeModal (worldAnimTime world) (worldJoinCodeInput world) (worldServerHost world)
                          (worldActiveField world) (worldLoginError world)
      , if snd (worldToast world) > 0.0
          then translate 0.0 250.0 $ drawTextCenter 0.1 (makeColorI 255 205 40 255) (fst (worldToast world))
          else blank
      ]

  PickHeroScreen _ -> renderPickScreen world

  ConnectingScreen msg ->
    return $ pictures
      [ color (makeColorI 7 10 16 255) (rectangleSolid 1400.0 800.0)
      , translate 0.0 40.0 $ rotate (worldAnimTime world * 120.0) $
          color (makeColorI 0 240 255 200) (thickCircle 35.0 4.0)
      , translate 0.0 (-40.0) $ pictures
          [ drawTextCenter 0.13 (makeColorI 0 240 255 255) "ĐANG KẾT NỐI MÁY CHỦ..."
          , translate 0.0 (-26.0) $ drawTextCenter 0.085 white msg
          , translate 0.0 (-50.0) $ drawTextCenter 0.07 (makeColorI 130 152 180 255) "[ESC] HỦY"
          ]
      ]

  PlayingScreen -> do
    !myId <- readTVarIO (worldMyId world)
    !st   <- readTVarIO (worldStateVar world)
    let !scenePic = draw3DPlayingScene
          (worldAnimTime world) myId (worldIsLocal2P world) (accGold (worldAccount world)) st
          (worldVisualTanks world) (worldVisualAngles world) (worldParticles world)
          (worldShockwaves world) (worldDustParticles world) (worldIsMuted world)
          (worldKeyGlow world) (worldMapBanner world) (worldToast world) (worldKillFeed world)
          (worldResult world) (worldExitTimer world)
        (!shakeX, !shakeY) =
          if worldScreenShake world > 0.0
            then ( cos (worldAnimTime world * 70.0) * (worldScreenShake world * 12.0)
                 , sin (worldAnimTime world * 90.0) * (worldScreenShake world * 12.0) )
            else (0.0, 0.0)
        !fullPic = if worldSurrenderModal world
                     then pictures [scenePic, drawSurrenderModal (worldAnimTime world)]
                     else scenePic
    return $ translate shakeX shakeY fullPic

renderPickScreen :: ClientWorld -> IO Picture
renderPickScreen world
  | worldIsOnline world = do
      myId <- readTVarIO (worldMyId world)
      st   <- readTVarIO (worldStateVar world)
      let players = roomPlayers st
          cursors = [ PickCursor (if pid == myId then n ++ " (BẠN)" else n) (playerSlotColor i) (fromMaybe hov lk) (isJust lk)
                    | (i, (pid, (n, hov, lk, _))) <- zip [0 ..] players ]
          (preview, iLocked) = fromMaybe (worldClass world, False) (myServerPick myId st)
          secs = case gsRoomPhase st of
                   PhaseHeroPicking t -> fromIntegral t / 30.0
                   _                  -> 0.0
          notice = fmap fst (gsNotice st)
      return $ drawPickHeroScreen (worldAnimTime world) secs (worldAccount world) preview cursors
        "W A S D: DỜI CON TRỎ  |  G / H HOẶC CLICK: KHÓA  |  TƯỚNG CÓ VÒNG MÀU LÀ NGƯỜI KHÁC ĐANG GIỮ"
        (if iLocked then "ĐÃ KHÓA - ĐANG ĐỢI NGƯỜI KHÁC..." else "KHÓA TƯỚNG [G / H] HOẶC CLICK")
        iLocked notice
  | worldIsLocal2P world =
      let p1 = PickCursor "P1" (playerSlotColor 0) (fromMaybe (worldClass world) (worldP1Locked world)) (isJust (worldP1Locked world))
          p2 = PickCursor "P2" (playerSlotColor 1) (fromMaybe (worldP2Hover world) (worldP2Locked world)) (isJust (worldP2Locked world))
          bothLocked = isJust (worldP1Locked world) && isJust (worldP2Locked world)
          action | bothLocked                  = "CẢ HAI ĐÃ KHÓA - ĐANG VÀO TRẬN..."
                 | isJust (worldP1Locked world) = "P1 ĐÃ KHÓA  |  P2 BẤM [1] HOẶC [2]"
                 | isJust (worldP2Locked world) = "P1 BẤM [G] HOẶC [H]  |  P2 ĐÃ KHÓA"
                 | otherwise                    = "P1: [G / H] KHÓA  |  P2: [1 / 2] KHÓA"
      in return $ drawLocal2PPickScreen (worldAnimTime world) (worldPickTimer world) (worldAccount world)
           p1 p2 action bothLocked
  | otherwise =
      let me = PickCursor (accCallsign (worldAccount world)) (playerSlotColor 0) (worldClass world) False
      in return $ drawPickHeroScreen (worldAnimTime world) (worldPickTimer world) (worldAccount world) (worldClass world) [me]
           "ĐẤU BOT 1v1: W A S D DỜI CON TRỎ  |  G / H HOẶC CLICK ĐỂ KHÓA  |  BOT CHỌN NGẪU NHIÊN TƯỚNG KHÁC"
           "KHÓA TƯỚNG [G / H] HOẶC CLICK" False Nothing

handleAppEvent :: Event -> ClientWorld -> IO ClientWorld
handleAppEvent event world = case worldScreen world of
  LoginScreen        -> handleLoginEvent event world
  LobbyScreen        -> handleLobbyEvent event world
  ShopScreen         -> handleShopEvent event world
  EventScreen        -> handleEventEvent event world
  RoomScreen _       -> handleRoomEvent event world
  JoinCodeScreen     -> handleJoinCodeEvent event world
  PickHeroScreen _   -> withImeKey handlePickHeroEvent event world
  ConnectingScreen _ -> handleConnectingEvent event world
  PlayingScreen      -> withImeKey handlePlayingEvent event world

-- Chuẩn hóa ký tự từ bộ gõ tiếng Việt trước khi chuyển vào bộ xử lý phím
withImeKey :: (Event -> Bool -> ClientWorld -> IO ClientWorld) -> Event -> ClientWorld -> IO ClientWorld
withImeKey handler event world = case event of
  EventKey (Char '\b') Down _ _ -> return world { worldImeBackspace = True }
  EventKey (Char '\b') Up _ _   -> return world
  EventKey (Char c) Down m p ->
    let c' = normalizeKeyCharWith (worldImeBackspace world) c
    in handler (EventKey (Char c') Down m p) (isImeComposedChar c) world { worldImeBackspace = False }
  EventKey (Char c) Up m p ->
    handler (EventKey (Char (normalizeKeyChar c)) Up m p) False world
  _ -> handler event False world

flashKeyOf :: Event -> ClientWorld -> ClientWorld
flashKeyOf event world = case keyLabelOf event (worldIsLocal2P world) of
  Just lbl -> world { worldKeyGlow = Map.insert lbl 1.0 (worldKeyGlow world) }
  Nothing  -> world

keyLabelOf :: Event -> Bool -> Maybe String
keyLabelOf (EventKey key Down _ _) isLocal2P = case key of
  Char c | c `elem` "wasdghm" -> Just [toUpperAscii c]
         | isLocal2P && c `elem` "12" -> Just [c]
  SpecialKey KeyUp    | isLocal2P -> Just "\x25B2"
  SpecialKey KeyLeft  | isLocal2P -> Just "\x25C4"
  SpecialKey KeyDown  | isLocal2P -> Just "\x25BC"
  SpecialKey KeyRight | isLocal2P -> Just "\x25BA"
  SpecialKey KeyF9    -> Just "F9"
  _                   -> Nothing
  where
    toUpperAscii ch = toEnum (fromEnum ch - 32)
keyLabelOf _ _ = Nothing

handleLoginEvent :: Event -> ClientWorld -> IO ClientWorld
handleLoginEvent event world = case event of
  EventKey (SpecialKey KeyTab) Down _ _ -> do
    playSound (worldIsMuted world) SndBtnClick
    let nextF = if worldActiveField world == FieldLoginUser then FieldLoginPass else FieldLoginUser
    return world { worldActiveField = nextF, worldLoginError = Nothing }

  EventKey (SpecialKey KeyEnter) Down _ _ -> performLogin world
  EventKey (SpecialKey KeyDelete) Down _ _ -> deleteCharLogin world
  EventKey (Char '\b') Down _ _ -> deleteCharLogin world

  EventKey (Char c) Down _ _
    | isAscii c && (isAlphaNum c || c `elem` "._-@") -> appendCharLogin c world

  EventKey (MouseButton LeftButton) Down _ (mx, my) ->
    case loginHitTest mx my of
      Just BtnLoginUserField -> do
        playSound (worldIsMuted world) SndBtnClick
        return world { worldActiveField = FieldLoginUser, worldLoginError = Nothing }
      Just BtnLoginPassField -> do
        playSound (worldIsMuted world) SndBtnClick
        return world { worldActiveField = FieldLoginPass, worldLoginError = Nothing }
      Just BtnLoginSubmit -> performLogin world
      Nothing -> return world

  _ -> return world

performLogin :: ClientWorld -> IO ClientWorld
performLogin world = do
  let rawUser = worldInputUser world
      rawPass = worldInputPass world
  if null rawUser
    then return world { worldLoginError = Just "Vui lòng nhập tên phi công!" }
    else do
      res <- loginOrRegisterAccount rawUser rawPass
      playSound (worldIsMuted world) SndBtnClick
      case res of
        Right acc -> do
          putStrLn $ "[CLIENT] Dang nhap: " ++ accCallsign acc ++ " | Vang: " ++ show (accGold acc)
          let initCls = case accUnlockedHeroes acc of
                          (c:_) -> c
                          []    -> VanguardCyan
          return world { worldAccount = acc, worldScreen = LobbyScreen, worldLoginError = Nothing, worldClass = initCls }
        Left err -> return world { worldLoginError = Just err }

deleteCharLogin :: ClientWorld -> IO ClientWorld
deleteCharLogin w = case worldActiveField w of
  FieldLoginPass -> return w { worldInputPass = dropLast (worldInputPass w), worldLoginError = Nothing }
  _              -> return w { worldInputUser = dropLast (worldInputUser w), worldLoginError = Nothing }

dropLast :: String -> String
dropLast s = if null s then "" else init s

appendCharLogin :: Char -> ClientWorld -> IO ClientWorld
appendCharLogin c w = case worldActiveField w of
  FieldLoginPass
    | length (worldInputPass w) < 20 -> return w { worldInputPass = worldInputPass w ++ [c], worldLoginError = Nothing }
  FieldLoginUser
    | length (worldInputUser w) < 14 -> return w { worldInputUser = worldInputUser w ++ [c], worldLoginError = Nothing }
  _ -> return w

handleLobbyEvent :: Event -> ClientWorld -> IO ClientWorld
handleLobbyEvent event world = case event of
  EventKey (SpecialKey KeyF1) Down _ _ -> click >> startBotPick world
  EventKey (SpecialKey KeyF2) Down _ _ -> click >> startLocal2PPick world
  EventKey (SpecialKey KeyF3) Down _ _ -> click >> return world { worldScreen = ShopScreen }
  EventKey (SpecialKey KeyF5) Down _ _ -> click >> return world { worldScreen = EventScreen }
  EventKey (SpecialKey KeyF4) Down _ _ -> click >> return world { worldActiveField = FieldServerHost }

  EventKey (SpecialKey KeyLeft) Down _ _  -> browse prevClass
  EventKey (SpecialKey KeyRight) Down _ _ -> browse nextClass
  EventKey (Char '<') Down _ _ -> browse prevClass
  EventKey (Char '>') Down _ _ -> browse nextClass

  EventKey (SpecialKey KeyTab) Down _ _ -> do
    click
    let nextF = if worldActiveField world == FieldServerHost then FieldLoginUser else FieldServerHost
    return world { worldActiveField = nextF }

  EventKey (SpecialKey KeyEnter) Down _ _
    | worldActiveField world == FieldServerHost -> return world { worldActiveField = FieldLoginUser }

  EventKey (Char '\b') Down _ _
    | worldActiveField world == FieldServerHost ->
        return world { worldServerHost = dropLast (worldServerHost world) }

  EventKey (Char c) Down _ _
    | worldActiveField world == FieldServerHost && (isDigit c || c == '.') && length (worldServerHost world) < 16 ->
        return world { worldServerHost = worldServerHost world ++ [c] }

  EventKey (MouseButton LeftButton) Down _ (mx, my) -> handleLobbyClick mx my world

  _ -> return world
  where
    click = playSound (worldIsMuted world) SndBtnClick
    browse f = do
      playSound (worldIsMuted world) SndHeroSelect
      return world { worldClass = f (worldClass world) }

handleLobbyClick :: Float -> Float -> ClientWorld -> IO ClientWorld
handleLobbyClick mx my world = case lobbyHitTest mx my of
  Just BtnPlayOffline -> click >> startBotPick world
  Just BtnPlayLocal2P -> click >> startLocal2PPick world
  Just BtnCreateRoom  -> click >> beginCreateRoom world
  Just BtnJoinRoom    -> do
    click
    return world { worldScreen = JoinCodeScreen, worldActiveField = FieldJoinCode, worldLoginError = Nothing }
  Just BtnHost        -> click >> return world { worldActiveField = FieldServerHost }
  Just BtnDiscoverLan -> click >> startLanDiscovery world
  Just BtnOpenShop    -> click >> return world { worldScreen = ShopScreen }
  Just BtnOpenEvent   -> click >> return world { worldScreen = EventScreen }
  Just BtnTestGold    -> do
    let acc' = addTestGold 10000 (worldAccount world)
    playSound (worldIsMuted world) SndClaimReward
    saveAccount acc'
    return world { worldAccount = acc', worldToast = ("+10,000 Vàng Test!", 2.5) }
  Just BtnLogout      -> click >> return world { worldScreen = LoginScreen, worldLoginError = Nothing }
  Just BtnPrevHero    -> do
    playSound (worldIsMuted world) SndHeroSelect
    return world { worldClass = prevClass (worldClass world) }
  Just BtnNextHero    -> do
    playSound (worldIsMuted world) SndHeroSelect
    return world { worldClass = nextClass (worldClass world) }
  _ -> return world
  where
    click = playSound (worldIsMuted world) SndBtnClick

handleShopEvent :: Event -> ClientWorld -> IO ClientWorld
handleShopEvent event world = case event of
  EventKey (SpecialKey KeyEsc) Down _ _ -> do
    playSound (worldIsMuted world) SndBtnClick
    return world { worldScreen = LobbyScreen }

  EventKey (MouseButton LeftButton) Down _ (mx, my) ->
    case shopHitTest mx my of
      Just BtnShopBack -> do
        playSound (worldIsMuted world) SndBtnClick
        return world { worldScreen = LobbyScreen }
      Just BtnShopTestGold -> do
        let acc' = addTestGold 10000 (worldAccount world)
        playSound (worldIsMuted world) SndClaimReward
        saveAccount acc'
        return world { worldAccount = acc', worldToast = ("+10,000 Vàng Test!", 2.5) }
      Just (BtnShopBuy cls) ->
        case buyHero cls (worldAccount world) of
          Right acc' -> do
            playSound (worldIsMuted world) SndBuyHero
            saveAccount acc'
            return world { worldAccount = acc', worldToast = ("Mở khóa thành công " ++ heroTitle (heroClassInfo cls) ++ "!", 3.0) }
          Left err -> do
            playSound (worldIsMuted world) SndBtnClick
            return world { worldToast = (err, 2.5) }
      Nothing -> return world

  _ -> return world

handleEventEvent :: Event -> ClientWorld -> IO ClientWorld
handleEventEvent event world = case event of
  EventKey (SpecialKey KeyEsc) Down _ _ -> do
    playSound (worldIsMuted world) SndBtnClick
    return world { worldScreen = LobbyScreen }

  EventKey (MouseButton LeftButton) Down _ (mx, my) ->
    case eventHitTest mx my of
      Just BtnEventBack -> do
        playSound (worldIsMuted world) SndBtnClick
        return world { worldScreen = LobbyScreen }
      Just BtnClaimM1 -> claimM 1
      Just BtnClaimM2 -> claimM 2
      Just BtnClaimM3 -> claimM 3
      Nothing -> return world

  _ -> return world
  where
    claimM qNum = case claimQuest qNum (worldAccount world) of
      Right acc' -> do
        playSound (worldIsMuted world) SndClaimReward
        saveAccount acc'
        return world { worldAccount = acc', worldToast = ("Nhận thưởng nhiệm vụ thành công!", 2.5) }
      Left err -> do
        playSound (worldIsMuted world) SndBtnClick
        return world { worldToast = (err, 2.5) }

handleJoinCodeEvent :: Event -> ClientWorld -> IO ClientWorld
handleJoinCodeEvent event world = case event of
  EventKey (SpecialKey KeyEsc) Down _ _ -> do
    playSound (worldIsMuted world) SndBtnClick
    return world { worldScreen = LobbyScreen, worldLoginError = Nothing }

  EventKey (SpecialKey KeyF4) Down _ _ -> return world { worldActiveField = FieldServerHost }

  EventKey (SpecialKey KeyTab) Down _ _ -> do
    playSound (worldIsMuted world) SndBtnClick
    let nextF = if worldActiveField world == FieldJoinCode then FieldServerHost else FieldJoinCode
    return world { worldActiveField = nextF }

  EventKey (SpecialKey KeyEnter) Down _ _ -> performJoinCode

  EventKey (Char '\b') Down _ _ -> case worldActiveField world of
    FieldServerHost -> return world { worldServerHost = dropLast (worldServerHost world), worldLoginError = Nothing }
    _               -> return world { worldJoinCodeInput = dropLast (worldJoinCodeInput world), worldLoginError = Nothing }

  EventKey (Char c) Down _ _ -> case worldActiveField world of
    FieldServerHost
      | (isDigit c || c == '.') && length (worldServerHost world) < 16 ->
          return world { worldServerHost = worldServerHost world ++ [c], worldLoginError = Nothing }
    FieldJoinCode
      | isDigit c && length (worldJoinCodeInput world) < 4 ->
          return world { worldJoinCodeInput = worldJoinCodeInput world ++ [c], worldLoginError = Nothing }
    _ -> return world

  EventKey (MouseButton LeftButton) Down _ (mx, my) ->
    case joinCodeHitTest mx my of
      Just BtnJoinCodeCancel -> do
        playSound (worldIsMuted world) SndBtnClick
        return world { worldScreen = LobbyScreen, worldLoginError = Nothing }
      Just BtnJoinCodeSubmit      -> performJoinCode
      Just BtnJoinCodeField       -> return world { worldActiveField = FieldJoinCode }
      Just BtnJoinCodeHostField   -> return world { worldActiveField = FieldServerHost }
      Just BtnJoinCodeDiscoverLan -> do
        playSound (worldIsMuted world) SndBtnClick
        startLanDiscovery world
      Nothing -> return world

  _ -> return world
  where
    performJoinCode =
      let code = worldJoinCodeInput world
      in if length code < 4
           then return world { worldLoginError = Just "Mã phòng phải gồm 4 chữ số!" }
           else do
             playSound (worldIsMuted world) SndBtnClick
             startConnectJob world (worldServerHost world) (PendingJoin code) ("ĐANG VÀO PHÒNG " ++ code ++ "...")

handleConnectingEvent :: Event -> ClientWorld -> IO ClientWorld
handleConnectingEvent event world = case event of
  EventKey (SpecialKey KeyEsc) Down _ _ ->
    return world { worldScreen = LobbyScreen, worldPendingNet = Nothing, worldLoginError = Nothing
                 , worldToast = ("Đã hủy kết nối!", 2.0) }
  _ -> return world

autoConfigureFirewall :: IO ()
autoConfigureFirewall = when (os == "mingw32") $ void $ forkIO $
  (do _ <- spawnCommand "netsh advfirewall firewall add rule name=\"TankBattleTCP\" dir=in action=allow protocol=TCP localport=9123"
      _ <- spawnCommand "netsh advfirewall firewall add rule name=\"TankBattleUDP\" dir=in action=allow protocol=UDP localport=9124"
      return ())
  `catch` (\(_ :: SomeException) -> return ())

-- Trích địa chỉ IP từ phản hồi UDP của máy chủ khi máy chủ chưa mở phòng
lanReplyServerIp :: [(String, String)] -> Maybe String
lanReplyServerIp replies =
  let valid = [ chosenIp
              | (fromIp, msg) <- replies
              , Right (LanReply mSelf _) <- [decodeLanReply msg]
              , let chosenIp = case mSelf of
                      Just ip | ip /= "127.0.0.1" && ip /= "0.0.0.0" -> ip
                      _       | fromIp /= "0.0.0.0"                  -> fromIp
                              | otherwise                            -> ""
              , not (null chosenIp)
              ]
      nonLoop = filter (/= "127.0.0.1") valid
  in case nonLoop ++ valid of
       (ip : _) -> Just ip
       []       -> Nothing

-- Dò tìm phòng LAN qua UDP với luồng thu nhận tách riêng để không bị treo bởi lời gọi FFI recvFrom trên Windows
discoverLanRoom :: String -> IO LanResult
discoverLanRoom currentHost = do
  myIp <- getLocalLanIp
  let subnetOf ip = case break (== '.') (reverse ip) of
        (_, '.' : prefix) | ip /= "127.0.0.1" && not (null ip) -> reverse prefix ++ ".255"
        _                                                      -> "255.255.255.255"
      targets = nub $ filter (not . null)
        [ "127.0.0.1", "255.255.255.255", myIp, subnetOf myIp, currentHost, subnetOf currentHost ]
  repliesVar <- newTVarIO ([] :: [(String, String)])
  closedVar  <- newTVarIO False
  (do sock <- socket AF_INET Datagram defaultProtocol
      (do setSocketOption sock Broadcast 1
          _ <- forkIO $ recvLoop sock repliesVar closedVar
          mapM_ (sendProbe sock) targets
          waitForReply sock targets repliesVar (12 :: Int))
        `finally` do
          atomically $ writeTVar closedVar True
          close sock `catch` (\(_ :: SomeException) -> return ()))
    `catch` (\(_ :: SomeException) -> return ())
  replies <- reverse <$> readTVarIO repliesVar
  return $ case pickLanRoom replies of
    Just (ip, code) -> LanFound ip code
    Nothing -> case lanReplyServerIp replies of
      Just ip -> LanHostOnly ip
      Nothing -> LanNotFound
  where
    sendProbe sock host = (do
      let hints = defaultHints { addrSocketType = Datagram, addrFamily = AF_INET }
      addrs <- getAddrInfo (Just hints) (Just host) (Just defaultUdpPort)
      case addrs of
        (a : _) -> void (sendTo sock (BC.pack lanDiscoverMessage) (addrAddress a))
        []      -> return ())
      `catch` (\(_ :: SomeException) -> return ())

    recvLoop sock repliesVar closedVar = do
      isClosed <- readTVarIO closedVar
      unless isClosed $ do
        mPkt <- (Just <$> recvFrom sock 256) `catch` (\(_ :: SomeException) -> return Nothing)
        case mPkt of
          Just (bs, SockAddrInet _ ha) -> do
            atomically $ modifyTVar' repliesVar ((extractIpString ha, BC.unpack bs) :)
            recvLoop sock repliesVar closedVar
          Just _ -> recvLoop sock repliesVar closedVar
          Nothing -> do
            closedNow <- readTVarIO closedVar
            unless closedNow $ do
              threadDelay 10000
              recvLoop sock repliesVar closedVar

    waitForReply _ _ _ 0 = return ()
    waitForReply sock targets repliesVar stepsLeft = do
      threadDelay 40000
      when (stepsLeft == 6) $ mapM_ (sendProbe sock) targets
      curReplies <- reverse <$> readTVarIO repliesVar
      case pickLanRoom curReplies of
        Just (ip, _) | ip /= "127.0.0.1" || stepsLeft <= 9 -> return ()
        _ -> waitForReply sock targets repliesVar (stepsLeft - 1)

startLanDiscovery :: ClientWorld -> IO ClientWorld
startLanDiscovery world
  | worldLanBusy world = return world { worldToast = ("Đang dò tìm phòng LAN...", 1.5) }
  | otherwise = do
      atomically $ writeTVar (worldLanVar world) Nothing
      let hostHint = worldServerHost world
      putStrLn "[LAN] Dang tim phong trong mang LAN..."
      _ <- forkIO $ do
        res <- discoverLanRoom hostHint
        atomically $ writeTVar (worldLanVar world) (Just res)
      return world { worldLanBusy = True, worldToast = ("Đang dò tìm phòng LAN...", 1.5) }

-- Nhận kết quả dò tìm LAN từ luồng nền và tự động điền Mã phòng 4 số cùng địa chỉ IP vào giao diện
pollLanDiscovery :: ClientWorld -> IO ClientWorld
pollLanDiscovery world
  | not (worldLanBusy world) = return world
  | otherwise = do
      mRes <- readTVarIO (worldLanVar world)
      case mRes of
        Nothing -> return world
        Just res -> do
          atomically $ writeTVar (worldLanVar world) Nothing
          let world' = world { worldLanBusy = False }
          case res of
            LanFound ip code -> do
              putStrLn $ "[LAN] Tim thay phong " ++ code ++ " tai " ++ ip
              playSound (worldIsMuted world) SndHeroSelect
              return world'
                { worldServerHost    = ip
                , worldJoinCodeInput = code
                , worldScreen        = if worldScreen world `elem` [LobbyScreen, JoinCodeScreen] then JoinCodeScreen else worldScreen world
                , worldActiveField   = FieldJoinCode
                , worldLoginError    = Nothing
                , worldToast         = ("Đã tìm thấy phòng " ++ code ++ " tại " ++ ip ++ " - bấm VÀO PHÒNG!", 4.0)
                }
            LanHostOnly ip -> do
              putStrLn $ "[LAN] Tim thay may chu tai " ++ ip ++ " (chua co phong mo)."
              playSound (worldIsMuted world) SndBtnClick
              return world'
                { worldServerHost  = ip
                , worldLoginError  = Nothing
                , worldToast       = ("Đã tìm thấy máy chủ tại " ++ ip ++ " (chưa mở phòng) - Bạn có thể bấm Tạo Phòng ngay!", 4.0)
                }
            LanNotFound -> do
              putStrLn "[LAN] Chua tim thay phong LAN nao."
              let fallbackHost = if null (worldServerHost world') then worldHostLanIp world' else worldServerHost world'
              return world'
                { worldServerHost = fallbackHost
                , worldLoginError = Nothing
                , worldToast      = ("Chưa tìm thấy phòng LAN nào đang mở - Bạn có thể bấm Tạo Phòng ngay!", 4.0)
                }

connectToServer :: HostName -> ServiceName -> IO (Either String Socket)
connectToServer host port = (do
  let hints = defaultHints { addrSocketType = Stream }
  addrs <- getAddrInfo (Just hints) (Just host) (Just port)
  case addrs of
    [] -> return $ Left $ "Không tìm thấy địa chỉ " ++ host ++ ":" ++ port
    (serverAddr:_) -> do
      sock <- socket (addrFamily serverAddr) (addrSocketType serverAddr) (addrProtocol serverAddr)
      r <- timeout 3000000 (connect sock (addrAddress serverAddr))
      case r of
        Just () -> return (Right sock)
        Nothing -> close sock >> return (Left ("Hết thời gian kết nối tới " ++ host ++ ":" ++ port))
  ) `catch` (\(_ :: SomeException) -> return $ Left $ "Không kết nối được máy chủ " ++ host ++ ":" ++ port)

beginCreateRoom :: ClientWorld -> IO ClientWorld
beginCreateRoom world = do
  code <- show <$> randomRIO (1000, 9999 :: Int)
  startConnectJob world "127.0.0.1" (PendingCreate code) ("ĐANG TẠO PHÒNG " ++ code ++ "...")

startConnectJob :: ClientWorld -> String -> PendingNet -> String -> IO ClientWorld
startConnectJob world host pending msg = do
  atomically $ writeTVar (worldNetVar world) Nothing
  let isLocal = host `elem` ["127.0.0.1", "localhost", worldHostLanIp world]
      port    = worldServerPort world
  _ <- forkIO $ do
    when isLocal (void startEmbeddedServer)
    res <- connectToServer host port
    atomically $ writeTVar (worldNetVar world) (Just res)
  return world { worldScreen = ConnectingScreen msg, worldPendingNet = Just pending
               , worldServerHost = host, worldLoginError = Nothing }

pollConnectJob :: ClientWorld -> IO ClientWorld
pollConnectJob world = do
  mRes <- readTVarIO (worldNetVar world)
  case (mRes, worldPendingNet world) of
    (Nothing, _) -> return world
    (Just res, Nothing) -> do
      atomically $ writeTVar (worldNetVar world) Nothing
      either (const (return ())) (\s -> close s `catch` (\(_ :: SomeException) -> return ())) res
      return world
    (Just (Left err), Just pending) -> do
      atomically $ writeTVar (worldNetVar world) Nothing
      putStrLn $ "[CLIENT] Ket noi that bai: " ++ err
      playSound (worldIsMuted world) SndBtnClick
      return world { worldPendingNet = Nothing, worldLoginError = Just err, worldToast = (err, 3.5)
                   , worldScreen = case pending of
                                     PendingCreate _ -> LobbyScreen
                                     PendingJoin _   -> JoinCodeScreen }
    (Just (Right sock), Just pending) -> do
      atomically $ writeTVar (worldNetVar world) Nothing
      idVar   <- newTVarIO 0
      stVar   <- newTVarIO (emptyGameState MapFortress)
      lostVar <- newTVarIO False
      startClientReceiver sock idVar stVar lostVar
      let myName = accCallsign (worldAccount world)
          (code, cmd) = case pending of
            PendingCreate c -> (c, CreateRoomCmd c myName)
            PendingJoin c   -> (c, JoinRoomCmd c myName)
      sendAll sock (encodeCommand cmd) `catch` (\(_ :: SomeException) -> return ())
      putStrLn $ "[ROOM] " ++ (case pending of PendingCreate _ -> "Tao phong "; PendingJoin _ -> "Vao phong ") ++ code
      return world
        { worldScreen     = RoomScreen code
        , worldPendingNet = Nothing
        , worldSocket     = Just sock
        , worldIsOnline   = True
        , worldIsLocal2P  = False
        , worldRoomCode   = code
        , worldMyId       = idVar
        , worldStateVar   = stVar
        , worldPrevState  = emptyGameState MapFortress
        , worldConnLost   = lostVar
        , worldJoined     = False
        , worldJoinWait   = 3.0
        , worldChatInput  = ""
        , worldActiveField = FieldChat
        , worldResult     = Nothing
        }

startClientReceiver :: Socket -> TVar Int -> TVar GameState -> TVar Bool -> IO ()
startClientReceiver sock idVar stVar lostVar = void $ forkIO $
  (receiveLinesLoop sock $ \lineBs ->
     case decodeClientId lineBs of
       Just assignedId -> do
         putStrLn $ "[CLIENT] May chu cap ID: " ++ show assignedId
         atomically $ writeTVar idVar assignedId
       Nothing -> case decodeGameState lineBs of
         Right st -> atomically $ writeTVar stVar st
         Left _   -> return ())
  `catch` (\(_ :: SomeException) -> return ())
  `finally` atomically (writeTVar lostVar True)

disconnectOnline :: ClientWorld -> IO ()
disconnectOnline world = case worldSocket world of
  Just sock -> do
    sendAll sock (encodeCommand Disconnect) `catch` (\(_ :: SomeException) -> return ())
    close sock `catch` (\(_ :: SomeException) -> return ())
  Nothing -> return ()

sendPlayerCommand :: ClientWorld -> Command -> IO ()
sendPlayerCommand world cmd
  | worldIsOnline world = case worldSocket world of
      Just sock -> sendAll sock (encodeCommand cmd)
                     `catch` (\(_ :: SomeException) -> return ())
      Nothing   -> return ()
  | otherwise = do
      myId <- readTVarIO (worldMyId world)
      atomically $ modifyTVar' (worldStateVar world) (updateGameState myId cmd)

sendPlayer2Command :: ClientWorld -> Command -> IO ()
sendPlayer2Command world cmd = atomically $ modifyTVar' (worldStateVar world) (updateGameState 2 cmd)

returnToLobby :: ClientWorld -> IO ClientWorld
returnToLobby world = do
  when (worldIsOnline world) (disconnectOnline world)
  return world
    { worldScreen         = LobbyScreen
    , worldSocket         = Nothing
    , worldIsOnline       = False
    , worldIsLocal2P      = False
    , worldHeldDirs       = []
    , worldHeldDirsP2     = []
    , worldBotMemories    = Map.empty
    , worldResult         = Nothing
    , worldExitTimer      = 0.0
    , worldSurrenderModal = False
    , worldP1Locked       = Nothing
    , worldP2Locked       = Nothing
    , worldJoined         = False
    }

handleRoomEvent :: Event -> ClientWorld -> IO ClientWorld
handleRoomEvent event world = case event of
  EventKey (SpecialKey KeyEsc) Down _ _ -> leaveRoom

  EventKey (SpecialKey KeyEnter) Down _ _
    | not (null (worldChatInput world)) -> do
        playSound (worldIsMuted world) SndChat
        sendPlayerCommand world (SendChat (worldChatInput world))
        return world { worldChatInput = "" }
    | otherwise -> primaryRoomAction world

  EventKey (Char '\b') Down _ _ -> return world { worldChatInput = dropLast (worldChatInput world) }

  EventKey (Char c) Down _ _
    | isPrint c && c /= '\t' && length (worldChatInput world) < 40 ->
        return world { worldChatInput = worldChatInput world ++ [c] }

  EventKey (MouseButton LeftButton) Down _ (mx, my) ->
    case roomHitTest mx my of
      Just BtnRoomLeave     -> leaveRoom
      Just BtnRoomStart     -> primaryRoomAction world
      Just BtnRoomChatInput -> return world { worldActiveField = FieldChat }
      _ -> return world

  _ -> return world
  where
    leaveRoom = do
      playSound (worldIsMuted world) SndBtnClick
      putStrLn "[ROOM] Roi phong."
      returnToLobby world

primaryRoomAction :: ClientWorld -> IO ClientWorld
primaryRoomAction world = do
  myId <- readTVarIO (worldMyId world)
  st   <- readTVarIO (worldStateVar world)
  playSound (worldIsMuted world) SndBtnClick
  if gsRoomHost st == myId
    then sendPlayerCommand world StartHeroPickCmd
    else case Map.lookup myId (gsLobbyPlayers st) of
      Just (_, _, _, isReady) -> sendPlayerCommand world (ToggleReadyCmd (not isReady))
      Nothing                 -> return ()
  return world

firstOwned :: PlayerAccount -> [TankClass] -> TankClass
firstOwned acc blocked = case filter (`notElem` blocked) (accUnlockedHeroes acc) of
  (c : _) -> c
  []      -> VanguardCyan

startBotPick :: ClientWorld -> IO ClientWorld
startBotPick world = do
  let acc = worldAccount world
      cls = if worldClass world `elem` accUnlockedHeroes acc then worldClass world else firstOwned acc []
  putStrLn "[MATCH] Chon tuong dau Bot 1v1 (30 giay)"
  return world
    { worldScreen = PickHeroScreen pickSeconds, worldPickTimer = pickSeconds, worldClass = cls
    , worldIsOnline = False, worldIsLocal2P = False, worldP1Locked = Nothing, worldP2Locked = Nothing
    , worldImeBackspace = False }

startLocal2PPick :: ClientWorld -> IO ClientWorld
startLocal2PPick world = do
  let acc = worldAccount world
      p1  = firstOwned acc []
      p2  = firstOwned acc [p1]
  putStrLn "[MATCH] Chon tuong Local 2P song song (30 giay)"
  return world
    { worldScreen = PickHeroScreen pickSeconds, worldPickTimer = pickSeconds
    , worldClass = p1, worldP2Hover = p2, worldP1Locked = Nothing, worldP2Locked = Nothing
    , worldIsOnline = False, worldIsLocal2P = True, worldImeBackspace = False }

handlePickHeroEvent :: Event -> Bool -> ClientWorld -> IO ClientWorld
handlePickHeroEvent event _ world = case event of
  EventKey (SpecialKey KeyEsc) Down _ _ -> do
    playSound (worldIsMuted world) SndBtnClick
    returnToLobby world

  EventKey (Char c) Down _ _
    | Just dir <- wasdDir c -> moveP1Cursor dir world
    | c == 'g' || c == 'h'  -> lockP1 world
    | worldIsLocal2P world && (c == '1' || c == '2') -> lockP2 world

  EventKey (SpecialKey sk) Down _ _
    | worldIsLocal2P world, Just dir <- arrowDir sk -> moveP2Cursor dir world

  EventKey (MouseButton LeftButton) Down _ (mx, my) -> do
    blocked <- p1Blocked world
    let hitTest = if worldIsLocal2P world then localPickHitTest else pickHeroHitTest
    case hitTest (worldAccount world) blocked mx my of
      Just BtnPickConfirm   -> lockP1 world
      Just (BtnPickHero cls) -> setP1Cursor cls world
      Nothing               -> return world

  _ -> return world

p1Blocked :: ClientWorld -> IO [TankClass]
p1Blocked world
  | worldIsOnline world = do
      myId <- readTVarIO (worldMyId world)
      heroesHeldByOthers myId <$> readTVarIO (worldStateVar world)
  | worldIsLocal2P world = return (worldP2Hover world : maybe [] pure (worldP2Locked world))
  | otherwise = return []

p1Cursor :: ClientWorld -> IO (TankClass, Bool)
p1Cursor world
  | worldIsOnline world = do
      myId <- readTVarIO (worldMyId world)
      st   <- readTVarIO (worldStateVar world)
      return (fromMaybe (worldClass world, False) (myServerPick myId st))
  | otherwise = return (fromMaybe (worldClass world) (worldP1Locked world), isJust (worldP1Locked world))

moveP1Cursor :: Direction -> ClientWorld -> IO ClientWorld
moveP1Cursor dir world = do
  (cur, locked) <- p1Cursor world
  blocked <- p1Blocked world
  let valid = filter (`notElem` blocked) (accUnlockedHeroes (worldAccount world))
      next  = moveHeroCursor valid cur dir
  if locked || next == cur then return world else setP1Cursor next world

setP1Cursor :: TankClass -> ClientWorld -> IO ClientWorld
setP1Cursor cls world = do
  (_, locked) <- p1Cursor world
  if locked
    then return world
    else do
      playSound (worldIsMuted world) SndHeroSelect
      when (worldIsOnline world) (sendPlayerCommand world (HoverHeroCmd cls))
      return world { worldClass = cls }

lockP1 :: ClientWorld -> IO ClientWorld
lockP1 world
  | worldIsOnline world = do
      (cur, locked) <- p1Cursor world
      unless locked $ do
        playSound (worldIsMuted world) SndHeroLock
        sendPlayerCommand world (LockHeroCmd cur)
      return world
  | worldIsLocal2P world =
      if isJust (worldP1Locked world)
        then return world
        else do
          playSound (worldIsMuted world) SndHeroLock
          maybeStartLocal2P world { worldP1Locked = Just (worldClass world) }
  | otherwise = do
      playSound (worldIsMuted world) SndHeroLock
      playSound (worldIsMuted world) SndMatchStart
      startBotMatch world

moveP2Cursor :: Direction -> ClientWorld -> IO ClientWorld
moveP2Cursor dir world
  | isJust (worldP2Locked world) = return world
  | otherwise = do
      let blocked = worldClass world : maybe [] pure (worldP1Locked world)
          valid   = filter (`notElem` blocked) (accUnlockedHeroes (worldAccount world))
          next    = moveHeroCursor valid (worldP2Hover world) dir
      when (next /= worldP2Hover world) (playSound (worldIsMuted world) SndHeroSelect)
      return world { worldP2Hover = next }

lockP2 :: ClientWorld -> IO ClientWorld
lockP2 world
  | isJust (worldP2Locked world) = return world
  | otherwise = do
      playSound (worldIsMuted world) SndHeroLock
      maybeStartLocal2P world { worldP2Locked = Just (worldP2Hover world) }

maybeStartLocal2P :: ClientWorld -> IO ClientWorld
maybeStartLocal2P world = case (worldP1Locked world, worldP2Locked world) of
  (Just p1, Just p2) -> do
    playSound (worldIsMuted world) SndMatchStart
    startLocal2PMatch world p1 p2
  _ -> return world

freshMatchWorld :: ClientWorld -> GameState -> ClientWorld
freshMatchWorld world st = world
  { worldScreen         = PlayingScreen
  , worldPrevState      = st
  , worldVisualTanks    = Map.empty
  , worldVisualAngles   = Map.empty
  , worldParticles      = []
  , worldShockwaves     = []
  , worldDustParticles  = []
  , worldScreenShake    = 0.0
  , worldKillFeed       = []
  , worldResult         = Nothing
  , worldExitTimer      = 0.0
  , worldTickAcc        = 0.0
  , worldMapBanner      = 3.0
  , worldLastMap        = currentMap st
  , worldHeldDirs       = []
  , worldHeldDirsP2     = []
  , worldMoveTimer      = 0.0
  , worldMoveTimerP2    = 0.0
  , worldSurrenderModal = False
  , worldImeBackspace   = False
  }

nextMapFor :: ClientWorld -> Int -> MapId
nextMapFor world seed = cycleMap (1 + seed `mod` (mapCount - 1)) (worldLastMap world)

startBotMatch :: ClientWorld -> IO ClientWorld
startBotMatch world = do
  seed <- randomRIO (1, 1000000 :: Int)
  let acc       = worldAccount world
      playerCls = worldClass world
      botCls    = pickBotHero (accUnlockedHeroes acc) playerCls seed
      newMap    = nextMapFor world seed
      st0       = setPlayerNames [(1, accCallsign acc)] (makeOfflineStateWithBot newMap playerCls botCls seed)
  putStrLn $ "[MATCH] Dau Bot 3 phut | Ban do: " ++ show newMap ++ " | Bot chon: " ++ show botCls
  idVar <- newTVarIO 1
  stVar <- newTVarIO st0
  return (freshMatchWorld world st0)
    { worldMyId        = idVar
    , worldStateVar    = stVar
    , worldIsOnline    = False
    , worldIsLocal2P   = False
    , worldBotMemories = Map.singleton botPlayerId ((initBotMemory botPlayerId) { botChosenClass = botCls })
    , worldBotTimer    = botThinkInterval
    , worldToast       = ("BOT ĐÃ CHỌN " ++ heroTitle (heroClassInfo botCls) ++ "!", 3.0)
    }

startLocal2PMatch :: ClientWorld -> TankClass -> TankClass -> IO ClientWorld
startLocal2PMatch world p1Cls p2Cls = do
  seed <- randomRIO (1, 1000000 :: Int)
  let newMap = nextMapFor world seed
      st0    = setPlayerNames [(1, accCallsign (worldAccount world)), (2, "NGƯỜI CHƠI 2")]
                              (makeLocal2PState newMap p1Cls p2Cls seed)
  putStrLn $ "[MATCH] Doi khang Local 2P | Ban do: " ++ show newMap ++ " | P1: " ++ show p1Cls ++ " | P2: " ++ show p2Cls
  idVar <- newTVarIO 1
  stVar <- newTVarIO st0
  return (freshMatchWorld world st0)
    { worldMyId        = idVar
    , worldStateVar    = stVar
    , worldIsOnline    = False
    , worldIsLocal2P   = True
    , worldBotMemories = Map.empty
    , worldToast       = ("ĐỐI KHÁNG LOCAL: P1 (XANH) VS P2 (VÀNG)!", 3.0)
    }

startOnlineMatch :: ClientWorld -> IO ClientWorld
startOnlineMatch world = do
  putStrLn "[MATCH] Tran dau LAN 3 phut bat dau."
  st <- readTVarIO (worldStateVar world)
  return (freshMatchWorld world st) { worldBotMemories = Map.empty }

handlePlayingEvent :: Event -> Bool -> ClientWorld -> IO ClientWorld
handlePlayingEvent event composed world0 =
  let world    = flashKeyOf event world0
      finished = isJust (worldResult world)
  in case event of
    EventKey (Char 'm') Down _ _ -> do
      let newMuted = not (worldIsMuted world)
      setBgmMute newMuted
      return world { worldIsMuted = newMuted }

    EventKey (SpecialKey KeyEsc) Down _ _
      | worldSurrenderModal world -> do
          playSound (worldIsMuted world) SndBtnClick
          return world { worldSurrenderModal = False }
      | finished -> do
          playSound (worldIsMuted world) SndBtnClick
          returnToLobby world
      | otherwise -> do
          playSound (worldIsMuted world) SndBtnClick
          return world { worldToast = ("ĐANG TRONG TRẬN 3 PHÚT! BẤM [F9] ĐỂ ĐẦU HÀNG!", 3.0) }

    _ | worldSurrenderModal world -> handleSurrenderModal event world

    EventKey (SpecialKey KeyF9) Down _ _
      | not finished -> openSurrenderModal world

    EventKey (MouseButton LeftButton) Down _ (mx, my)
      | not finished && surrenderHitTest mx my -> openSurrenderModal world

    _ | finished -> return world

    EventKey (Char c) Down _ _
      | Just dir <- wasdDir c -> do
          sendPlayerCommand world (Move dir)
          return $ if composed
            then world
            else world { worldHeldDirs = dir : filter (/= dir) (worldHeldDirs world), worldMoveTimer = holdMoveInterval }
      | c == 'g' -> shootWith sendPlayerCommand world
      | c == 'h' -> ultWith sendPlayerCommand world

    EventKey (Char c) Up _ _
      | Just dir <- wasdDir c -> return world { worldHeldDirs = filter (/= dir) (worldHeldDirs world) }

    _ | not (worldIsLocal2P world) -> return world

    EventKey (SpecialKey sk) Down _ _
      | Just dir <- arrowDir sk -> do
          sendPlayer2Command world (Move dir)
          return world { worldHeldDirsP2 = dir : filter (/= dir) (worldHeldDirsP2 world), worldMoveTimerP2 = holdMoveInterval }

    EventKey (SpecialKey sk) Up _ _
      | Just dir <- arrowDir sk -> return world { worldHeldDirsP2 = filter (/= dir) (worldHeldDirsP2 world) }

    EventKey (Char '1') Down _ _ -> shootWith sendPlayer2Command world
    EventKey (Char '2') Down _ _ -> ultWith sendPlayer2Command world

    _ -> return world
  where
    shootWith :: (ClientWorld -> Command -> IO ()) -> ClientWorld -> IO ClientWorld
    shootWith send w = do
      playSound (worldIsMuted w) SndShoot
      send w Shoot
      return w { worldScreenShake = max (worldScreenShake w) 0.05 }
    ultWith :: (ClientWorld -> Command -> IO ()) -> ClientWorld -> IO ClientWorld
    ultWith send w = do
      send w CastUltimate
      return w

openSurrenderModal :: ClientWorld -> IO ClientWorld
openSurrenderModal world = do
  playSound (worldIsMuted world) SndBtnClick
  return world { worldSurrenderModal = True, worldHeldDirs = [], worldHeldDirsP2 = [] }

handleSurrenderModal :: Event -> ClientWorld -> IO ClientWorld
handleSurrenderModal event world = case event of
  EventKey (Char 'y') Down _ _ -> performSurrender world
  EventKey (Char 'n') Down _ _ -> cancel
  EventKey (MouseButton LeftButton) Down _ (mx, my) ->
    case surrenderModalHitTest mx my of
      Just BtnSurrenderConfirm -> performSurrender world
      Just BtnSurrenderCancel  -> cancel
      Nothing                  -> return world
  _ -> return world
  where
    cancel = do
      playSound (worldIsMuted world) SndBtnClick
      return world { worldSurrenderModal = False }

performSurrender :: ClientWorld -> IO ClientWorld
performSurrender world = do
  playSound (worldIsMuted world) SndBtnClick
  myId <- readTVarIO (worldMyId world)
  let acc' = recordMatchPlayed (worldAccount world)
  saveAccount acc'
  putStrLn "[MATCH] Nguoi choi dau hang."
  if worldIsOnline world
    then sendPlayerCommand world SurrenderCmd
    else if worldIsLocal2P world
      then atomically $ modifyTVar' (worldStateVar world) (\st -> st { gsRoomPhase = PhaseMatchOver, bullets = [] })
      else atomically $ modifyTVar' (worldStateVar world) (handlePlayerSurrenderOrDisconnect False myId)
  return world
    { worldAccount        = acc'
    , worldResult         = Just MatchResult
        { mrHeadline    = "BẠN ĐÃ ĐẦU HÀNG TRẬN ĐẤU!"
        , mrHeadline2   = "TỰ ĐỘNG THOÁT SAU 5 GIÂY..."
        , mrDetail      = if worldIsLocal2P world then "Trận đối kháng Local 2P đã dừng." else "Trận đấu được tính là thua."
        , mrWinner      = ""
        , mrWinnerScore = 0
        , mrGold        = 0
        , mrSurrender   = True
        }
    , worldExitTimer      = surrenderSeconds
    , worldSurrenderModal = False
    , worldHeldDirs       = []
    , worldHeldDirsP2     = []
    }

stepAppWorld :: Float -> ClientWorld -> IO ClientWorld
stepAppWorld !dt world0 = do
  let (tMsg, tSec) = worldToast world0
      !tSec' = tSec - dt
      world1 = world0 { worldAnimTime = worldAnimTime world0 + dt
                      , worldToast = if tSec' <= 0.0 then ("", 0.0) else (tMsg, tSec') }
  world2 <- pollLanDiscovery world1
  world  <- pollConnectJob world2
  case worldScreen world of
    RoomScreen code  -> stepRoomScreen dt code world
    PickHeroScreen _ -> stepPickScreen dt world
    PlayingScreen    -> do
      world' <- stepHeldMovement dt world
      stepPlayingScreen dt world'
    _ -> return world

checkConnection :: ClientWorld -> IO (Maybe ClientWorld)
checkConnection world
  | not (worldIsOnline world) = return Nothing
  | otherwise = do
      lost <- readTVarIO (worldConnLost world)
      if lost
        then do
          putStrLn "[CLIENT] Mat ket noi voi may chu."
          w <- returnToLobby world
          return (Just w { worldToast = ("MẤT KẾT NỐI VỚI MÁY CHỦ!", 3.5) })
        else return Nothing

stepRoomScreen :: Float -> String -> ClientWorld -> IO ClientWorld
stepRoomScreen !dt code world = do
  mLost <- checkConnection world
  case mLost of
    Just w -> return w
    Nothing -> do
      myId <- readTVarIO (worldMyId world)
      st   <- readTVarIO (worldStateVar world)
      let isMember = myId > 0 && gsRoomCode st == code && Map.member myId (gsLobbyPlayers st)
      if not isMember
        then
          if worldJoined world
            then do
              w <- returnToLobby world
              return w { worldToast = ("Phòng " ++ code ++ " đã đóng.", 3.0) }
            else do
              let !wait' = worldJoinWait world - dt
              if wait' > 0.0
                then return world { worldJoinWait = wait' }
                else do
                  let isCreate = gsRoomCode st /= code && worldServerHost world == "127.0.0.1" && null (worldJoinCodeInput world)
                      err = if isCreate
                              then "Không tạo được phòng: máy chủ trên máy này đang có phòng khác."
                              else "Không vào được phòng " ++ code ++ ": sai mã, phòng đã đầy hoặc đã vào trận!"
                  putStrLn $ "[ROOM] " ++ (if isCreate then "Khong tao duoc phong " else "Khong vao duoc phong ") ++ code
                  w <- returnToLobby world
                  return w { worldScreen = if isCreate then LobbyScreen else JoinCodeScreen
                           , worldLoginError = Just err, worldToast = (err, 4.0) }
        else case gsRoomPhase st of
          PhaseHeroPicking _ -> do
            playSound (worldIsMuted world) SndMatchStart
            let cls = maybe (worldClass world) fst (myServerPick myId st)
            return world { worldScreen = PickHeroScreen pickSeconds, worldJoined = True, worldClass = cls }
          _ -> return world { worldJoined = True }

stepPickScreen :: Float -> ClientWorld -> IO ClientWorld
stepPickScreen !dt world
  | worldIsOnline world = do
      mLost <- checkConnection world
      case mLost of
        Just w -> return w
        Nothing -> do
          myId <- readTVarIO (worldMyId world)
          st   <- readTVarIO (worldStateVar world)
          let world' = maybe world (\(c, _) -> world { worldClass = c }) (myServerPick myId st)
          if not (Map.member myId (gsLobbyPlayers st))
            then do
              w <- returnToLobby world'
              return w { worldToast = ("Phòng đã đóng.", 3.0) }
            else case gsRoomPhase st of
              PhaseInMatch _ -> do
                playSound (worldIsMuted world) SndMatchStart
                startOnlineMatch world'
              PhaseWaitingLobby ->
                return world' { worldScreen = RoomScreen (worldRoomCode world')
                              , worldToast = ("Không đủ người chơi - quay lại phòng chờ.", 3.0) }
              _ -> return world'
  | otherwise = do
      let !pTimer = worldPickTimer world - dt
      if pTimer > 0.0
        then return world { worldPickTimer = pTimer }
        else if worldIsLocal2P world
          then do
            let p1 = fromMaybe (worldClass world) (worldP1Locked world)
                p2 = fromMaybe (worldP2Hover world) (worldP2Locked world)
            playSound (worldIsMuted world) SndMatchStart
            startLocal2PMatch world { worldPickTimer = 0.0 } p1 p2
          else do
            playSound (worldIsMuted world) SndMatchStart
            startBotMatch world { worldPickTimer = 0.0 }

stepHeldMovement :: Float -> ClientWorld -> IO ClientWorld
stepHeldMovement !dt world
  | isJust (worldResult world) || worldSurrenderModal world = return world
  | otherwise = do
      (dirs1, t1) <- repeatMove (worldHeldDirs world) (worldMoveTimer world) (sendPlayerCommand world)
      (dirs2, t2) <- if worldIsLocal2P world
                       then repeatMove (worldHeldDirsP2 world) (worldMoveTimerP2 world) (sendPlayer2Command world)
                       else return (worldHeldDirsP2 world, 0.0)
      return world { worldHeldDirs = dirs1, worldMoveTimer = t1, worldHeldDirsP2 = dirs2, worldMoveTimerP2 = t2 }
  where
    repeatMove :: [Direction] -> Float -> (Command -> IO ()) -> IO ([Direction], Float)
    repeatMove held timer send = case held of
      [] -> return ([], 0.0)
      (dir : _) -> do
        let !timer' = timer - dt
        if timer' > 0.0
          then return (held, timer')
          else do
            send (Move dir)
            return (held, max 0.0 (timer' + holdMoveInterval))

resolveResult :: ClientWorld -> Int -> GameState -> MatchResult
resolveResult world myId st
  | worldIsLocal2P world =
      let (title, gold) = resolveLocal2PMatchOutcome st
      in MatchResult "HẾT 3 PHÚT - TRẬN ĐỐI KHÁNG KẾT THÚC!" title
           "Thắng thua tính theo điểm trên bảng xếp hạng lúc hết giờ." winnerName winnerScore gold False
  | Just quitter <- gsLastQuit st
  , Map.member myId (tanks st)
  , length [ () | t <- Map.elems (tanks st), not (isBotPlayer (tankId t)) ] <= 1 =
      MatchResult ("ĐỐI THỦ " ++ quitter ++ " ĐÃ ĐẦU HÀNG / THOÁT TRẬN!")
                  "BẠN GIÀNH CHIẾN THẮNG TUYỆT ĐỐI!"
                  "Bạn là người cuối cùng còn lại trên chiến trường."
                  (playerNameOf st myId) (Map.findWithDefault 0 myId (score st)) 500 False
  | otherwise =
      let (title, gold, _) = resolveMatchOutcome st myId
      in MatchResult "HẾT 3 PHÚT - TRẬN ĐẤU KẾT THÚC!" title
           "Thắng thua tính theo điểm trên bảng xếp hạng lúc hết giờ." winnerName winnerScore gold False
  where
    (winnerName, winnerScore) = case matchLeader st of
      Just (_, n, s) -> (n, s)
      Nothing        -> ("", 0)

stepPlayingScreen :: Float -> ClientWorld -> IO ClientWorld
stepPlayingScreen !dt world = do
  mLost <- if isJust (worldResult world) then return Nothing else checkConnection world
  case mLost of
    Just w -> return w
    Nothing -> stepPlayingScreen' dt world

stepPlayingScreen' :: Float -> ClientWorld -> IO ClientWorld
stepPlayingScreen' !dt world = do
  let !tickLen  = 1.0 / 30.0
      !acc0     = worldTickAcc world + dt
      !numTicks = if worldIsOnline world then 0 else min 5 (floor (acc0 / tickLen)) :: Int
      !accLeft  = if worldIsOnline world then 0.0 else acc0 - fromIntegral numTicks * tickLen
  when (numTicks > 0) $
    atomically $ modifyTVar' (worldStateVar world) (\s -> iterate stepGameTick s !! numTicks)

  let !bTimer = worldBotTimer world - dt
  (!nextBotMems, !bTimer') <-
    if not (worldIsOnline world) && not (worldIsLocal2P world) && bTimer <= 0.0
      then do
        stNow <- readTVarIO (worldStateVar world)
        let step (!sAcc, !mAcc) (bId, bMem) =
              let (mCmd, bMem') = decideBotAction bMem sAcc
              in (maybe sAcc (\cmd -> updateGameState bId cmd sAcc) mCmd, Map.insert bId bMem' mAcc)
            (!stNext, !memsNext) = foldl' step (stNow, worldBotMemories world) (Map.toList (worldBotMemories world))
        atomically $ writeTVar (worldStateVar world) stNext
        return (memsNext, botThinkInterval)
      else return (worldBotMemories world, max 0.0 bTimer)

  !myId     <- readTVarIO (worldMyId world)
  !curState <- readTVarIO (worldStateVar world)
  let !prevState  = worldPrevState world
      !mapChanged = currentMap curState /= currentMap prevState
      !mapBanner' = if mapChanged then 3.0 else max 0.0 (worldMapBanner world - dt)
      (!toast', !mEventSnd) = case detectWorldEvent prevState curState of
                                Just msg | not mapChanged -> ((msg, 2.5), Just SndEvent)
                                _ -> (worldToast world, Nothing)

  let (!newParticles, !newShockwaves, !soundPosEvents, !deadTanks, !playerHit, !ultFired) =
        detectCombatImpacts myId prevState curState
      !listenerPos = case Map.lookup myId (tanks curState) of
                       Just myTank -> toScreenPos (tankPos myTank)
                       Nothing     -> (0.0, 0.0)

  mapM_ (playSound (worldIsMuted world)) mEventSnd
  mapM_ (\(evt, sPos) -> playSoundAt (worldIsMuted world) listenerPos sPos evt) soundPosEvents

  let !newShake = maximum
        [ max 0.0 (worldScreenShake world - dt)
        , if playerHit then 0.16 else 0.0
        , if ultFired then 0.22 else 0.0
        , if not (null deadTanks) then 0.25 else 0.0 ]

  let !movingTanks =
        [ tCur
        | (tId, tCur) <- Map.toList (tanks curState)
        , Just tPrev <- [Map.lookup tId (tanks prevState)]
        , tankPos tCur /= tankPos tPrev
        , tankRespawnTimer tCur == 0
        ]
      !updatedDust = take 60 $ stepDustParticles dt (concatMap spawnTreadDust movingTanks ++ worldDustParticles world)

  let isNewKill = case (gsLastKillBanner curState, gsLastKillBanner prevState) of
        (Just (_, _, t), Just (_, _, pt)) -> t > pt
        (Just _, Nothing)                 -> True
        _                                 -> False
      newKills = case gsLastKillBanner curState of
        Just (k, v, _) | isNewKill ->
          [ KillNotice k v 3.5 (not (worldIsLocal2P world) && fmap fst (gsLastKillIds curState) == Just myId) ]
        _ -> []
      !feed' = take 3 (newKills ++ [ kn { knTimer = knTimer kn - dt } | kn <- worldKillFeed world, knTimer kn - dt > 0.0 ])

  (!result', !account', !exit0) <-
    case worldResult world of
      Just r -> return (Just r, worldAccount world, worldExitTimer world)
      Nothing
        | gsRoomPhase curState == PhaseMatchOver -> do
            let res = resolveResult world myId curState
                acc = recordMatchPlayed ((worldAccount world) { accGold = accGold (worldAccount world) + mrGold res })
            putStrLn $ "[MATCH] Ket thuc tran | Nguoi thang: " ++ mrWinner res ++ " | Vang: +" ++ show (mrGold res)
            saveAccount acc
            playSound (worldIsMuted world) (if mrGold res >= 500 then SndVictory else SndTakedown)
            return (Just res, acc, resultSeconds)
        | otherwise -> return (Nothing, worldAccount world, 0.0)

  let !exitTimer' = if isJust result' then exit0 - dt else 0.0

  if isJust result' && exitTimer' <= 0.0
    then do
      putStrLn "[CLIENT] Tu dong ve sanh chinh."
      w <- returnToLobby world { worldAccount = account' }
      return w
    else return world
      { worldAccount        = account'
      , worldTickAcc        = min accLeft tickLen
      , worldKeyGlow        = Map.filter (> 0.0) (Map.map (subtract (dt * 4.0)) (worldKeyGlow world))
      , worldMapBanner      = mapBanner'
      , worldToast          = toast'
      , worldPrevState      = curState
      , worldParticles      = stepParticles dt (newParticles ++ worldParticles world)
      , worldShockwaves     = stepShockwaves dt (newShockwaves ++ worldShockwaves world)
      , worldDustParticles  = updatedDust
      , worldScreenShake    = newShake
      , worldKillFeed       = feed'
      , worldResult         = result'
      , worldExitTimer      = exitTimer'
      , worldVisualTanks    = interpolateTanks (if mapChanged then Map.empty else worldVisualTanks world) (tanks curState)
      , worldVisualAngles   = interpolateAngles (worldVisualAngles world) (tanks curState)
      , worldBotMemories    = nextBotMems
      , worldBotTimer       = bTimer'
      }

detectWorldEvent :: GameState -> GameState -> Maybe String
detectWorldEvent !sPrev !sCur
  | hasOrb StarOrb sCur && not (hasOrb StarOrb sPrev)     = Just "NGÔI SAO BẤT TỬ XUẤT HIỆN GIỮA BẢN ĐỒ!"
  | hasOrb PierceOrb sCur && not (hasOrb PierceOrb sPrev) = Just "ĐẠN CƯỜNG HÓA XUẤT HIỆN TRÊN BẢN ĐỒ!"
  | not (Map.null (effectTiles sPrev))
    && Map.keysSet (effectTiles sCur) /= Map.keysSet (effectTiles sPrev)
    && Map.size (effectTiles sCur) > 1                   = Just "CÁC Ô ĐẶC BIỆT ĐÃ ĐỔI VỊ TRÍ!"
  | otherwise                                             = Nothing
  where
    hasOrb k st = any (\o -> orbType o == k) (powerUps st)

lerpVal :: Float -> Float -> Float -> Float
lerpVal !a !b !factor = a + (b - a) * factor

interpolateTanks
  :: Map.Map Int (Float, Float)
  -> Map.Map Int Tank
  -> Map.Map Int (Float, Float)
interpolateTanks !curMap !tanksMap =
  foldl' updateSingleTank curMap (Map.toList tanksMap)
  where
    updateSingleTank acc (tId, tank) =
      let (!targetX, !targetY) = toScreenPos (tankPos tank)
          (!curX, !curY)       = Map.findWithDefault (targetX, targetY) tId acc
          !smoothX = lerpVal curX targetX 0.28
          !smoothY = lerpVal curY targetY 0.28
      in Map.insert tId (smoothX, smoothY) acc

interpolateAngles
  :: Map.Map Int Float
  -> Map.Map Int Tank
  -> Map.Map Int Float
interpolateAngles !curMap !tanksMap =
  foldl' updateSingleAngle curMap (Map.toList tanksMap)
  where
    updateSingleAngle acc (tId, tank) =
      let !targA = targetAngle (tankDir tank)
          !curA  = Map.findWithDefault targA tId acc
          !diff0 = (targA - curA + 180.0) `mod'` 360.0 - 180.0
          !diff  = if diff0 < -180.0 then diff0 + 360.0 else diff0
          !smoothA = curA + diff * 0.32
      in Map.insert tId smoothA acc

spawnTreadDust :: Tank -> [DustParticle]
spawnTreadDust !tank =
  let (!tx, !ty) = toScreenPos (tankPos tank)
      !trackOffset = 7.0
      !rearOffset  = 10.0
      (!p1, !p2) = case tankDir tank of
        North -> ((tx - trackOffset, ty - rearOffset), (tx + trackOffset, ty - rearOffset))
        South -> ((tx - trackOffset, ty + rearOffset), (tx + trackOffset, ty + rearOffset))
        East  -> ((tx - rearOffset, ty - trackOffset), (tx - rearOffset, ty + trackOffset))
        West  -> ((tx + rearOffset, ty - trackOffset), (tx + rearOffset, ty + trackOffset))
  in [ DustParticle (fst p1) (snd p1) 0.5 0.5 3.2
     , DustParticle (fst p2) (snd p2) 0.5 0.5 3.2
     ]

stepDustParticles :: Float -> [DustParticle] -> [DustParticle]
stepDustParticles !dt ps =
  [ p { dustLife = l' }
  | p <- ps
  , let !l' = dustLife p - dt
  , l' > 0.0
  ]

detectCombatImpacts
  :: Int
  -> GameState
  -> GameState
  -> ( [Particle]
     , [Shockwave]
     , [(SoundEvent, (Float, Float))]
     , [Tank]
     , Bool
     , Bool
     )
detectCombatImpacts !myId !sPrev !sCur =
  let !prevBulletPos = map bulletPos (bullets sPrev)
      !curBulletPos  = map bulletPos (bullets sCur)
      !explodedPos   = filter (`notElem` curBulletPos) prevBulletPos

      !bParticles  = concatMap (\bp -> let (!bx, !by) = toScreenPos bp in spawnBulletSparks bx by) explodedPos
      !bShockwaves = []

      !hitTanks =
        [ tCur
        | (tId, tCur) <- Map.toList (tanks sCur)
        , Just tPrev <- [Map.lookup tId (tanks sPrev)]
        , tankHealth tCur < tankHealth tPrev
        , tankRespawnTimer tCur == 0
        ]

      !deadTanks =
        [ tCur
        | (tId, tCur) <- Map.toList (tanks sCur)
        , Just tPrev <- [Map.lookup tId (tanks sPrev)]
        , tankRespawnTimer tCur > 0 && tankRespawnTimer tPrev == 0
        ]

      !ultTanks =
        [ tCur
        | (tId, tCur) <- Map.toList (tanks sCur)
        , Just tPrev <- [Map.lookup tId (tanks sPrev)]
        , tankEnergy tPrev >= maxEnergy && tankEnergy tCur < 20
        ]

      !respawnedTanks =
        [ tCur
        | (tId, tCur) <- Map.toList (tanks sCur)
        , Just tPrev <- [Map.lookup tId (tanks sPrev)]
        , tankRespawnTimer tPrev > 0 && tankRespawnTimer tCur == 0
        ]

      !enemyBulletsFired =
        filter (\b -> bulletOwner b /= myId && b `notElem` bullets sPrev) (bullets sCur)

      (!hitParticles, !hitShockwaves) =
        foldl' (\(!pAcc, !wAcc) tank ->
          let (!tx, !ty) = toScreenPos (tankPos tank)
          in ( spawnSparks tx ty (makeColorI 255 70 70 255) ++ pAcc
             , spawnCustomShockwave tx ty 22.0 (makeColorI 255 80 80 200) : wAcc
             )
          ) ([], []) hitTanks

      (!deadParticles, !deadShockwaves) =
        foldl' (\(!pAcc, !wAcc) tank ->
          let (!tx, !ty) = toScreenPos (tankPos tank)
          in ( spawnSparks tx ty (makeColorI 255 140 20 255)
               ++ spawnSparks tx ty (makeColorI 255 40 40 255) ++ pAcc
             , spawnCustomShockwave tx ty 58.0 (makeColorI 255 100 20 255) : wAcc
             )
          ) ([], []) deadTanks

      (!ultParticles, !ultShockwaves) =
        foldl' (\(!pAcc, !wAcc) tank ->
          let (!tx, !ty) = toScreenPos (tankPos tank)
              !glowC     = heroGlowCol (heroClassInfo (tankClass tank))
              !ringR     = case specUltShape (specOf tank) of
                             Radial r       -> (fromIntegral r + 0.5) * cellSize
                             Pull r         -> (fromIntegral r + 0.5) * cellSize
                             BlinkBurst _ r -> (fromIntegral r + 0.5) * cellSize
                             _              -> cellSize
          in ( spawnSparks tx ty glowC ++ pAcc
             , spawnCustomShockwave tx ty ringR glowC : wAcc
             )
          ) ([], []) ultTanks

      !pickedOrbs = filter (`notElem` powerUps sCur) (powerUps sPrev)

      !soundList =
        [ (SndTakedown, toScreenPos (tankPos dt)) | dt <- deadTanks ]
        ++ [ (SndUltimate, toScreenPos (tankPos ut)) | ut <- ultTanks ]
        ++ [ (SndRespawn, toScreenPos (tankPos rt)) | rt <- respawnedTanks ]
        ++ [ (SndHit, toScreenPos (tankPos ht)) | ht <- hitTanks ]
        ++ [ (SndPowerUp, toScreenPos (orbPos o)) | o <- pickedOrbs ]
        ++ [ (SndShoot, toScreenPos (bulletPos b)) | b <- enemyBulletsFired ]

      !allParticles  = bParticles ++ hitParticles ++ deadParticles ++ ultParticles
      !allShockwaves = bShockwaves ++ hitShockwaves ++ deadShockwaves ++ ultShockwaves
      !playerWasHit  = any (\t -> tankId t == myId) hitTanks
      !ultWasFired   = not (null ultTanks)
  in (allParticles, allShockwaves, soundList, deadTanks, playerWasHit, ultWasFired)

spawnBulletSparks :: Float -> Float -> [Particle]
spawnBulletSparks !x !y =
  [ Particle
      { partX     = x
      , partY     = y
      , partVX    = 35.0 * cos a
      , partVY    = 35.0 * sin a
      , partLife  = 0.35
      , partDecay = 3.2
      , partSize  = 1.8
      , partColor = makeColorI 255 210 120 220
      }
  | i <- [0..3 :: Int]
  , let !a = fromIntegral i * (pi / 2.0) + 0.3
  ]

spawnSparks :: Float -> Float -> Color -> [Particle]
spawnSparks !x !y !col =
  [ Particle
      { partX     = x
      , partY     = y
      , partVX    = 60.0 * cos a
      , partVY    = 60.0 * sin a
      , partLife  = 0.65
      , partDecay = 2.8
      , partSize  = 2.4
      , partColor = col
      }
  | i <- [0..7 :: Int]
  , let !a = fromIntegral i * (2.0 * pi / 8.0)
  ]

spawnCustomShockwave :: Float -> Float -> Float -> Color -> Shockwave
spawnCustomShockwave !x !y !maxR !col = Shockwave
  { shockX      = x
  , shockY      = y
  , shockRadius = 4.0
  , shockMaxR   = maxR
  , shockLife   = 1.0
  , shockColor  = col
  }

stepParticles :: Float -> [Particle] -> [Particle]
stepParticles !dt ps =
  [ p { partX = px + pvx * dt
      , partY = py + pvy * dt
      , partLife = life'
      }
  | p@(Particle px py pvx pvy life decay _ _) <- ps
  , let !life' = life - decay * dt
  , life' > 0.0
  ]

stepShockwaves :: Float -> [Shockwave] -> [Shockwave]
stepShockwaves !dt sws =
  [ sw { shockRadius = r + max 95.0 (maxR * 2.6) * dt
       , shockLife   = life'
       }
  | sw@(Shockwave _ _ r maxR life _) <- sws
  , let !life' = life - 2.5 * dt
  , life' > 0.0 && r < maxR + 1.0
  ]
