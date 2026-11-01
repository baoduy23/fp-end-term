-- Protocol.hs
-- Mã hóa, giải mã và kiểm tra gói tin giao thức mạng TCP / UDP

{-# LANGUAGE BangPatterns #-}

module Protocol
  ( encodeCommand
  , decodeCommand
  , encodeGameState
  , decodeGameState
  , encodeClientId
  , decodeClientId
  , parseRaw
  , validateCommand
  , validateGameState
  , directionParser
  , tankClassParser
  , commandParser
  , slashCommandParser
  , LanReply(..)
  , lanDiscoverMessage
  , encodeLanReply
  , decodeLanReply
  , ipv4Parser
  , pickLanRoom
  ) where

import Types
import ParserCombinator
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as Map
import Text.Read (readMaybe)
import Control.Applicative ((<|>), many, empty)
import Data.Char (isDigit)
import Data.List (intercalate, sortOn)

parseRaw :: BS.ByteString -> Either String String
parseRaw !bs =
  let cleanStr = BS.unpack (BS.filter (\c -> c /= '\r' && c /= '\n') bs)
  in if null cleanStr
       then Left "Goi tin rong hoac chi chua ky tu xuong dong"
       else Right cleanStr

directionParser :: Parser Direction
directionParser =
      (symbol "North" >> pure North)
  <|> (symbol "South" >> pure South)
  <|> (symbol "East"  >> pure East)
  <|> (symbol "West"  >> pure West)
  <|> (symbol "NORTH" >> pure North)
  <|> (symbol "SOUTH" >> pure South)
  <|> (symbol "EAST"  >> pure East)
  <|> (symbol "WEST"  >> pure West)

tankClassParser :: Parser TankClass
tankClassParser = choice
  [ symbol "VanguardCyan"    >> pure VanguardCyan
  , symbol "TitanCrimson"    >> pure TitanCrimson
  , symbol "ViperEmerald"    >> pure ViperEmerald
  , symbol "SolarisGold"     >> pure SolarisGold
  , symbol "PhantomViolet"   >> pure PhantomViolet
  , symbol "StrikerRuby"     >> pure StrikerRuby
  , symbol "AegisOnyx"       >> pure AegisOnyx
  , symbol "TempestAzure"    >> pure TempestAzure
  , symbol "MirageAmethyst"  >> pure MirageAmethyst
  , symbol "EclipseObsidian" >> pure EclipseObsidian
  ]

slashCommandParser :: Parser Command
slashCommandParser = choice
  [ symbol "/help" >> pure (SendChat "[Tro giup] Cac lenh: /help, /surrender, /hero <ten>")
  , symbol "/surrender" >> pure SurrenderCmd
  , symbol "/hero" >> do
      cls <- tankClassParser
      pure (LockHeroCmd cls)
  , symbol "/list" >> pure (SendChat "[Tuong] Vanguard, Titan, Viper, Solaris, Phantom, Striker...")
  ]

commandParser :: Parser Command
commandParser = choice
  [ slashCommandParser
  , do
      _ <- symbol "MOVE" <|> symbol "Move"
      dir <- directionParser
      pure (Move dir)
  , (symbol "SHOOT" <|> symbol "Shoot") >> pure Shoot
  , (symbol "CAST_ULTIMATE" <|> symbol "CastUltimate") >> pure CastUltimate
  , do
      _ <- symbol "SELECT_CLASS" <|> symbol "SelectClass"
      cls <- tankClassParser
      pure (SelectClass cls)
  , do
      _ <- symbol "CREATE_ROOM_CMD" <|> symbol "CreateRoomCmd"
      code <- identifier
      user <- identifier
      pure (CreateRoomCmd code user)
  , do
      _ <- symbol "JOIN_ROOM_CMD" <|> symbol "JoinRoomCmd"
      code <- identifier
      user <- identifier
      pure (JoinRoomCmd code user)
  , (symbol "CREATE_ROOM" <|> symbol "CreateRoom") >> pure CreateRoom
  , do
      _ <- symbol "JOIN_ROOM" <|> symbol "JoinRoom"
      code <- identifier
      pure (JoinRoom code)
  , (symbol "START_HERO_PICK" <|> symbol "StartHeroPickCmd") >> pure StartHeroPickCmd
  , do
      _ <- symbol "LOCK_HERO" <|> symbol "LockHeroCmd"
      cls <- tankClassParser
      pure (LockHeroCmd cls)
  , do
      _ <- symbol "HOVER_HERO" <|> symbol "HoverHeroCmd"
      cls <- tankClassParser
      pure (HoverHeroCmd cls)
  , do
      _ <- symbol "TOGGLE_READY" <|> symbol "ToggleReadyCmd"
      val <- integer
      pure (ToggleReadyCmd (val /= 0))
  , (symbol "START_MATCH" <|> symbol "StartMatch") >> pure StartMatch
  , (symbol "SURRENDER" <|> symbol "SurrenderCmd") >> pure SurrenderCmd
  , (symbol "DISCONNECT" <|> symbol "Disconnect") >> pure Disconnect
  , do
      _ <- symbol "CHAT" <|> symbol "SendChat"
      msg <- dropWhile (== ' ') <$> restOfLine
      pure (SendChat (maybe msg id (quoted msg)))
  ]

-- Giải mã chuỗi có dấu nháy kép do show tạo ra (hỗ trợ ký tự tiếng Việt escaped)
quoted :: String -> Maybe String
quoted msg = case msg of
  ('"' : _) -> readMaybe msg
  _         -> Nothing

encodeCommand :: Command -> BS.ByteString
encodeCommand !cmd =
  BS.snoc (BS.pack (show cmd)) '\n'

decodeCommand :: BS.ByteString -> Either String Command
decodeCommand !bs = do
  rawStr <- parseRaw bs
  case parse commandParser rawStr of
    Right cmd -> validateCommand cmd
    Left parseErr ->
      case readMaybe rawStr of
        Just cmd -> validateCommand cmd
        Nothing  -> Left ("Loi cu phap goi tin tai vi tri " ++ show (errPos parseErr) ++ ": " ++ errMsg parseErr)

validateCommand :: Command -> Either String Command
validateCommand !cmd = case cmd of
  Move d                  -> Right (Move d)
  Shoot                   -> Right Shoot
  CastUltimate            -> Right CastUltimate
  SendChat msg ->
    if length msg > 100
      then Left "Tin nhan tro chuyen vuot qua gioi han 100 ky tu"
      else Right (SendChat (take 48 msg))
  SelectClass c           -> Right (SelectClass c)
  Disconnect              -> Right Disconnect
  CreateRoom              -> Right CreateRoom
  JoinRoom r              -> Right (JoinRoom (take 12 r))
  StartMatch              -> Right StartMatch
  CreateRoomCmd code user -> Right (CreateRoomCmd (take 8 code) (take 16 user))
  JoinRoomCmd code user   -> Right (JoinRoomCmd (take 8 code) (take 16 user))
  StartHeroPickCmd        -> Right StartHeroPickCmd
  LockHeroCmd c           -> Right (LockHeroCmd c)
  HoverHeroCmd c          -> Right (HoverHeroCmd c)
  ToggleReadyCmd r        -> Right (ToggleReadyCmd r)
  SurrenderCmd            -> Right SurrenderCmd

encodeGameState :: GameState -> BS.ByteString
encodeGameState !st =
  BS.snoc (BS.pack (show st)) '\n'

decodeGameState :: BS.ByteString -> Either String GameState
decodeGameState !bs = do
  rawStr <- parseRaw bs
  case readMaybe rawStr of
    Just st -> validateGameState st
    Nothing -> Left "Loi cu phap goi tin GameState: Cau truc trang thai bi hong"

validateGameState :: GameState -> Either String GameState
validateGameState !st = do
  let allTanks   = Map.elems (tanks st)
      allBullets = bullets st
      validPos (Pos x y) = x >= 0 && x < gridWidth && y >= 0 && y < gridHeight

      invalidTanks   = filter (\t -> not (validPos (tankPos t))) allTanks
      invalidBullets = filter (\b -> not (validPos (bulletPos b))) allBullets

  if not (null invalidTanks)
    then Left "Loi ngu nghia GameState: Ton tai xe tang ngoai ban co"
    else if not (null invalidBullets)
      then Left "Loi ngu nghia GameState: Ton tai vien dan ngoai ban co"
      else Right st

encodeClientId :: Int -> BS.ByteString
encodeClientId !cId =
  BS.snoc (BS.pack ("CLIENT_ID " ++ show cId)) '\n'

decodeClientId :: BS.ByteString -> Maybe Int
decodeClientId !bs =
  case BS.words (BS.filter (/= '\r') bs) of
    [tag, idBs] | tag == BS.pack "CLIENT_ID" -> readMaybe (BS.unpack idBs)
    _                                        -> Nothing

-- Giao thức dò tìm phòng LAN qua UDP:
-- Client phát "DISCOVER_TANK_SERVER", máy chủ phản hồi "TANK_SERVER_OK <ip> <mã phòng | NONE>"

data LanReply = LanReply
  { lanServerIp :: !(Maybe String)
  , lanRoomCode :: !(Maybe String)
  } deriving (Eq, Show)

lanDiscoverMessage :: String
lanDiscoverMessage = "DISCOVER_TANK_SERVER"

encodeLanReply :: String -> Maybe String -> String
encodeLanReply ip mCode = "TANK_SERVER_OK " ++ ip ++ " " ++ maybe "NONE" id mCode

ipv4Parser :: Parser String
ipv4Parser = do
  parts <- sepBy1 natural (char '.')
  if length parts == 4 && all (\n -> n >= 0 && n <= 255) parts
    then pure (intercalate "." (map show parts))
    else empty

roomCodeParser :: Parser (Maybe String)
roomCodeParser =
  (symbol "NONE" >> pure Nothing)
  <|> token (do
        ds <- many (satisfy isDigit)
        if length ds == 4 then pure (Just ds) else empty)

lanReplyParser :: Parser LanReply
lanReplyParser =
  (do _ <- string "TANK_SERVER_OK:"
      code <- roomCodeParser
      eof
      pure (LanReply Nothing code))
  <|>
  (do _ <- symbol "TANK_SERVER_OK"
      ip <- token ipv4Parser
      code <- roomCodeParser
      eof
      pure (LanReply (Just ip) code))

decodeLanReply :: String -> Either String LanReply
decodeLanReply raw = case parse lanReplyParser (filter (`notElem` "\r\n") raw) of
  Right r  -> Right r
  Left err -> Left ("Goi tin LAN sai cu phap tai vi tri " ++ show (errPos err) ++ ": " ++ errMsg err)

-- Chọn phòng đang mở từ các gói phản hồi UDP, ưu tiên IP LAN thật hơn 127.0.0.1
pickLanRoom :: [(String, String)] -> Maybe (String, String)
pickLanRoom replies =
  let found = [ (ip, code)
              | (sender, msg) <- replies
              , Right (LanReply mIp (Just code)) <- [decodeLanReply msg]
              , let ip = case mIp of
                           Just i | i /= "0.0.0.0" -> i
                           _                       -> sender ]
  in case sortOn (\(ip, _) -> ip == "127.0.0.1") found of
       (r : _) -> Just r
       []      -> Nothing
