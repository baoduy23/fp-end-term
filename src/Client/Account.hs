-- Client/Account.hs
-- Quản lý tài khoản: lưu, đọc và tiến trình của người chơi

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Client.Account
  ( PlayerAccount(..)
  , defaultAccount
  , defaultAccountWithPass
  , loadAccount
  , loginOrRegisterAccount
  , mkCallsign
  , saveAccount
  , buyHero
  , claimDailyReward
  , addMatchReward
  , addTestGold
  , recordMatchPlayed
  , claimQuest
  , heroCost
  , heroIsBase
  , accountKVParser
  , parseAccountStrict
  ) where

import Types
import Hero
import ParserCombinator
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import Text.Read (readMaybe)
import Control.Exception (catch, SomeException)
import Data.Char (isAlphaNum, isAscii)
import System.IO (hPutStrLn, stderr)
import Control.Applicative ((<|>), many)
import Data.List (intercalate)
import qualified Data.ByteString.Char8 as BS

tankClassParser :: Parser TankClass
tankClassParser = choice
  [ string "VanguardCyan" *> pure VanguardCyan
  , string "TitanCrimson" *> pure TitanCrimson
  , string "ViperEmerald" *> pure ViperEmerald
  , string "SolarisGold" *> pure SolarisGold
  , string "PhantomViolet" *> pure PhantomViolet
  , string "StrikerRuby" *> pure StrikerRuby
  , string "AegisOnyx" *> pure AegisOnyx
  , string "TempestAzure" *> pure TempestAzure
  , string "MirageAmethyst" *> pure MirageAmethyst
  , string "EclipseObsidian" *> pure EclipseObsidian
  ]

boolParser :: Parser Bool
boolParser = (string "True" *> pure True) <|> (string "False" *> pure False)

kvLine :: String -> Parser a -> Parser a
kvLine key valP = symbol key *> char ':' *> spaces *> valP <* spaces

unlockedListParser :: Parser [TankClass]
unlockedListParser = between (char '[') (char ']') (tankClassParser `sepBy` (char ',' *> spaces))

-- Phân tích cú pháp tệp tài khoản định dạng key-value
accountKVParser :: Parser PlayerAccount
accountKVParser = do
  _ <- spaces
  callsign <- kvLine "callsign" (many (satisfy (\c -> c /= '\n' && c /= '\r')))
  _ <- spaces
  pass <- kvLine "password" (many (satisfy (\c -> c /= '\n' && c /= '\r')))
  _ <- spaces
  gold <- kvLine "gold" integer
  _ <- spaces
  unlocked <- kvLine "unlocked" unlockedListParser
  _ <- spaces
  matches <- kvLine "matches_played" integer
  _ <- spaces
  m1 <- kvLine "claimed_m1" boolParser
  _ <- spaces
  m2 <- kvLine "claimed_m2" boolParser
  _ <- spaces
  m3 <- kvLine "claimed_m3" boolParser
  _ <- spaces
  eof
  return PlayerAccount
    { accCallsign      = callsign
    , accPassword      = pass
    , accGold          = gold
    , accUnlocked      = unlocked
    , accMatchesPlayed = matches
    , accClaimedM1     = m1
    , accClaimedM2     = m2
    , accClaimedM3     = m3
    }

-- Giải mã chuỗi tài khoản bằng parser combinator, dự phòng readMaybe cho định dạng cũ
parseAccountStrict :: String -> Maybe PlayerAccount
parseAccountStrict raw = case parse accountKVParser raw of
  Right acc -> Just acc
  Left _    -> readMaybe raw

serializeAccountKV :: PlayerAccount -> String
serializeAccountKV acc = unlines
  [ "callsign: " ++ accCallsign acc
  , "password: " ++ accPassword acc
  , "gold: " ++ show (accGold acc)
  , "unlocked: [" ++ intercalate "," (map show (accUnlocked acc)) ++ "]"
  , "matches_played: " ++ show (accMatchesPlayed acc)
  , "claimed_m1: " ++ show (accClaimedM1 acc)
  , "claimed_m2: " ++ show (accClaimedM2 acc)
  , "claimed_m3: " ++ show (accClaimedM3 acc)
  ]

-- Đọc và ghi tệp bằng ByteString nghiêm ngặt để giải phóng file descriptor ngay lập tức
readAccountStrict :: FilePath -> IO String
readAccountStrict !path = catch (fmap BS.unpack (BS.readFile path)) (\(_ :: SomeException) -> return "")

writeAccountStrict :: FilePath -> String -> IO ()
writeAccountStrict !path !content =
  catch (BS.writeFile path (BS.pack content))
        (\(e :: SomeException) -> hPutStrLn stderr ("Khong luu duoc tai khoan vao " ++ path ++ ": " ++ show e))

loadAccount :: String -> IO PlayerAccount
loadAccount !rawName = do
  createDirectoryIfMissing True "saves"
  let !safeName = if null rawName then "COMMANDER" else rawName
      !filePath = accountPath safeName
  exists <- doesFileExist filePath
  if not exists
    then do
      let !initAcc = defaultAccountWithPass safeName "123456"
      saveAccount initAcc
      return initAcc
    else do
      contentRes <- readAccountStrict filePath
      case parseAccountStrict contentRes of
        Just acc -> return acc
        Nothing  -> do
          let !initAcc = defaultAccountWithPass safeName "123456"
          saveAccount initAcc
          return initAcc

loginOrRegisterAccount :: String -> String -> IO (Either String PlayerAccount)
loginOrRegisterAccount !rawName !rawPass = case mkCallsign rawName of
  Left err -> return (Left err)
  Right name -> do
    createDirectoryIfMissing True "saves"
    let !safePass = if null rawPass then "123456" else rawPass
        !filePath = accountPath name
    exists <- doesFileExist filePath
    if not exists
      then do
        let !newAcc = defaultAccountWithPass name safePass
        saveAccount newAcc
        return (Right newAcc)
      else do
        content <- readAccountStrict filePath
        case parseAccountStrict content of
          Just acc
            | accPassword acc == safePass || null (accPassword acc) -> do
                let acc' = acc { accPassword = safePass }
                saveAccount acc'
                return (Right acc')
            | otherwise -> return (Left "Mật khẩu không chính xác! Vui lòng thử lại.")
          Nothing -> return (Left ("Tệp " ++ filePath ++ " bị hỏng, hãy đổi tên hoặc xóa tệp này."))

-- Kiểm tra tên phi công hợp lệ (1..14 ký tự ASCII an toàn cho đường dẫn tệp)
mkCallsign :: String -> Either String String
mkCallsign raw
  | null raw             = Left "Vui lòng nhập tên phi công!"
  | length raw > 14      = Left "Tên phi công tối đa 14 ký tự!"
  | not (all okChar raw) = Left "Tên chỉ gồm chữ không dấu, số và . _ - @"
  | otherwise            = Right raw
  where
    okChar c = isAscii c && (isAlphaNum c || c `elem` "._-@")

accountPath :: String -> FilePath
accountPath name = "saves" </> ("account_" ++ name ++ ".dat")

saveAccount :: PlayerAccount -> IO ()
saveAccount !acc = do
  createDirectoryIfMissing True "saves"
  let !safeName = if null (accCallsign acc) then "COMMANDER" else accCallsign acc
  writeAccountStrict (accountPath safeName) (serializeAccountKV acc)

buyHero :: TankClass -> PlayerAccount -> Either String PlayerAccount
buyHero !cls !acc
  | cls `elem` accUnlocked acc = Left "Ban da so huu vi tuong nay roi!"
  | accGold acc < cost         = Left ("Khong du Vang! Can " ++ show cost ++ " Vang.")
  | otherwise                  = Right acc
      { accGold     = accGold acc - cost
      , accUnlocked = cls : accUnlocked acc
      }
  where
    cost = heroCost cls

addTestGold :: Int -> PlayerAccount -> PlayerAccount
addTestGold !amount !acc =
  acc { accGold = max 0 (accGold acc + amount) }

recordMatchPlayed :: PlayerAccount -> PlayerAccount
recordMatchPlayed !acc =
  acc { accMatchesPlayed = accMatchesPlayed acc + 1 }

claimQuest :: Int -> PlayerAccount -> Either String PlayerAccount
claimQuest 1 !acc
  | accClaimedM1 acc           = Left "Nhiem vu 1 tran da nhan thuong roi!"
  | accMatchesPlayed acc < 1   = Left "Chua hoan thanh yeu cau: Can danh it nhat 1 tran!"
  | otherwise                  = Right acc { accGold = accGold acc + 300, accClaimedM1 = True }
claimQuest 2 !acc
  | accClaimedM2 acc           = Left "Nhiem vu 5 tran da nhan thuong roi!"
  | accMatchesPlayed acc < 5   = Left "Chua hoan thanh yeu cau: Can danh it nhat 5 tran!"
  | otherwise                  = Right acc { accGold = accGold acc + 800, accClaimedM2 = True }
claimQuest 3 !acc
  | accClaimedM3 acc           = Left "Nhiem vu 10 tran da nhan thuong roi!"
  | accMatchesPlayed acc < 10  = Left "Chua hoan thanh yeu cau: Can danh it nhat 10 tran!"
  | otherwise                  = Right acc { accGold = accGold acc + 1500, accClaimedM3 = True }
claimQuest _ _                 = Left "Nhiem vu khong hop le!"

claimDailyReward :: PlayerAccount -> PlayerAccount
claimDailyReward !acc =
  acc { accGold = accGold acc + 1000 }

addMatchReward :: Bool -> PlayerAccount -> PlayerAccount
addMatchReward !isWinner !acc =
  let bonus = if isWinner then 500 else 200
  in acc { accGold = accGold acc + bonus }
