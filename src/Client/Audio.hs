-- Client/Audio.hs
-- Phát âm thanh hiệu ứng và nhạc nền qua WinMM

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Client.Audio
  ( SoundEvent(..)
  , initAudioSystem
  , playSound
  , playSoundAt
  , startBgmLoop
  , stopBgm
  , setBgmMute
  ) where

import System.Directory (doesFileExist, makeAbsolute, createDirectoryIfMissing)
import Control.Exception (SomeException, catch)
import Control.Concurrent (forkIO)
import System.Random (randomRIO)
import Foreign.C.String (withCWString, CWString)
import Foreign.C.Types (CUInt(..), CBool(..))
import Foreign.Ptr (nullPtr, Ptr)

foreign import ccall safe "PlaySoundW"
  c_playSoundW :: CWString -> Ptr () -> CUInt -> IO CBool

foreign import ccall safe "mciSendStringW"
  c_mciSendStringW :: CWString -> Ptr () -> CUInt -> Ptr () -> IO CUInt

-- Cờ SND_ASYNC | SND_NODEFAULT | SND_FILENAME
sndFlagsAsync :: CUInt
sndFlagsAsync = 0x00020003

data SoundEvent
  = SndBgmMenu
  | SndBgmBattle
  | SndBtnClick
  | SndClick
  | SndHeroSelect
  | SndHeroLock
  | SndBuyHero
  | SndClaimReward
  | SndCountdown
  | SndMatchStart
  | SndShoot
  | SndHit
  | SndExplode
  | SndUltimate
  | SndPowerUp
  | SndTakedown
  | SndRespawn
  | SndChat
  | SndVictory
  | SndEvent
  deriving (Eq, Show)

soundFileList :: SoundEvent -> [FilePath]
soundFileList evt = case evt of
  SndBgmMenu     -> ["assets/audio/bgm_lobby.wav", "assets/audio/bgm_menu.wav", "assets/audio/bgm.wav"]
  SndBgmBattle   -> ["assets/audio/bgm_battle.wav", "assets/audio/bgm.wav"]
  SndBtnClick    -> ["assets/audio/ui_click.wav", "assets/audio/click.wav"]
  SndClick       -> ["assets/audio/ui_click.wav", "assets/audio/click.wav"]
  SndHeroSelect  -> ["assets/audio/ui_hover.wav", "assets/audio/hero_select.wav", "assets/audio/click.wav"]
  SndHeroLock    -> ["assets/audio/ui_lock_hero.wav", "assets/audio/hero_lock.wav", "assets/audio/click.wav"]
  SndBuyHero     -> ["assets/audio/ui_buy_success.wav", "assets/audio/buy_hero.wav", "assets/audio/powerup.wav"]
  SndClaimReward -> ["assets/audio/ui_reward_gold.wav", "assets/audio/reward.wav", "assets/audio/powerup.wav"]
  SndCountdown   -> ["assets/audio/countdown_tick.wav", "assets/audio/countdown.wav", "assets/audio/click.wav"]
  SndMatchStart  -> ["assets/audio/match_start.wav", "assets/audio/event.wav"]
  SndShoot       -> ["assets/audio/shoot_normal.wav", "assets/audio/shoot.wav"]
  SndHit         -> ["assets/audio/bullet_hit.wav", "assets/audio/hit.wav"]
  SndExplode     -> ["assets/audio/tank_explode.wav", "assets/audio/explode.wav"]
  SndUltimate    -> ["assets/audio/ultimate.wav", "assets/audio/skill_titan.wav", "assets/audio/explode.wav"]
  SndPowerUp     -> ["assets/audio/orb_pickup.wav", "assets/audio/powerup.wav"]
  SndTakedown    -> ["assets/audio/kill_announce.wav", "assets/audio/takedown.wav"]
  SndRespawn     -> ["assets/audio/respawn_shield.wav", "assets/audio/respawn.wav"]
  SndChat        -> ["assets/audio/chat_msg.wav", "assets/audio/chat.wav", "assets/audio/click.wav"]
  SndVictory     -> ["assets/audio/match_victory.wav", "assets/audio/victory.wav", "assets/audio/takedown.wav"]
  SndEvent       -> ["assets/audio/event.wav", "assets/audio/powerup.wav"]

resolveSoundFile :: SoundEvent -> IO (Maybe FilePath)
resolveSoundFile evt = checkFiles (soundFileList evt)
  where
    checkFiles [] = return Nothing
    checkFiles (p:ps) = do
      exists <- doesFileExist p
      if exists
        then Just <$> makeAbsolute p
        else checkFiles ps

initAudioSystem :: IO ()
initAudioSystem = do
  createDirectoryIfMissing True "assets/audio"
  putStrLn "[Audio] Khoi tao he thong am thanh file-based."

playSound :: Bool -> SoundEvent -> IO ()
playSound !isMuted !evt
  | isMuted   = return ()
  | otherwise = do
      _ <- forkIO $ do
        mPath <- resolveSoundFile evt
        case mPath of
          Just absPath -> do
            catch (withCWString absPath $ \cPath -> do
                     _ <- c_playSoundW cPath nullPtr sndFlagsAsync
                     return ()
                  )
                  (\(_ :: SomeException) -> return ())
          Nothing -> return ()
      return ()

playSoundVol :: Bool -> Int -> SoundEvent -> IO ()
playSoundVol !isMuted !vol !evt
  | isMuted   = return ()
  | otherwise = do
      _ <- forkIO $ do
        mPath <- resolveSoundFile evt
        case mPath of
          Just absPath -> do
            chan <- randomRIO (0, 7 :: Int)
            let !alias   = "sfx" ++ show chan
                !safeVol = max 0 (min 1000 vol)
            catch (do
                    _ <- withCWString ("close " ++ alias) (\c -> c_mciSendStringW c nullPtr 0 nullPtr)
                    _ <- withCWString ("open \"" ++ absPath ++ "\" type mpegvideo alias " ++ alias) (\c -> c_mciSendStringW c nullPtr 0 nullPtr)
                    _ <- withCWString ("setaudio " ++ alias ++ " volume to " ++ show safeVol) (\c -> c_mciSendStringW c nullPtr 0 nullPtr)
                    _ <- withCWString ("play " ++ alias ++ " from 0") (\c -> c_mciSendStringW c nullPtr 0 nullPtr)
                    return ()
                  )
                  (\(_ :: SomeException) -> return ())
          Nothing -> return ()
      return ()

-- Suy giảm âm lượng theo khoảng cách Euclid từ người nghe đến nguồn âm
playSoundAt :: Bool -> (Float, Float) -> (Float, Float) -> SoundEvent -> IO ()
playSoundAt !isMuted (!lx, !ly) (!sx, !sy) !evt
  | isMuted   = return ()
  | otherwise = do
      let !dx     = sx - lx
          !dy     = sy - ly
          !dist   = sqrt (dx * dx + dy * dy)
      if dist > 1200.0
        then return ()
        else do
          let !factor = 1.0 - min 1.0 (max 0.0 (dist / 850.0))
              !vol    = round (150.0 + 850.0 * (factor ** 1.35)) :: Int
          playSoundVol isMuted vol evt

startBgmLoop :: Bool -> SoundEvent -> IO ()
startBgmLoop !isMuted !bgmEvt = do
  _ <- forkIO $ do
    stopBgm
    mPath <- resolveSoundFile bgmEvt
    case mPath of
      Just absPath -> do
        let !alias = "bgm"
            vol :: Int
            !vol = if isMuted then 0 else 520
        catch (do
                _ <- withCWString ("close " ++ alias) (\c -> c_mciSendStringW c nullPtr 0 nullPtr)
                _ <- withCWString ("open \"" ++ absPath ++ "\" type mpegvideo alias " ++ alias) (\c -> c_mciSendStringW c nullPtr 0 nullPtr)
                _ <- withCWString ("setaudio " ++ alias ++ " volume to " ++ show vol) (\c -> c_mciSendStringW c nullPtr 0 nullPtr)
                _ <- withCWString ("play " ++ alias ++ " repeat") (\c -> c_mciSendStringW c nullPtr 0 nullPtr)
                return ()
              )
              (\(_ :: SomeException) -> return ())
      Nothing -> return ()
  return ()

stopBgm :: IO ()
stopBgm = do
  let !alias = "bgm"
  _ <- catch (withCWString ("stop " ++ alias) (\c -> c_mciSendStringW c nullPtr 0 nullPtr))
             (\(_ :: SomeException) -> return 0)
  _ <- catch (withCWString ("close " ++ alias) (\c -> c_mciSendStringW c nullPtr 0 nullPtr))
             (\(_ :: SomeException) -> return 0)
  return ()

setBgmMute :: Bool -> IO ()
setBgmMute !muted = do
  let !alias  = "bgm"
      !volStr = if muted then "0" else "520"
  _ <- catch (withCWString ("setaudio " ++ alias ++ " volume to " ++ volStr) (\c -> c_mciSendStringW c nullPtr 0 nullPtr))
             (\(_ :: SomeException) -> return 0)
  return ()
