-- Client/Input.hs
-- Chuẩn hóa phím khi bộ gõ tiếng Việt (Unikey, EVKey, VIE của Windows) đang bật

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Client.Input
  ( normalizeKeyChar
  , normalizeKeyCharWith
  , isImeComposedChar
  , disableWindowsIme
  ) where

import Control.Exception (SomeException, catch)
import Data.Char (isAscii, toLower, ord)
import Foreign.C.Types (CInt(..), CUInt(..))

-- Vô hiệu hóa IME và Text Services Framework của Windows cho tiến trình hiện tại
foreign import ccall unsafe "ImmDisableIME"
  c_immDisableIME :: CUInt -> IO CInt

foreign import ccall unsafe "ImmDisableTextFrameService"
  c_immDisableTextFrameService :: CUInt -> IO CInt

disableWindowsIme :: IO ()
disableWindowsIme =
  (do _ <- c_immDisableIME maxBound
      _ <- c_immDisableTextFrameService maxBound
      return ())
  `catch` (\(_ :: SomeException) -> return ())

normalizeKeyChar :: Char -> Char
normalizeKeyChar = normalizeKeyCharWith False

-- Khôi phục phím vật lý vừa nhấn từ ký tự đã bị bộ gõ Telex hoặc bàn phím VIE biến đổi
normalizeKeyCharWith :: Bool -> Char -> Char
normalizeKeyCharWith afterBackspace c
  | afterBackspace, Just k <- lookupIn telexComposed = k
  | Just k <- lookupIn layoutChars                   = k
  | isAscii c                                        = toLower c
  | otherwise                                        = c
  where
    lookupIn table = case [k | (cs, k) <- table, c `elem` cs] of
      (k : _) -> Just k
      []      -> Nothing

isImeComposedChar :: Char -> Bool
isImeComposedChar c = not (isAscii c) || ord c < 32 && c `notElem` "\b\t\r\n\ESC" || c `elem` "?uU"

layoutChars :: [(String, Char)]
layoutChars =
  [ ("ưƯ\xB0\xAF\xFD\xDD?uU", 'w')
  , ("ăĂ\x03\x02\xE3\xC3",     '1')
  , ("âÂ",                     '2')
  , ("đĐ\x11\x10\xF0\xD0",     '0')
  ]

telexComposed :: [(String, Char)]
telexComposed =
  [ ("ăĂ\x03\x02\xE3\xC3ưƯ\xB0\xAFơƠ\xA1\xA0\xF5\xD5", 'w')
  , ("âÂ", 'a')
  , ("êÊ", 'e')
  , ("ôÔ", 'o')
  , ("đĐ\x11\x10\xF0\xD0", 'd')
  , ("\xEC" ++ acuteChars, 's')
  , ("\xCC" ++ graveChars, 'f')
  , ("\xD2" ++ hookChars,  'r')
  , ("\xDE" ++ tildeChars, 'x')
  , ("\xF2" ++ dotChars,   'j')
  ]

acuteChars, graveChars, hookChars, tildeChars, dotChars :: String
acuteChars = "áấắéếíóốớúứýÁẤẮÉẾÍÓỐỚÚỨÝ"
graveChars = "àầằèềìòồờùừỳÀẦẰÈỀÌÒỒỜÙỪỲ"
hookChars  = "ảẩẳẻểỉỏổởủửỷẢẨẲẺỂỈỎỔỞỦỬỶ"
tildeChars = "ãẫẵẽễĩõỗỡũữỹÃẪẴẼỄĨÕỖỠŨỮỸ"
dotChars   = "ạậặẹệịọộợụựỵẠẬẶẸỆỊỌỘỢỤỰỴ"
