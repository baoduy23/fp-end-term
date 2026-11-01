-- Hero.hs
-- Hồ sơ 10 vị tướng: chỉ số cơ bản, đòn đánh thường, tuyệt kỹ và hình dạng vùng tác dụng

{-# LANGUAGE BangPatterns #-}

module Hero
  ( SkillShape(..)
  , ShotStyle(..)
  , HeroSpec(..)
  , HasHero(..)
  , heroSpec
  , heroCost
  , heroShortName
  , heroIsBase
  , heroMaxHealth
  , getClassMaxHP
  , heroBaseFireCooldown
  , shotBonusHalf
  , ultBonus
  , heroShotDamageHalf
  , heroBulletDamage
  , heroUltDamage
  , heroUltRange
  , ultDamageCap
  , showHalf
  , ultDescription
  , dummyTankOfClass
  , maxHealthOf
  , ultRangeOf
  ) where

import Types

-- Hình dạng vùng tác dụng của tuyệt kỹ (đơn vị: ô lưới)
data SkillShape
  = Radial !Int
  | BlinkBurst !Int !Int
  | Beam !Int
  | Cross !Int
  | Shell !Int
  | Volley !Int
  | Pull !Int
  deriving (Eq, Show)

data ShotStyle
  = ShotIce | ShotHeavy | ShotNeedle | ShotSolar | ShotVoid
  | ShotRocket | ShotQuartz | ShotArc | ShotPrism | ShotDark
  deriving (Eq, Show, Enum, Bounded)

data HeroSpec = HeroSpec
  { specClass        :: !TankClass
  , specShortName    :: !String
  , specTitle        :: !String
  , specRole         :: !String
  , specCost         :: !Int
  , specMaxHp        :: !Int
  , specShotBase     :: !Int
  , specFireCooldown :: !Int
  , specShotStyle    :: !ShotStyle
  , specUltName      :: !String
  , specUltBase      :: !Int
  , specUltShape     :: !SkillShape
  , specUltEffect    :: !String
  , specUltTrueDmg   :: !Bool
  } deriving (Eq, Show)

heroSpec :: TankClass -> HeroSpec
heroSpec cls = case cls of
  VanguardCyan -> HeroSpec
    { specClass = cls, specShortName = "BĂNG GIÁ", specTitle = "Tiên Phong Băng Giá"
    , specRole = "Vai trò: Đấu sĩ - Khống chế diện rộng", specCost = 0
    , specMaxHp = 500, specShotBase = 28, specFireCooldown = 9, specShotStyle = ShotIce
    , specUltName = "CHẤN ĐỘNG BĂNG GIÁ", specUltBase = 115, specUltShape = Radial 3
    , specUltEffect = "và đóng băng kẻ địch 1.8 giây.", specUltTrueDmg = False }
  TitanCrimson -> HeroSpec
    { specClass = cls, specShortName = "BẠO CHÚA", specTitle = "Thiết Giáp Bạo Chúa"
    , specRole = "Vai trò: Pháo thủ hạng nặng - Càn quét", specCost = 0
    , specMaxHp = 530, specShotBase = 32, specFireCooldown = 12, specShotStyle = ShotHeavy
    , specUltName = "PHÁO KÍCH HỎA THẦN", specUltBase = 130, specUltShape = Shell 8
    , specUltEffect = "đẩy lùi 2 ô, phá giáp (+20% ST nhận) 3s.", specUltTrueDmg = False }
  ViperEmerald -> HeroSpec
    { specClass = cls, specShortName = "ĐỘC XÀ", specTitle = "Độc Xà Hồi Phục"
    , specRole = "Vai trò: Hỗ trợ sinh tồn - Độc tính", specCost = 0
    , specMaxHp = 500, specShotBase = 28, specFireCooldown = 9, specShotStyle = ShotNeedle
    , specUltName = "KHIÊN ĐỘC PHỤC HỒI", specUltBase = 95, specUltShape = Radial 2
    , specUltEffect = "hồi 130 máu, độc -10 máu mỗi 0.5s trong 3s.", specUltTrueDmg = False }
  SolarisGold -> HeroSpec
    { specClass = cls, specShortName = "QUANG NĂNG", specTitle = "Quang Năng Hủy Diệt"
    , specRole = "Vai trò: Xạ thủ bắn tỉa - Laze xuyên thấu", specCost = 0
    , specMaxHp = 500, specShotBase = 30, specFireCooldown = 9, specShotStyle = ShotSolar
    , specUltName = "TIA THÁI DƯƠNG", specUltBase = 135, specUltShape = Beam 6
    , specUltEffect = "sát thương chuẩn, chói mắt (nạp đạn x2) 2.5s.", specUltTrueDmg = True }
  PhantomViolet -> HeroSpec
    { specClass = cls, specShortName = "HƯ KHÔNG", specTitle = "Hư Không Sát Thủ"
    , specRole = "Vai trò: Sát thủ đột kích - Biến ảnh", specCost = 0
    , specMaxHp = 500, specShotBase = 30, specFireCooldown = 7, specShotStyle = ShotVoid
    , specUltName = "BƯỚC NHẢY HƯ KHÔNG", specUltBase = 115, specUltShape = BlinkBurst 4 2
    , specUltEffect = "tốc biến xuyên tường, tàng hình + tăng tốc 2s.", specUltTrueDmg = False }
  StrikerRuby -> HeroSpec
    { specClass = cls, specShortName = "HỒNG NGỌC", specTitle = "Xung Kích Hồng Ngọc"
    , specRole = "Vai trò: Đấu sĩ hỏa tiễn - Càn quét 3 làn", specCost = 1500
    , specMaxHp = 550, specShotBase = 32, specFireCooldown = 8, specShotStyle = ShotRocket
    , specUltName = "MƯA PHÁO HỎA TIỄN CHÙM", specUltBase = 140, specUltShape = Volley 7
    , specUltEffect = "bản thân chạy và bắn nhanh x2 trong 3s.", specUltTrueDmg = False }
  AegisOnyx -> HeroSpec
    { specClass = cls, specShortName = "HẮC NGỌC", specTitle = "Bảo Hộ Hắc Ngọc"
    , specRole = "Vai trò: Siêu đỡ đòn - Phản chấn từ trường", specCost = 3000
    , specMaxHp = 620, specShotBase = 34, specFireCooldown = 11, specShotStyle = ShotQuartz
    , specUltName = "THÀNH TRÌ PHẢN CHẤN", specUltBase = 120, specUltShape = Radial 2
    , specUltEffect = "giáp ảo +160, phản 30% ST đạn 3s, đẩy lùi.", specUltTrueDmg = False }
  TempestAzure -> HeroSpec
    { specClass = cls, specShortName = "LAM NGỌC", specTitle = "Cuồng Phong Lam Ngọc"
    , specRole = "Vai trò: Pháp sư cơ động - Bão điện tự dẫn", specCost = 4500
    , specMaxHp = 600, specShotBase = 32, specFireCooldown = 6, specShotStyle = ShotArc
    , specUltName = "BÃO ĐIỆN TỪ EMP", specUltBase = 140, specUltShape = Radial 4
    , specUltEffect = "ST chuẩn, EMP khóa bắn + tuyệt kỹ 2s.", specUltTrueDmg = True }
  MirageAmethyst -> HeroSpec
    { specClass = cls, specShortName = "THẠCH ANH", specTitle = "Ảo Ảnh Thạch Anh"
    , specRole = "Vai trò: Xạ thủ ảo ảnh - Lăng kính toàn phương", specCost = 6500
    , specMaxHp = 650, specShotBase = 35, specFireCooldown = 7, specShotStyle = ShotPrism
    , specUltName = "ẢO ẢNH TOÀN PHƯƠNG", specUltBase = 150, specUltShape = Cross 3
    , specUltEffect = "tàng hình và đạn xuyên giáp 3 giây.", specUltTrueDmg = False }
  EclipseObsidian -> HeroSpec
    { specClass = cls, specShortName = "HẮC DIỆU", specTitle = "Nhật Thực Hắc Diệu"
    , specRole = "Vai trò: Thần thoại - Hố đen trọng trường", specCost = 9000
    , specMaxHp = 700, specShotBase = 38, specFireCooldown = 10, specShotStyle = ShotDark
    , specUltName = "HỐ ĐEN TRỌNG TRƯỜNG", specUltBase = 160, specUltShape = Pull 4
    , specUltEffect = "hút địch 2 ô, ST chuẩn, suy yếu -30% 3s.", specUltTrueDmg = True }

-- Lớp kiểu truy xuất hồ sơ tướng từ TankClass, Tank hoặc HeroSpec
class HasHero a where
  specOf :: a -> HeroSpec

instance HasHero TankClass where
  specOf = heroSpec

instance HasHero Tank where
  specOf = heroSpec . tankClass

instance HasHero HeroSpec where
  specOf = id

maxHealthOf :: HasHero a => a -> Int
maxHealthOf = specMaxHp . specOf

ultRangeOf :: HasHero a => a -> Int
ultRangeOf = shapeReach . specUltShape . specOf

heroCost :: TankClass -> Int
heroCost = specCost . heroSpec

heroShortName :: TankClass -> String
heroShortName = specShortName . heroSpec

heroIsBase :: TankClass -> Bool
heroIsBase cls = heroCost cls == 0

heroMaxHealth :: TankClass -> Int
heroMaxHealth = specMaxHp . heroSpec

getClassMaxHP :: TankClass -> Int
getClassMaxHP = heroMaxHealth

heroBaseFireCooldown :: TankClass -> Int
heroBaseFireCooldown = specFireCooldown . heroSpec

-- Sát thương đạn thường tính theo đơn vị nửa điểm máu (+7.5 máu = +15 nửa máu)
shotBonusHalf :: Int
shotBonusHalf = 15

ultBonus :: Int
ultBonus = 20

heroShotDamageHalf :: TankClass -> Int
heroShotDamageHalf cls = 2 * specShotBase (heroSpec cls) + shotBonusHalf

heroBulletDamage :: TankClass -> Double
heroBulletDamage cls = fromIntegral (heroShotDamageHalf cls) / 2

heroUltDamage :: TankClass -> Int
heroUltDamage cls = specUltBase (heroSpec cls) + ultBonus

-- Trần sát thương của một lần tuyệt kỹ lên mục tiêu
ultDamageCap :: TankClass -> Int
ultDamageCap targetCls = min 185 (heroMaxHealth targetCls `div` 3 + ultBonus)

shapeReach :: SkillShape -> Int
shapeReach shape = case shape of
  Radial r       -> r
  BlinkBurst k r -> k + r
  Beam r         -> r
  Cross r        -> r
  Shell r        -> r
  Volley r       -> r
  Pull r         -> r

heroUltRange :: TankClass -> Int
heroUltRange = ultRangeOf

showHalf :: Int -> String
showHalf h
  | even h    = show (h `div` 2)
  | otherwise = show (h `div` 2) ++ ".5"

ultDescription :: TankClass -> (String, String)
ultDescription cls =
  let sp  = heroSpec cls
      dmg = show (heroUltDamage cls) ++ " ST" ++ (if specUltTrueDmg sp then " chuẩn" else "")
      area = case specUltShape sp of
        Radial r       -> "Vòng tỏa " ++ show r ++ " ô"
        BlinkBurst k r -> "Tốc biến " ++ show k ++ " ô, nổ " ++ show r ++ " ô"
        Beam r         -> "Tia laze dài " ++ show r ++ " ô"
        Cross r        -> "Lăng kính chữ thập " ++ show r ++ " ô"
        Shell r        -> "Đại bác bay " ++ show r ++ " ô"
        Volley r       -> "3 làn hỏa tiễn dài " ++ show r ++ " ô"
        Pull r         -> "Hố đen bán kính " ++ show r ++ " ô"
  in (area ++ ": " ++ dmg, specUltEffect sp)

dummyTankOfClass :: TankClass -> Tank
dummyTankOfClass cls = Tank
  { tankId              = 0
  , tankPos             = Pos 0 0
  , tankDir             = North
  , tankHealth          = heroMaxHealth cls
  , tankEnergy          = 100
  , tankFireCooldown    = 0
  , tankMoveCooldown    = 0
  , tankRespawnTimer    = 0
  , tankInvincibleTimer = 0
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
  , tankName            = ""
  , tankArmorBreakTimer = 0
  , tankPoisonTimer     = 0
  , tankBlindTimer      = 0
  , tankHasteTimer      = 0
  , tankReflectTimer    = 0
  , tankWeakenTimer     = 0
  , tankDamageCarry     = 0
  }
