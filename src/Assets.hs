module Assets where

import Graphics.Gloss
import Graphics.Gloss.Juicy
import System.FilePath ((</>))

data Assets = Assets
  { tankBlue        :: Picture
  , tankGreen       :: Picture
  , tankRed         :: Picture
  , tankSand        :: Picture
  , turretPic       :: Picture
  , bulletBlue      :: Picture
  , bulletDark      :: Picture
  , bulletGreen     :: Picture
  , bulletRed       :: Picture
  , bulletSand      :: Picture
  , explosionFrames :: [Picture]
  , arenaPic        :: Picture
  , destroyedTank   :: Picture 
  , crateWood       :: Picture 
  , fenceRed        :: Picture 
  , barricadeWood   :: Picture
  , barrelBlack     :: Picture
  , oilSpill        :: Picture
  , stunStar1       :: Picture
  , stunStar2       :: Picture
  }

loadAssets :: IO Assets
loadAssets = do
  tankBluePic   <- loadJuicyPNG "assets/robots/tankBody_blue.png"
  tankGreenPic  <- loadJuicyPNG "assets/robots/tankBody_green.png"
  tankRedPic    <- loadJuicyPNG "assets/robots/tankBody_red.png"
  tankSandPic   <- loadJuicyPNG "assets/robots/tankBody_sand.png"
  turretPic'    <- loadJuicyPNG "assets/robots/turret.png"

  bulletBluePic  <- loadJuicyPNG "assets/robots/bulletBlue2.png"
  bulletDarkPic  <- loadJuicyPNG "assets/robots/bulletDark2.png"
  bulletGreenPic <- loadJuicyPNG "assets/robots/bulletGreen2.png"
  bulletRedPic   <- loadJuicyPNG "assets/robots/bulletRed2.png"
  bulletSandPic  <- loadJuicyPNG "assets/robots/bulletSand2.png"

  explosion1 <- loadJuicyPNG "assets/effects/explosion1.png"
  explosion2 <- loadJuicyPNG "assets/effects/explosion2.png"
  explosion3 <- loadJuicyPNG "assets/effects/explosion3.png"
  explosion4 <- loadJuicyPNG "assets/effects/explosion4.png"
  explosion5 <- loadJuicyPNG "assets/effects/explosion5.png"

  arenaPic'     <- loadJuicyPNG "assets/map/mapa.png"
  destroyedTankPic <- loadJuicyPNG "assets/robots/tanqueDestruido.png"
  
  crateWoodPic  <- loadJuicyPNG "assets/obstacles/crateWood.png"
  fenceRedPic   <- loadJuicyPNG "assets/obstacles/fenceRed.png"
  barricadeWoodPic <- loadJuicyPNG "assets/obstacles/barricadeWood.png"
  barrelBlackPic <- loadJuicyPNG "assets/obstacles/barrelBlack_side.png"
  oilSpillPic    <- loadJuicyPNG "assets/obstacles/oilSpill_large.png"
  stunStar1Pic   <- loadJuicyPNG "assets/effects/estrella1.png"
  stunStar2Pic   <- loadJuicyPNG "assets/effects/estrella2.png"

  let explosionPics = [explosion1, explosion2, explosion3, explosion4, explosion5]

  case (tankBluePic, tankGreenPic, tankRedPic, tankSandPic, turretPic',
        bulletBluePic, bulletDarkPic, bulletGreenPic, bulletRedPic, bulletSandPic,
        sequence explosionPics, arenaPic', destroyedTankPic, crateWoodPic, fenceRedPic, barricadeWoodPic, barrelBlackPic, oilSpillPic, stunStar1Pic, stunStar2Pic) of
    (Just tb, Just tg, Just tr, Just ts, Just tp,
     Just bb, Just bd, Just bg, Just br, Just bs,
     Just eps, Just ap, Just dtp, Just cw, Just fr, Just bw, Just bbl, Just osp, Just ss1, Just ss2) ->
      return Assets { tankBlue = tb, tankGreen = tg, tankRed = tr, tankSand = ts, turretPic = tp
                    , bulletBlue = bb, bulletDark = bd, bulletGreen = bg, bulletRed = br, bulletSand = bs
                    , explosionFrames = eps, arenaPic = ap, destroyedTank = dtp
                    , crateWood = cw, fenceRed = fr, barricadeWood = bw, barrelBlack = bbl, oilSpill = osp
                    , stunStar1 = ss1, stunStar2 = ss2
                    }
    _ -> error "Failed to load assets! Missing image files or invalid paths."
