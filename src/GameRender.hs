{-# LANGUAGE RecordWildCards #-}

module GameRender where

import Graphics.Gloss hiding (Polygon)
import qualified Graphics.Gloss.Interface.Pure.Game as Gloss (text)
import GameState
import Entities (Robot(..), RobotState(..), Projectile(..), GameObject(..), RobotTurret(..), Obstacle(..), ObstacleType(..))
import GameConstants (screenWidth, screenHeight, robotWidth, robotHeight, turretWidth, turretHeight, projectileWidth, projectileHeight, robotOriginalWidth, robotOriginalHeight)
import qualified Geometry as G
import GameUtils (getColorForId)
import Data.List (find, partition)
import Assets (Assets(..))
import ExplosionTypes (Explosion(..), ExplosionType(..)) -- Importamos Explosion de ExplosionTypes

-- | Dibuja el estado actual de la partida.
drawGame :: GameState -> Picture
drawGame gs =
  let (impactExplosions, deathExplosions) = partition ((== Impact) . expType) (explosions gs)
  in pictures $
     [ scale (fromIntegral screenWidth / 1024) (fromIntegral screenHeight / 768) (arenaPic (assets gs)) ] -- Fondo
  ++ map (drawObstacle (assets gs)) (obstacles gs) -- Obstáculos (dibujados antes que robots para que queden detrás)
  ++ map (drawExplosion (assets gs)) impactExplosions -- Explosiones de impacto
  ++ map (drawRobotBody (assets gs) gs) (robots gs)
  ++ map (drawProjectile (assets gs)) (projectiles gs)
  ++ map (drawExplosion (assets gs)) deathExplosions  -- Explosiones de muerte
  ++ [drawGameOverMessage gs]
  ++ [drawMapBoundary]

-- | Dibuja el mensaje del ganador o empate al terminar la partida.
drawGameOverMessage :: GameState -> Picture
drawGameOverMessage gs
  | countActiveRobots (robots gs) > 1 = blank
  | otherwise =
      let winnerTxt = case find (\r -> robotHealth r > 0) (robots gs) of
                        Just r  -> "Robot " ++ show (objId (robotBase r)) ++ " WINS!"
                        Nothing -> "IT'S A DRAW!"
      in pictures
          [ translate (-250) 0    $ scale 0.3 0.3 $ color white $ Gloss.text winnerTxt
          , translate (-200) (-50)$ scale 0.2 0.2 $ color white $ Gloss.text "Press R to return to menu"
          ]

-- | Dibuja el cuerpo y torreta de un robot con su barra de vida.
drawRobotBody :: Assets -> GameState -> Robot -> Picture
drawRobotBody Assets{..} gs r =
  let base        = robotBase r
      (x,y)       = objPos base
      bodyAngle   = G.rad2deg (objDir base) -90
      turretAngle = G.rad2deg (turretDir (robotTurret r)) - 90

      -- Escala base para los cuerpos de los tanques activos
      baseScale = 0.75

      tankBodyPic = case objId base `mod` 4 of -- Asigna un sprite de tanque según el ID
                      0 -> scale baseScale baseScale tankBlue
                      1 -> scale baseScale baseScale tankGreen
                      2 -> scale baseScale baseScale tankRed
                      _ -> scale baseScale baseScale tankSand
      scaledTurretPic = scale baseScale baseScale turretPic

      -- Calculamos una escala específica para el tanque destruido
      -- para que coincida con el tamaño 'visual' del robot activo.
      -- Por ahora, asumimos que todos los sprites de tanque (incluido el destruido) tienen una resolución base similar 
      -- y el scale 0.75 es el factor de ajuste general.
      destroyedTankScaled = scale baseScale baseScale destroyedTank

  in if robotState r == Destroyed
        then pictures
              [ translate x y $ rotate (-bodyAngle) destroyedTankScaled -- Usa el sprite de tanque destruido ya escalado
              ]
        else pictures
              ( [ translate x y $ rotate (-bodyAngle) tankBodyPic
                , translate x y $ rotate (-turretAngle) scaledTurretPic
                , if robotHealth r > 0 then translate x (y+20) $ drawHealthBar r else blank
                ] ++ drawStunEffect Assets{..} gs r
              )

-- | Dibuja la barra de salud encima del robot.
drawHealthBar :: Robot -> Picture
drawHealthBar r =
  let ratio   = max 0 (fromIntegral (robotHealth r) / fromIntegral (robotMaxHealth r))
      w       = robotWidth * 0.75 -- Escalar la barra de vida también
      yOff    = robotHeight * 0.75 / 2 + 10
      backBar = color red   $ rectangleSolid w 8
      hpBar   = color green $ rectangleSolid (w * ratio) 8
  in translate 0 yOff $ pictures [ backBar, translate (-(w - w * ratio) / 2) 0 hpBar ]

-- | Dibuja un proyectil en movimiento.
drawProjectile :: Assets -> Projectile -> Picture
drawProjectile Assets{..} p =
  let (x,y) = objPos (projBase p)
      a     = G.rad2deg (objDir (projBase p))
      bulletPic = case projOwner p `mod` 4 of -- Usa projOwner para seleccionar el sprite de la bala
                    0 -> bulletBlue
                    1 -> bulletGreen
                    2 -> bulletRed
                    _ -> bulletSand
  in translate x y $ rotate (-a +90) bulletPic

-- | Dibuja una explosión circular con desvanecimiento progresivo.
drawExplosion :: Assets -> Explosion -> Picture
drawExplosion Assets{..} Explosion{..} =
  let (x,y) = expPos
      frame = explosionFrames !! expFrameIdx
      scaledFrame = case expType of
                      Impact -> scale 0.5 0.5 frame
                      Death  -> frame
  in translate x y scaledFrame

-- | Dibuja el límite del mapa.
drawMapBoundary :: Picture
drawMapBoundary = color white $ rectangleWire (fromIntegral screenWidth) (fromIntegral screenHeight)

-- | Dibuja un obstáculo según su tipo.
drawObstacle :: Assets -> Obstacle -> Picture
drawObstacle Assets{..} obs =
  let (x, y) = obstaclePos obs
      w = obstacleWidth obs
      h = obstacleHeight obs
      
      -- Selecciona el sprite según el tipo de obstáculo
      obstaclePic = case obstacleType obs of
                     CrateObstacle -> crateWood
                     FenceObstacle -> fenceRed
                     BarricadeObstacle -> barricadeWood
                     ExplosiveBarrel -> barrelBlack
                     OilSpillObstacle -> oilSpill
      
      -- Escala el sprite para que coincida con el tamaño del obstáculo
      -- Los sprites se escalan proporcionalmente al tamaño definido
      -- Asumimos un tamaño base aproximado de 64x64 píxeles para los sprites
      scaleX = w / 64.0
      scaleY = h / 64.0
  in translate x y $ scale scaleX scaleY obstaclePic

-- | Dibuja un efecto visual sobre el robot si está stuneado.
drawStunEffect :: Assets -> GameState -> Robot -> [Picture]
drawStunEffect Assets{..} gs r =
  if robotStunTime r > 0.0
    then
      let (x,y) = objPos (robotBase r)
          -- Alternar entre dos sprites de estrella para la animación
          starPic = if (floor (time gs * 5) `mod` 2) == (0 :: Int) -- Flash every 0.2 seconds
                      then stunStar1
                      else stunStar2
          -- Posición de la estrella (un poco por encima del robot)
          starOffset = 40.0 -- Ajustar según el tamaño del robot
          scaledStar = scale 0.5 0.5 starPic -- Ajustar escala de la estrella
      in [translate x (y + starOffset) scaledStar]
    else []
