module GameUtils
  ( findById
  , findProjById
  , replaceRobot
  , getColorForId
  , clampPosition
  ) where

import GameState
import Entities
import Data.List (find)
import Graphics.Gloss
import qualified Geometry as G
import GameConstants (screenWidth, screenHeight, robotWidth, robotHeight)

-- | Busca un robot por su identificador dentro de una lista.
findById :: Int -> [Robot] -> Maybe Robot
findById i = find (\r -> objId (robotBase r) == i)

-- | Busca un proyectil por su identificador dentro de una lista.
findProjById :: Int -> [Projectile] -> Maybe Projectile
findProjById i = find (\p -> objId (projBase p) == i)

-- | Reemplaza un robot existente por una versión actualizada.
--   Si no existe otro con el mismo ID, simplemente lo agrega.
replaceRobot :: Robot -> [Robot] -> [Robot]
replaceRobot r' =
  (r' :) . filter (\r -> objId (robotBase r) /= objId (robotBase r'))

-- | Asigna un color a cada robot según su ID.
--   Esto permite distinguir visualmente los bots en pantalla.
getColorForId :: Int -> Color
getColorForId 1 = light blue
getColorForId 2 = light red
getColorForId 3 = light green
getColorForId 4 = light yellow
getColorForId _ = white

-- | Restringe una posición para que no salga del área visible de la pantalla.
clampPosition :: G.Point -> G.Point
clampPosition (x,y) = 
    ( max (-fromIntegral screenWidth / 2 + robotWidth / 2) (min (fromIntegral screenWidth / 2 - robotWidth / 2) x)
    , max (-fromIntegral screenHeight / 2 + robotHeight / 2) (min (fromIntegral screenHeight / 2 - robotHeight / 2) y)
    )
