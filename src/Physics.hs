module Physics
  ( updatePosition
  , updateVelocity
  , updateRobotVelocity
  , detectedAgent
  , isCollidingAhead
  , isCollidingBehind
  ) where

import qualified Geometry as G
import Entities
import GameState (GameState(..), GameMap(..))
import CollisionSAT (checkCollision)

-- Actualiza posición de un GameObject
updatePosition :: GameObject -> Float -> GameObject
updatePosition obj dt =
  obj { objPos = G.addVec (objPos obj) (G.vectorXScalar dt (objVel obj)) }


-- Actualiza velocidad según una acción
updateVelocity :: Float -> G.Angle -> G.Velocity
updateVelocity speed dir = (speed * cos dir, speed * sin dir)

-- Aplica una nueva velocidad a un robot
updateRobotVelocity :: Robot -> G.Velocity -> Robot
updateRobotVelocity r newV =
  r { robotBase = (robotBase r) { objVel = newV } }

-- Detectar si un robot ve a otro
detectedAgent :: Robot -> Robot -> Bool
detectedAgent r1 r2 =
  let p1 = objPos (robotBase r1)
      p2 = objPos (robotBase r2)
      range = case robotAIType r1 of
                Hunter { hunterDetectionRange = r }  -> r
                Evasive { evasiveDetectionRange = r } -> r
  in G.distanceBetween p1 p2 <= range && objId (robotBase r1) /= objId (robotBase r2)

-- | Predice si un robot colisionará con una pared u obstáculo si avanza un poco.
isCollidingAhead :: GameState -> Robot -> Bool
isCollidingAhead gs r =
  let currentPos = objPos (robotBase r)
      currentDir = objDir (robotBase r)
      -- Predice una posición ligeramente hacia adelante
      lookAheadDist = 60.0 -- Distancia a mirar hacia adelante
      (vx, vy) = (lookAheadDist * cos currentDir, lookAheadDist * sin currentDir)
      predictedPos = G.addVec currentPos (vx, vy)
      
      -- Crea un objeto hipotético en la posición futura
      -- Usamos el mismo robotBase pero con la nueva posición y forma
      predictedRobotBase = (robotBase r) { objPos = predictedPos, objShape = (G.moveTo (objShape (robotBase r)) predictedPos) }
      
      -- Filtra los obstáculos no sólidos (como OilSpillObstacle)
      solidObstacles = filter (\obs -> obstacleType obs /= OilSpillObstacle) (obstacles gs)
      
      -- Verifica colisión con paredes y obstáculos sólidos
      wallsCollision = any (checkCollision (objShape predictedRobotBase)) (mapWalls (gameMap gs))
      obstaclesCollision = any (\obs -> checkCollision (objShape predictedRobotBase) (obstacleShape obs)) solidObstacles
  in wallsCollision || obstaclesCollision

-- | Predice si un robot colisionará con una pared u obstáculo si retrocede un poco.
isCollidingBehind :: GameState -> Robot -> Bool
isCollidingBehind gs r =
  let currentPos = objPos (robotBase r)
      currentDir = objDir (robotBase r)
      -- Predice una posición ligeramente hacia atrás
      lookBehindDist = 50.0 -- Distancia a mirar hacia atrás
      (vx, vy) = (lookBehindDist * cos (currentDir + pi), lookBehindDist * sin (currentDir + pi)) -- + pi para retroceder
      predictedPos = G.addVec currentPos (vx, vy)

      -- Crea un objeto hipotético en la posición futura
      predictedRobotBase = (robotBase r) { objPos = predictedPos, objShape = (G.moveTo (objShape (robotBase r)) predictedPos) }
      
      -- Filtra los obstáculos no sólidos (como OilSpillObstacle)
      solidObstacles = filter (\obs -> obstacleType obs /= OilSpillObstacle) (obstacles gs)
      
      -- Verifica colisión con paredes y obstáculos sólidos
      wallsCollision = any (checkCollision (objShape predictedRobotBase)) (mapWalls (gameMap gs))
      obstaclesCollision = any (\obs -> checkCollision (objShape predictedRobotBase) (obstacleShape obs)) solidObstacles
  in wallsCollision || obstaclesCollision
