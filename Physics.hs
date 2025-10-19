module Physics
  ( updatePosition
  , updateVelocity
  , updateRobotVelocity
  , detectedAgent
  ) where

import Geometry
import Entities

-- Actualiza posición de un GameObject
updatePosition :: GameObject -> Float -> GameObject
updatePosition obj dt =
  obj { objPos = addVec (objPos obj) (vectorXScalar dt (objVel obj)) }


-- Actualiza velocidad según una acción
updateVelocity :: Float -> Angle -> Velocity
updateVelocity speed dir = (speed * cos dir, speed * sin dir)

-- Aplica una nueva velocidad a un robot
updateRobotVelocity :: Robot -> Velocity -> Robot
updateRobotVelocity r newV =
  r { robotBase = (robotBase r) { objVel = newV } }

-- Detectar si un robot ve a otro
detectedAgent :: Robot -> Robot -> Bool
detectedAgent r1 r2 =
  let p1 = objPos (robotBase r1)
      p2 = objPos (robotBase r2)
      range = robotRange r1
  in distanceBetween p1 p2 <= range && objId (robotBase r1) /= objId (robotBase r2)
