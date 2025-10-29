{-# LANGUAGE RecordWildCards #-}

module GamePhysics where

import GameState
import Entities
import ExplosionTypes (Explosion(..), ExplosionType(Impact, Death))
import GameConstants (robotWidth, robotHeight, projectileWidth, projectileHeight, screenWidth, screenHeight, robotCollisionRadius, robotCollisionDamage)
import CollisionSAT
import Physics (updatePosition)
import Data.Maybe (mapMaybe)
import GameUtils (findById, findProjById, replaceRobot, clampPosition)
import Graphics.Gloss
import Geometry (Point, Vector, Distance, addVec, subVec, vectorXScalar, angleToTarget, distanceBetween, createRectanglePolygon)
import Data.List (nub)

-- | Calcula la magnitud de un vector.
magnitude :: Geometry.Vector -> Float
magnitude (x, y) = sqrt (x**2 + y**2)

-- | Normaliza un vector (lo convierte en un vector unitario).
normalize :: Geometry.Vector -> Geometry.Vector
normalize v@(x, y) = 
  let mag = magnitude v
  in if mag == 0 then (0, 0) else (x / mag, y / mag)

-- | Incrementa el tiempo total del juego.
updateGameTime :: Float -> GameState -> GameState
updateGameTime dt gs = gs { time = time gs + dt }

-- | Actualiza la física de todos los objetos del juego.
--   - Mueve robots y proyectiles
--   - Reduce cooldowns
--   - Elimina proyectiles y explosiones expiradas
updatePhysics :: Float -> GameState -> GameState
updatePhysics dt gs =
  let movedR  = map (updateRobotPosition dt) (robots gs)
      movedP  = map (updateProjectilePosition dt) (projectiles gs)
      coolR   = map (updateRobotCooldown dt) movedR
      aliveP  = mapMaybe (updateProjectileLifetime dt) movedP
      aliveE  = mapMaybe updateExplosionFrame (explosions gs)
  in updateGameTime dt gs { robots = coolR, projectiles = aliveP, explosions = aliveE }

-- | Mueve un robot según su velocidad y limita su posición dentro de la pantalla.
updateRobotPosition :: Float -> Robot -> Robot
updateRobotPosition dt r =
  if robotState r == Destroyed
    then r { robotBase = (robotBase r) { objVel = (0,0) } } -- Inmóvil si está destruido
    else
      let base0 = robotBase r
          base1 = updatePosition base0 dt
          clamped = clampPosition (objPos base1)  -- evita que salga de la pantalla
          base2 = base1 { objPos = clamped }
          shape = createRectanglePolygon (objPos base2) robotWidth robotHeight (objDir base2)
      in r { robotBase = base2 { objShape = shape } }

-- | Actualiza posición y forma del proyectil.
updateProjectilePosition :: Float -> Projectile -> Projectile
updateProjectilePosition dt p =
  let b1 = updatePosition (projBase p) dt
      shape = createRectanglePolygon (objPos b1) projectileWidth projectileHeight (objDir b1)
  in p { projBase = b1 { objShape = shape } }

-- | Reduce el cooldown de disparo de la torreta.
updateRobotCooldown :: Float -> Robot -> Robot
updateRobotCooldown dt r =
  let t = robotTurret r
  in r { robotTurret = t { turretCooldown = max 0 (turretCooldown t - dt) } }

-- | Disminuye la vida restante de un proyectil, eliminándolo si caduca.
updateProjectileLifetime :: Float -> Projectile -> Maybe Projectile
updateProjectileLifetime dt p =
  let t = projLifetime p - dt
  in if t <= 0 then Nothing else Just p { projLifetime = t }

-- | Disminuye la duración de una explosión visual.
updateExplosionFrame :: Explosion -> Maybe Explosion
updateExplosionFrame e =
  let nextFrame = expFrameIdx e + 1
  in if nextFrame >= 5 then Nothing else Just e { expFrameIdx = nextFrame }

-- | Calcula las colisiones y aplica sus efectos.
handleCollisions :: GameState -> GameState
handleCollisions gs =
  let -- Detección y resolución de colisiones entre robots
      gs' = detectAndResolveRobotCollisions gs
      -- Colisiones de proyectiles y otras entidades (usando CollisionSAT)
      activeRobots = filter (\r -> robotState r == Alive) (robots gs')
      objs = map robotBase activeRobots ++ map projBase (projectiles gs')
  in applyCollisionEffects (CollisionSAT.checkCollisions objs) gs'

-- | Detección y resolución de colisiones entre robots.
--   Aplica daño y los separa físicamente.
detectAndResolveRobotCollisions :: GameState -> GameState
detectAndResolveRobotCollisions gs =
  let currentRobots = filter (\r -> robotState r == Alive) (robots gs) -- Solo robots vivos colisionan activamente
      -- Genera todos los pares únicos de robots.
      robotPairs = [(r1, r2) | r1 <- currentRobots, r2 <- currentRobots, objId (robotBase r1) < objId (robotBase r2)]

      -- Función auxiliar para aplicar colisión a un par de robots.
      applyRobotCollision :: GameState -> (Robot, Robot) -> GameState
      applyRobotCollision accGs (rA, rB) =
        let posA = objPos (robotBase rA)
            posB = objPos (robotBase rB)
            dist = distanceBetween posA posB
            minDist = robotCollisionRadius * 2 -- Suma de radios
        in if dist < minDist
              then
                -- Colisión detectada: aplicar daño y separar.
                let -- Calcular vector de separación y magnitud de superposición
                    overlap    = minDist - dist
                    direction  = normalize (subVec posA posB) -- Vector de B a A
                    separation = vectorXScalar (overlap / 2) direction

                    -- Aplicar separación
                    newPosA    = addVec posA separation
                    newPosB    = subVec posB separation

                    -- Actualizar robots con nuevas posiciones
                    rA' = rA { robotBase = (robotBase rA) { objPos = newPosA, objShape = createRectanglePolygon newPosA robotWidth robotHeight (objDir (robotBase rA)) } }
                    rB' = rB { robotBase = (robotBase rB) { objPos = newPosB, objShape = createRectanglePolygon newPosB robotWidth robotHeight (objDir (robotBase rB)) } }

                    -- Aplicar daño
                    rA'' = rA' { robotHealth = max 0 (robotHealth rA' - robotCollisionDamage) }
                    rB'' = rB { robotHealth = max 0 (robotHealth rB' - robotCollisionDamage) }

                    -- Comprobar si los robots han sido destruidos y actualizar su estado
                    rA_final = if robotHealth rA'' <= 0 then rA'' { robotState = Destroyed, robotBase = (robotBase rA'') { objVel = (0,0) } } else rA''
                    rB_final = if robotHealth rB'' <= 0 then rB'' { robotState = Destroyed, robotBase = (robotBase rB'') { objVel = (0,0) } } else rB''

                    -- Actualizar GameState con los robots modificados
                    tempRobots1 = replaceRobot rA_final (robots accGs)
                    finalRobots = replaceRobot rB_final tempRobots1

                in accGs { robots = finalRobots }
              else accGs -- No hay colisión, no hacer nada

  in foldl applyRobotCollision gs (filter (\(rA,rB) -> robotState rA == Alive && robotState rB == Alive) robotPairs)

-- | Aplica los efectos producidos por todas las colisiones detectadas.
applyCollisionEffects :: [CollisionEvent] -> GameState -> GameState
applyCollisionEffects events gs = foldl applyEffect gs events

-- | Aplica el efecto individual de una colisión (robot vs bala o robot vs robot).
applyEffect :: GameState -> CollisionEvent -> GameState
applyEffect gs (RobotProjectile rid pid) =
  let mr = findById rid (robots gs)
      mp = findProjById pid (projectiles gs)
  in case (mr, mp) of
       (Just r, Just p)
         | projOwner p == objId (robotBase r) -> gs -- Ignora balas del mismo robot
         | otherwise ->
             let damage  = projDamage p
                 hp'     = robotHealth r - damage
                 impactBoom = Explosion (objPos (projBase p)) 0 Impact -- Animación de impacto en la posición del proyectil
                 
                 (robotsAfterDamage, deathExplosions) = if hp' <= 0
                                                          then (replaceRobot (r { robotHealth = hp', robotState = Destroyed }) (robots gs),
                                                                [Explosion (objPos (robotBase r)) 0 Death]) -- Marcar como destruido y crear explosión
                                                          else (replaceRobot (r { robotHealth = hp' }) (robots gs),
                                                                [])

                 projs'  = filter (\q -> objId (projBase q) /= pid) (projectiles gs)
                 
                 allExplosions = impactBoom : deathExplosions ++ explosions gs
             in gs { robots = robotsAfterDamage, projectiles = projs', explosions = allExplosions }
       _ -> gs
applyEffect gs (RobotRobot _ _) = gs
