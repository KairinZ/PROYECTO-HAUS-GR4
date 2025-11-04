{-# LANGUAGE RecordWildCards #-}

module GamePhysics where

import GameState
import Entities
import ExplosionTypes (Explosion(..), ExplosionType(Impact, Death))
import GameConstants (robotWidth, robotHeight, projectileWidth, projectileHeight, screenWidth, screenHeight, robotCollisionRadius, robotCollisionDamage, barricadeCollisionDamage, explosiveBarrelRadius, explosiveBarrelDamage, explosiveBarrelCountdown, explosiveBarrelTriggerRadius)
import CollisionSAT
import Physics (updatePosition)
import Data.Maybe (mapMaybe)
import GameUtils (findById, findProjById, replaceRobot, clampPosition)
import Memory (get, set, MemoryValue(..))
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
  let movedR   = map (updateRobotPosition dt gs) (robots gs)
      stunnedR = map (updateRobotStun dt) movedR
      cooledR  = map (updateRobotCooldown dt) stunnedR
      movedP   = map (updateProjectilePosition dt) (projectiles gs)
      aliveP   = mapMaybe (updateProjectileLifetime dt) movedP
      aliveE   = mapMaybe updateExplosionFrame (explosions gs)
      gs'      = updateGameTime dt gs { robots = cooledR, projectiles = aliveP, explosions = aliveE }
  in processExplosiveBarrels dt gs'

-- | Gestiona barriles explosivos: dispara cuenta atrás al colisionar, reduce contador y detona aplicando daño en área.
processExplosiveBarrels :: Float -> GameState -> GameState
processExplosiveBarrels dt gs =
  let aliveRobots = filter (\r -> robotState r == Alive) (robots gs)

      stepObstacle :: Obstacle -> (Maybe Obstacle, [Explosion], [Robot])
      stepObstacle obs = case obstacleType obs of
        ExplosiveBarrel ->
          let pos = obstaclePos obs
              -- ¿Activar cuenta atrás por primera colisión?
              isColliding = any (\r -> checkCollision (objShape (robotBase r)) (obstacleShape obs)) aliveRobots
              isNear      = any (\r -> distanceBetween (objPos (robotBase r)) pos <= explosiveBarrelTriggerRadius) aliveRobots
              triggered = case obstacleCountdown obs of
                            Nothing -> isColliding || isNear
                            _       -> False
              countdown0 = case obstacleCountdown obs of
                             Nothing | triggered -> Just explosiveBarrelCountdown
                             other               -> other
              -- Avanzar el temporizador si está activo
              (keepObstacle, explosionsOut, robotsAfterDamage) = case countdown0 of
                Just t ->
                  let t' = t - dt in
                  if t' <= 0
                    then
                      -- Detona: eliminar obstáculo, dañar robots en radio y crear explosión visual
                      let affected = [ r | r <- aliveRobots
                                         , distanceBetween (objPos (robotBase r)) pos <= explosiveBarrelRadius ]
                          applyDmg r =
                            let hp' = max 0 (robotHealth r - explosiveBarrelDamage)
                                r'  = if hp' <= 0 then r { robotHealth = hp', robotState = Destroyed, robotBase = (robotBase r) { objVel = (0,0) } }
                                                   else r { robotHealth = hp' }
                            in r'
                          damaged = map applyDmg affected
                          boom = Explosion pos 0 Impact
                      in (Nothing, [boom], damaged)
                    else (Just obs { obstacleCountdown = Just t' }, [], [])
                Nothing -> (Just obs, [], [])
          in (keepObstacle, explosionsOut, robotsAfterDamage)
        _ -> (Just obs, [], [])

      -- Procesa todos los obstáculos y acumula resultados
      (keptObsMaybes, newExplosions, dmgRobots) = unzip3 (map stepObstacle (obstacles gs))
      keptObs = [o | Just o <- keptObsMaybes]

      -- Mezcla robots dañados con los no afectados
      dmgIds = [ objId (robotBase r) | r <- concat dmgRobots ]
      mergeRobot r = case lookup (objId (robotBase r)) [(objId (robotBase d), d) | d <- concat dmgRobots] of
                       Just d  -> d
                       Nothing -> r
      robots' = map mergeRobot (robots gs)

  in gs { obstacles = keptObs, explosions = explosions gs ++ concat newExplosions, robots = robots' }

-- | Mueve un robot según su velocidad y limita su posición dentro de la pantalla.
--   Si el robot está stuneado, su movimiento se anula y solo gira.
updateRobotPosition :: Float -> GameState -> Robot -> Robot
updateRobotPosition dt gs r
  | robotState r == Destroyed = r { robotBase = (robotBase r) { objVel = (0,0) } }
  | robotStunTime r > 0.0     = r
  | otherwise = 
      let base0   = robotBase r
          base1   = updatePosition base0 dt
          newPos  = objPos base1
          newShape= createRectanglePolygon newPos robotWidth robotHeight (objDir base1)
          solid   = filter (\o -> obstacleType o /= OilSpillObstacle) (obstacles gs)
          hitSolid= any (\o -> checkCollision newShape (obstacleShape o)) solid
          hitOil  = any (\o -> obstacleType o == OilSpillObstacle && checkCollision newShape (obstacleShape o)) (obstacles gs)
          r'      = if hitOil && robotStunTime r <= 0 && robotStunImmunity r <= 0 then r { robotStunTime = 2.0 } else r
          clamped = clampPosition newPos
          finalB  = if hitSolid then base0 else base1 { objPos = clamped, objShape = newShape }
      in r' { robotBase = finalB }

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
      gs'' = applyCollisionEffects (CollisionSAT.checkCollisions objs) gs'
      -- Daño por colisiones con barricadas
      gs''' = applyBarricadeCollisions gs''
  in detectAndResolveProjectileObstacleCollisions gs'''

-- | Aplica daño fijo a robots que colisionen con barricadas.
--   Usa memoria por robot-obstáculo para registrar el último impacto y evitar aplicar daño en todos los frames.
applyBarricadeCollisions :: GameState -> GameState
applyBarricadeCollisions gs =
  let aliveRobots = filter (\r -> robotState r == Alive) (robots gs)
      barricades  = filter (\o -> obstacleType o == BarricadeObstacle) (obstacles gs)

      applyForRobot :: Robot -> Robot
      applyForRobot r = foldl applyIfHit r barricades
        where
          applyIfHit accR obs =
            if checkCollision (objShape (robotBase accR)) (obstacleShape obs)
              then
                let key = "barricade_last_hit_" ++ show (obstacleId obs)
                    mVal = get key (robotMemory accR)
                    canHit = case mVal of
                               Just (MemFloat t) -> (time gs - t) > 0.4
                               _                 -> True
                in if canHit
                     then
                       let hp' = max 0 (robotHealth accR - barricadeCollisionDamage)
                           mem' = set key (MemFloat (time gs)) (robotMemory accR)
                           accR' = accR { robotHealth = hp', robotMemory = mem' }
                       in if hp' <= 0 then accR' { robotState = Destroyed, robotBase = (robotBase accR') { objVel = (0,0) } } else accR'
                     else accR
              else accR

      updatedRobots = map applyForRobot aliveRobots ++ filter (\r -> robotState r == Destroyed) (robots gs)
  in gs { robots = updatedRobots }

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

-- | Detecta y resuelve colisiones entre proyectiles y obstáculos.
--   Los proyectiles que colisionan con obstáculos son eliminados.
detectAndResolveProjectileObstacleCollisions :: GameState -> GameState
detectAndResolveProjectileObstacleCollisions gs =
  let projs = projectiles gs
      obss = obstacles gs
      
      -- Verifica cada proyectil contra cada obstáculo
      collidingPairs = [ (objId (projBase p), obstacleId obs)
                       | p <- projs
                       , obs <- obss
                       , checkCollision (objShape (projBase p)) (obstacleShape obs)
                       ]
      collidingProjectileIds = [ pid | (pid, _) <- collidingPairs ]
      hitObstacleIds = [ oid | (_, oid) <- collidingPairs ]
      
      -- Elimina los proyectiles que colisionaron con obstáculos
      filteredProjectiles = filter (\p -> objId (projBase p) `notElem` collidingProjectileIds) projs
      
      -- Activa la cuenta atrás en barriles explosivos golpeados por proyectil
      updatedObstacles = map (\obs ->
                                if obstacleId obs `elem` hitObstacleIds && obstacleType obs == ExplosiveBarrel
                                   then case obstacleCountdown obs of
                                          Nothing -> obs { obstacleCountdown = Just explosiveBarrelCountdown }
                                          justT  -> obs { obstacleCountdown = justT }
                                   else obs)
                           obss
      
  in gs { projectiles = filteredProjectiles, obstacles = updatedObstacles }

-- | Actualiza el tiempo de stun de un robot y aplica efectos visuales/físicos.
updateRobotStun :: Float -> Robot -> Robot
updateRobotStun dt r
  | robotState r /= Alive = r
  | robotStunTime r > 0.0 = 
      let newStun = max 0 (robotStunTime r - dt)
          newImmunity = max 0 (robotStunImmunity r - dt) -- Always decrement immunity
          base0   = robotBase r
          base'   = base0 { objVel = (0,0), objDir = objDir base0 + 6*dt }
          turret0 = robotTurret r
          turret' = turret0 { turretDir = turretDir turret0 + 6*dt } -- Rotate turret as well
          r_after_stun_time = r { robotStunTime = newStun, robotStunImmunity = newImmunity, robotBase = base', robotTurret = turret'}
      in if newStun == 0.0 && robotStunTime r > 0.0 -- If stun just ended
            then r_after_stun_time { robotStunImmunity = 3.0 } -- Apply immunity
            else r_after_stun_time
  | otherwise = r { robotStunImmunity = max 0 (robotStunImmunity r - dt)
                    , robotStunTime = max 0 (robotStunTime r - dt) } -- Still decrement stun time, even if 0, for consistency
