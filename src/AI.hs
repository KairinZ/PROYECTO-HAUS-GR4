{-# LANGUAGE RecordWildCards #-}

-- ===========================================================
-- AI.hs
-- ===========================================================
-- Módulo que define la "inteligencia artificial" de los robots.
-- Aquí se establecen las acciones posibles (BotAction)
-- y los distintos "cerebros" o estrategias de comportamiento (BotBrain).
-- ===========================================================
module AI
  ( BotAction(..)
  , BotBrain
  , hunterBot
  , evasiveBot
  ) where

import System.Random (randomRIO)
import Data.Maybe (listToMaybe)
import Data.List (sortBy)
import Data.Ord (comparing)

import Entities
import GameState
import Geometry (Point, Vector, dot, subVec, addVec, vectorXScalar, distanceBetween, angleToTarget, Angle, moveTo)
import Physics (updatePosition, updateVelocity, updateRobotVelocity, detectedAgent, isCollidingAhead, isCollidingBehind)
import Memory

-- ===========================================================
-- 1. ACCIONES DEL BOT
-- ===========================================================

-- | 'BotAction' define el conjunto de acciones atómicas que
--   un robot puede ejecutar en un tick (un paso de simulación).
--   Cada IA decide una lista de estas acciones por cada actualización.
data BotAction
  = MoveSpeed Float          -- ^ Mover el tanque hacia adelante/atrás con una velocidad (px/s).
  | Rotate Float             -- ^ Rotar el chasis del tanque (en radianes por segundo).
  | RotateTurret Float       -- ^ Rotar la torreta independientemente del chasis.
  | Aim Point                -- ^ Apuntar la torreta hacia un punto específico.
  | Shoot                    -- ^ Disparar un proyectil (si no está en cooldown).
  | SetMemory String MemoryValue -- ^ Guardar un valor en la memoria interna del robot.
  | Idle                     -- ^ No realizar ninguna acción este tick.
  deriving (Show, Eq)

-- | Un 'BotBrain' representa la lógica de decisión de un robot.
--   Dado el estado global del juego y el propio robot,
--   devuelve una lista de acciones a ejecutar en el siguiente tick.
--   Se usa IO para permitir decisiones aleatorias (por ejemplo, el bot evasivo).
type BotBrain = GameState -> Robot -> Float -> IO [BotAction]

-- | Helpers para acceder a la memoria del robot de forma segura.
getMemFloat :: String -> Memory -> Float -> Float
getMemFloat key mem defaultValue = case get key mem of
  Just (MemFloat f) -> f
  _                 -> defaultValue

getMemPoint :: String -> Memory -> Point -> Point
getMemPoint key mem defaultValue = case get key mem of
  Just (MemPoint p) -> p
  _                 -> defaultValue

-- ===========================================================
-- 2. CEREBRO "HUNTER" (CAZADOR)
-- ===========================================================

-- | El bot 'Hunter' busca al enemigo más cercano, se acerca a él
--   y dispara cuando lo tiene en la mira.
hunterBot :: GameState -> Robot -> Float -> IO [BotAction]
hunterBot gs self dt = do
  if robotState self == Destroyed
    then return []
    else do
      -- Genera valores aleatorios para el movimiento evasivo.
      evasiveRandAngle <- randomRIO (-0.3, 0.3) -- Pequeño ángulo para zig-zag
      scanRandAngle    <- randomRIO (-1.0, 1.0)  -- Ángulo para rotación de escaneo
      targetRandAngle  <- randomRIO (-pi, pi)    -- Nuevo ángulo aleatorio para patrullaje
      
      let Hunter { hunterDetectionRange = detectionRange } = robotAIType self
          isCollidingAhead' = isCollidingAhead gs self -- Mover esta línea aquí

      -- Decide la lógica en base a si hay un enemigo.
      case findClosestEnemy gs self detectionRange of -- Pass detectionRange
        Just enemy -> return $ aggressiveAttackLogic gs self enemy evasiveRandAngle isCollidingAhead' dt
        Nothing    -> return $ searchAndScanLogic gs self dt scanRandAngle targetRandAngle isCollidingAhead'

-- | Lógica del bot agresivo cuando hay un enemigo detectado.
aggressiveAttackLogic :: GameState -> Robot -> Robot -> Angle -> Bool -> Float -> [BotAction]
aggressiveAttackLogic gs self enemy evasiveRandAngle isCollidingAhead' dt =
  let selfPos        = objPos (robotBase self)
      enemyPos       = objPos (robotBase enemy)
      dist           = distanceBetween selfPos enemyPos -- Añadir la distancia aquí
      angleToEnemy   = angleToTarget selfPos enemyPos
      currentBodyAngle = objDir (robotBase self)

      -- Constantes para el comportamiento agresivo
      aggressiveMoveSpeed = 70.0
      hunterMinDistance   = 150.0 -- Nueva constante para la distancia mínima
      strafingMagnitude   = 0.1   -- Pequeño valor para movimiento lateral

      -- Lógica de movimiento y rotación del chasis
      (moveAction, bodyRotateAction) =
        if isCollidingAhead'
          then ([MoveSpeed (-50.0)], [Rotate (normalizeAngle (currentBodyAngle + pi/2 * signum evasiveRandAngle))]) -- Gira y retrocede si hay pared
          else if dist > hunterMinDistance
            then -- Avanzar hacia el enemigo
              let targetBodyAngle = normalizeAngle (angleToEnemy + evasiveRandAngle)
                  rotation = normalizeAngle (targetBodyAngle - currentBodyAngle)
              in ([MoveSpeed aggressiveMoveSpeed], [Rotate rotation])
            else -- Mantener distancia o moverse lateralmente
              let strafeAngle = normalizeAngle (currentBodyAngle + evasiveRandAngle + (if evasiveRandAngle > 0 then pi/2 else -pi/2)) -- Moverse lateralmente
                  rotation = normalizeAngle (strafeAngle - currentBodyAngle)
              in ([MoveSpeed aggressiveMoveSpeed], [Rotate rotation])

      -- Acciones de disparo
      attackActions =
        [ Aim enemyPos ] ++
        (if turretIsAimed self enemyPos && turretCooldown (robotTurret self) <= 0
         then [Shoot]
         else [])
  in bodyRotateAction ++ moveAction ++ attackActions

-- | Lógica del bot cuando no hay enemigos detectados: moverse y escanear.
searchAndScanLogic :: GameState -> Robot -> Float -> Angle -> Angle -> Bool -> [BotAction]
searchAndScanLogic gs self dt scanRandAngle targetRandAngle isCollidingAhead' =
  let currentBodyAngle = objDir (robotBase self)
      aggressiveMoveSpeed = 120.0

      -- Recuperar el tiempo del próximo cambio de dirección y el ángulo de memoria
      memNextChangeTime = getMemFloat "nextTurnTime" (robotMemory self) 0.0
      memTargetScanAngle = getMemFloat "targetScanAngle" (robotMemory self) currentBodyAngle

      -- Decidir si cambiar de dirección objetivo
      (newTargetScanAngle, newNextChangeTime, moveSpeed) =
        if isCollidingAhead'
          then (normalizeAngle (currentBodyAngle + pi), time gs + 0.5, MoveSpeed (-50.0)) -- Gira 180 y retrocede rápido
          else if time gs >= memNextChangeTime
                 then (targetRandAngle, time gs + (5.0 + scanRandAngle * 3.0), MoveSpeed aggressiveMoveSpeed) -- Nueva dirección objetivo y tiempo
                 else (memTargetScanAngle, memNextChangeTime, MoveSpeed aggressiveMoveSpeed)

      -- Calcular la rotación necesaria para alcanzar el ángulo objetivo
      angleDiff = normalizeAngle (newTargetScanAngle - currentBodyAngle)
      rotation = signum angleDiff * min (abs angleDiff) (1.0 * dt)

  in  [SetMemory "nextTurnTime" (MemFloat newNextChangeTime),
       SetMemory "targetScanAngle" (MemFloat newTargetScanAngle)] ++
      [Rotate rotation] ++
      [moveSpeed] ++
      [Aim (addVec (objPos (robotBase self)) (100 * cos currentBodyAngle, 100 * sin currentBodyAngle))] -- Apunta el radar en la dirección del cuerpo

-- ===========================================================
-- 3. CEREBRO "EVASIVE" (EVASIVO)
-- ===========================================================

-- | El bot evasivo intenta mantener la distancia y moverse
--   de forma impredecible para evitar disparos enemigos.
--   Usa aleatoriedad en sus ángulos y tiempos de decisión.
evasiveBot :: BotBrain
evasiveBot gs self dt = do
  if robotState self == Destroyed
    then return []
    else do
      let Evasive { evasiveDetectionRange = detectionRange } = robotAIType self

      evasiveLogic gs self detectionRange 0.0 0.0 -- randomAngle and nextDecisionTime are now handled internally

-- | Conversión auxiliar de Double a Float.
toFloat :: Double -> Float
toFloat = fromRational . toRational

-- | Lógica pura del bot evasivo:
--   - Si no hay enemigos: se mueve aleatoriamente un tiempo.
--   - Si hay enemigos: se mantiene en movimiento, esquiva y dispara.
evasiveLogic :: GameState -> Robot -> Float -> Float -> Float -> IO [BotAction]
evasiveLogic gs self detectionRange randomAngle nextDecisionTime = do
  -- Genera nuevos valores aleatorios para el movimiento evasivo y el tiempo de cambio
  patrolRandAngle <- randomRIO (-pi, pi) -- Para el patrullaje
  strafeRandAngle <- randomRIO (-0.5, 0.5) -- Para el strafing en combate
  randTurnTime <- randomRIO (3.0, 7.0) -- Para el tiempo de cambio de dirección

  let selfPos        = objPos (robotBase self)
      currentBodyAngle = objDir (robotBase self)
      evasiveMinDistance = 250.0 -- Distancia crítica para evasión agresiva
      evasiveOptimalDistance = 350.0 -- Distancia óptima para lurking

      -- Recuperar memoria de patrullaje
      memNextChangeTime = getMemFloat "nextChangeTime" (robotMemory self) 0.0
      memTargetDirection = getMemFloat "targetDirection" (robotMemory self) currentBodyAngle

      -- Predicción de colisiones
      isCollidingAhead' = isCollidingAhead gs self
      isCollidingBehind' = isCollidingBehind gs self

  return $ case findClosestEnemy gs self detectionRange of
    ----------------------------------------------------------
    -- 1. SIN ENEMIGOS DETECTADOS (Lógica de Patrullaje)
    ----------------------------------------------------------
    Nothing ->
      let (newTargetDirection, newNextChangeTime, moveAction) =
            if isCollidingAhead'
              then (normalizeAngle (currentBodyAngle + pi/2 * signum patrolRandAngle),
                    time gs + 0.5, -- Reevalúa rápido
                    MoveSpeed (-50)) -- Retrocede
              else if time gs >= memNextChangeTime
                     then (normalizeAngle (currentBodyAngle + patrolRandAngle),
                           time gs + randTurnTime, -- Genera nueva dirección y tiempo
                           MoveSpeed 80.0)
                     else (memTargetDirection, memNextChangeTime, MoveSpeed 80.0)

          rotation = normalizeAngle (newTargetDirection - currentBodyAngle)
      in [ SetMemory "nextChangeTime" (MemFloat newNextChangeTime)
         , SetMemory "targetDirection" (MemFloat newTargetDirection)
         , Rotate rotation
         , moveAction
         ]

    ----------------------------------------------------------
    -- 2. ENEMIGO DETECTADO (Lógica de Combate y Evasión)
    ----------------------------------------------------------
    Just enemy ->
      let enemyPos = objPos (robotBase enemy)
          dist     = distanceBetween selfPos enemyPos
          angleToEnemy = angleToTarget selfPos enemyPos

          -- Acciones de ataque (apuntar y disparar)
          attackActions =
            [ Aim enemyPos ] ++
            (if turretIsAimed self enemyPos
                && turretCooldown (robotTurret self) <= 0
             then [Shoot]
             else [])

          -- Lógica de movimiento
          moveActions
            | dist < evasiveMinDistance = -- Rango Crítico: Evasión Agresiva
                if isCollidingBehind'
                  then [Rotate (currentBodyAngle + pi), MoveSpeed 50] -- Gira 180 y avanza para desatascar
                  else [MoveSpeed (-100)] -- Retrocede
            | dist >= evasiveMinDistance && dist <= detectionRange = -- Rango de "Lurking"
                let strafeAngle = normalizeAngle (angleToEnemy + pi/2 * signum strafeRandAngle)
                    rotation = normalizeAngle (strafeAngle - currentBodyAngle)
                in [Rotate rotation, MoveSpeed 80]
            | otherwise = [] -- No se detecta enemigo, pero no debería llegar aquí si Just enemy

      in attackActions ++ moveActions

-- ===========================================================
-- 4. FUNCIONES AUXILIARES DE IA
-- ===========================================================
-- Estas funciones son compartidas entre diferentes cerebros (IA)
-- y proporcionan utilidades comunes como detección de enemigos
-- o comprobación de si la torreta está alineada con el objetivo.
-- ===========================================================

-- | Determina si la torreta del robot está correctamente alineada
--   hacia un objetivo determinado dentro de un pequeño margen de error.
--   Esto evita exigir precisión exacta (lo que sería poco realista).
turretIsAimed :: Robot -> Point -> Bool
turretIsAimed self targetPos =
  let selfPos     = objPos (robotBase self)          -- Posición actual del robot
      targetAngle = angleToTarget selfPos targetPos  -- Ángulo hacia el objetivo
      turretAngle = turretDir (robotTurret self)     -- Ángulo actual de la torreta
      angleDiff   = abs (targetAngle - turretAngle)  -- Diferencia angular
  in angleDiff < 0.1  -- Margen de 0.1 radianes ≈ 5.7 grados

-- | Normaliza un ángulo a un rango entre -pi y pi.
normalizeAngle :: Angle -> Angle
normalizeAngle angle = atan2 (sin angle) (cos angle)


-- | Busca y devuelve el enemigo más cercano al robot dado DENTRO DE UN RANGO.
--   Solo se consideran enemigos vivos (con salud > 0)
--   y que no sean el propio robot.
findClosestEnemy :: GameState -> Robot -> Float -> Maybe Robot -- ¡Nuevo Float para el rango!
findClosestEnemy gs self detectionRange =
  let selfId   = objId (robotBase self)
      selfPos = objPos (robotBase self) -- Necesitamos selfPos para calcular la distancia

      -- Filtra todos los robots enemigos (distinto ID, vivos y DENTRO DEL RANGO)
      enemies = [ r
                | r <- robots gs
                , objId (robotBase r) /= selfId
                , robotHealth r > 0
                , let dist = distanceBetween selfPos (objPos (robotBase r))
                , dist <= detectionRange -- Filtro por rango
                ]

      -- Ordena los enemigos por distancia creciente respecto al robot actual
      sortedEnemies = sortBy
                        (comparing (distanceBetween selfPos . objPos . robotBase))
                        enemies
  in listToMaybe sortedEnemies  -- Devuelve el más cercano (o Nothing si no hay)
