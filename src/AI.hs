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
import Geometry
import Physics
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
type BotBrain = GameState -> Robot -> IO [BotAction]


-- ===========================================================
-- 2. CEREBRO "HUNTER" (CAZADOR)
-- ===========================================================

-- | El bot 'Hunter' busca al enemigo más cercano, se acerca a él
--   y dispara cuando lo tiene en la mira.
hunterBot :: BotBrain
hunterBot gs self = return $ hunterLogic gs self

-- | Lógica pura del bot cazador.
--   Si no hay enemigos, gira lentamente para buscarlos.
--   Si hay uno, se mueve hacia él y dispara si tiene la torreta alineada.
hunterLogic :: GameState -> Robot -> [BotAction]
hunterLogic gs self =
  case findClosestEnemy gs self of
    Nothing ->
      -- No hay enemigos visibles: rotar para explorar.
      [Rotate 0.5]

    Just enemy ->
      let selfPos  = objPos (robotBase self)
          enemyPos = objPos (robotBase enemy)
          dist     = distanceBetween selfPos enemyPos
          
          actions =
            -- Siempre apunta al enemigo detectado.
            [ Aim enemyPos ] ++
            -- Si está lejos, acércate.
            (if dist > 200 then [MoveSpeed 100] else []) ++
            -- Si la torreta está alineada y puede disparar, ¡fuego!
            (if turretIsAimed self enemyPos
                && turretCooldown (robotTurret self) <= 0
               then [Shoot]
               else [])
      in actions


-- ===========================================================
-- 3. CEREBRO "EVASIVE" (EVASIVO)
-- ===========================================================

-- | El bot evasivo intenta mantener la distancia y moverse
--   de forma impredecible para evitar disparos enemigos.
--   Usa aleatoriedad en sus ángulos y tiempos de decisión.
evasiveBot :: BotBrain
evasiveBot gs self = do
  -- Genera valores aleatorios:
  randAngle <- randomRIO (-pi/2, pi/2) -- Ángulo aleatorio para variar dirección.
  randWait  <- randomRIO (2.0, 5.0)    -- Tiempo hasta el siguiente cambio de comportamiento.
  return $ evasiveLogic gs self randAngle (toFloat randWait)

-- | Conversión auxiliar de Double a Float.
toFloat :: Double -> Float
toFloat = fromRational . toRational

-- | Lógica pura del bot evasivo:
--   - Si no hay enemigos: se mueve aleatoriamente un tiempo.
--   - Si hay enemigos: se mantiene en movimiento, esquiva y dispara.
evasiveLogic :: GameState -> Robot -> Float -> Float -> [BotAction]
evasiveLogic gs self randomAngle nextDecisionTime =
  case findClosestEnemy gs self of
    ----------------------------------------------------------
    -- 1. SIN ENEMIGOS DETECTADOS
    ----------------------------------------------------------
    Nothing ->
      case get "nextMoveTime" (robotMemory self) of
        -- Aún dentro del tiempo de movimiento aleatorio → seguir avanzando.
        Just (MemFloat t) | time gs < t -> [MoveSpeed 80]

        -- Fin del ciclo → girar y programar nuevo tiempo.
        _ -> [ Rotate 1.0
             , SetMemory "nextMoveTime" (MemFloat (time gs + nextDecisionTime))
             ]

    ----------------------------------------------------------
    -- 2. ENEMIGO DETECTADO
    ----------------------------------------------------------
    Just enemy ->
      let selfPos  = objPos (robotBase self)
          enemyPos = objPos (robotBase enemy)
          dist     = distanceBetween selfPos enemyPos
          
          ----------------------------------------------------
          -- MOVIMIENTO
          ----------------------------------------------------
          moveActions =
            if dist < 250
              -- Si el enemigo está demasiado cerca → retrocede.
              then [MoveSpeed (-100)]
              else
                -- Mantiene distancia moviéndose lateralmente.
                let angleFromEnemy = angleToTarget enemyPos selfPos
                    evadeAngle     = angleFromEnemy + randomAngle
                    currentAngle   = objDir (robotBase self)
                    rotationToDo   = evadeAngle - currentAngle
                in [ Rotate rotationToDo
                   , MoveSpeed 80
                   ]

          ----------------------------------------------------
          -- ATAQUE
          ----------------------------------------------------
          attackActions =
            [ Aim enemyPos ] ++
            (if turretIsAimed self enemyPos
                && turretCooldown (robotTurret self) <= 0
               then [Shoot]
               else [])

      -- Devuelve la combinación de ataque + movimiento.
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


-- | Busca y devuelve el enemigo más cercano al robot dado.
--   Solo se consideran enemigos vivos (con salud > 0)
--   y que no sean el propio robot.
findClosestEnemy :: GameState -> Robot -> Maybe Robot
findClosestEnemy gs self =
  let selfId   = objId (robotBase self)
      
      -- Filtra todos los robots enemigos (distinto ID y vivos)
      enemies = [ r
                | r <- robots gs
                , objId (robotBase r) /= selfId
                , robotHealth r > 0
                ]
      
      selfPos = objPos (robotBase self)
      
      -- Ordena los enemigos por distancia creciente respecto al robot actual
      sortedEnemies = sortBy
                        (comparing (distanceBetween selfPos . objPos . robotBase))
                        enemies
  in listToMaybe sortedEnemies  -- Devuelve el más cercano (o Nothing si no hay)
