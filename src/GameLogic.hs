{-# LANGUAGE RecordWildCards #-}

module GameLogic where

import GameState (GameState(..), GameWorld(..), emptyGameState, initialWorld, countActiveRobots, createInitialObstacles)
import Entities
import GameTypes (GameConfig(..), GamePhase(..), BotConfig(..))
import GameConstants
import qualified AI as AI
import Memory
import qualified Geometry as G (Point, Angle, createRectanglePolygon, angleToTarget, addVec)
import System.IO.Unsafe (unsafePerformIO)
import Data.Maybe (mapMaybe)
import Physics (updateVelocity)
import CollisionSAT (CollisionEvent)
import Graphics.Gloss.Interface.Pure.Game (Event)
import GamePhysics (updatePhysics, handleCollisions)
import GameUtils (findById, replaceRobot)
import Data.List (find)
import System.Random (randomRIO)
import Assets (Assets)

-- | Crea un nuevo 'GameWorld' a partir de la configuración seleccionada.
--   Inicializa el 'GameState' con los robots y cambia a la fase Playing.
startGameFromConfig :: Assets -> GameConfig -> GameWorld
startGameFromConfig gameAssets conf =
  let newGS = unsafePerformIO $ createInitialGameStateFromConfigIO gameAssets conf
  in initialWorld
       { phase     = Playing
       , gameState = newGS
       , config    = conf
       , nextId    = numRobots conf + 1
       }

-- | Construye el 'GameState' inicial de la partida con posiciones aleatorias.
createInitialGameStateFromConfigIO :: Assets -> GameConfig -> IO GameState
createInitialGameStateFromConfigIO gameAssets GameConfig{..} =
  do
    let robotDiagonalHalf = sqrt (robotWidth**2 + robotHeight**2) / 2
        minX = - (fromIntegral screenWidth  / 2) + robotDiagonalHalf
        maxX =   (fromIntegral screenWidth  / 2) - robotDiagonalHalf
        minY = - (fromIntegral screenHeight / 2) + robotDiagonalHalf
        maxY =   (fromIntegral screenHeight / 2) - robotDiagonalHalf

    randomPositions <- sequence $ replicate numRobots (
      do
        x <- randomRIO (minX, maxX)
        y <- randomRIO (minY, maxY)
        angle <- randomRIO (0, 2*pi)
        return ((x, y), angle))

    let rs = zipWith3 (\(p,a) (BotConfig ai) i -> createRobot i p a ai)
                      randomPositions botConfigs [1..]
    return $ (emptyGameState gameAssets) { robots = rs, obstacles = createInitialObstacles }

-- | Posiciones y orientaciones iniciales de los robots en el mapa.
--   (soportan hasta 4 robots)
robotStartPositions :: [(G.Point, G.Angle)]
robotStartPositions =
  [ ((-200, 200),    -pi/4)
  , (( 200,-200),     3*pi/4)
  , ((-200,-200),     pi/4)
  , (( 200, 200),    -3*pi/4)
  ]

-- | Crea un robot completamente inicializado en la posición indicada.
createRobot :: Int -> G.Point -> G.Angle -> AIType -> Robot
createRobot i pos dir aiType =
  let base = GameObject
        { objId   = i
        , objPos  = pos
        , objDir  = dir
        , objVel  = (0,0)
        , objShape= G.createRectanglePolygon pos robotWidth robotHeight dir
        , objType = RobotType
        }
      turret = RobotTurret
        { turretDir       = dir
        , turretCooldown  = 0
        , turretMaxCooldown = 1.0
        }
  in Robot
      { robotBase      = base
      , robotHealth    = 100
      , robotMaxHealth = 100
      , robotTurret    = turret
      , robotMemory    = emptyMemory
      , robotAIType    = aiType
      , robotState     = Alive
      }

-- | Bucle principal de actualización cuando la partida está activa.
--   Se ejecuta en cada frame mientras queden al menos dos robots vivos.
runGameLogic :: Float -> GameWorld -> GameWorld
runGameLogic dt w@GameWorld{..} =
  let gs = gameState
  in if countActiveRobots (robots gs) <= 1
        then w  -- Si solo queda un robot, el juego se detiene.
        else
          let (actions, gs0) = getAIActions gs dt              -- 1️⃣ Obtener acciones de IA
              (gs1, newId)   = applyAllActions nextId dt actions gs0 -- 2️⃣ Aplicarlas
              gs2            = updatePhysics dt gs1         -- 3️⃣ Actualizar físicas
              gs3            = handleCollisions gs2         -- 4️⃣ Gestionar colisiones
          in w { gameState = gs3, nextId = newId }

-- | Manejador de eventos dentro de la partida (actualmente vacío).
handleGameEvents :: Event -> GameWorld -> GameWorld
handleGameEvents _ w = w

-- | Recoge las acciones de todos los robots vivos.
--   Devuelve una lista con sus IDs y acciones asociadas.
getAIActions :: GameState -> Float -> ([(Int, [AI.BotAction])], GameState)
getAIActions gs dt =
  let alive   = filter (\r -> robotHealth r > 0) (robots gs)
      -- Ejecuta las IAs de cada robot (Hunter o Evasive) mediante IO
      actions = unsafePerformIO $ mapM (\r -> getActionsForRobot gs r dt) alive
  in (actions, gs)

-- | Ejecuta el “cerebro” de un robot concreto según su tipo de IA.
getActionsForRobot :: GameState -> Robot -> Float -> IO (Int, [AI.BotAction])
getActionsForRobot gs r dt = do
  let brain = case robotAIType r of
                  (Hunter {})  -> \s -> AI.hunterBot gs s dt
                  (Evasive {}) -> \s -> AI.evasiveBot gs s dt
  acts <- brain r
  pure (objId (robotBase r), acts)

-- | Aplica todas las acciones de todos los robots al GameState.
--   Cada robot puede moverse, girar, disparar o actualizar su memoria.
--   Devuelve el nuevo estado del juego y el siguiente ID disponible.
applyAllActions :: Int -> Float -> [(Int, [AI.BotAction])] -> GameState -> (GameState, Int)
applyAllActions startId dt allActs gs =
  let step (currGs, currId) (rid, acts) =
        case findById rid (robots currGs) of
          Nothing -> (currGs, currId) -- Si el robot no existe, no hace nada
          Just r  ->
            -- Aplica todas las acciones de este robot una por una
            let (r', mProj, nextId') = foldl (applyAction dt) (r, Nothing, currId) acts
                rs' = replaceRobot r' (robots currGs)
                ps' = projectiles currGs ++ mapMaybe id [mProj] -- Agrega proyectil si disparó
            in (currGs { robots = rs', projectiles = ps' }, nextId')
  in foldl step (gs, startId) allActs

-- | Aplica una acción individual a un robot.
--   Algunas acciones devuelven un nuevo proyectil (como Shoot).
applyAction :: Float -> (Robot, Maybe Projectile, Int) -> AI.BotAction -> (Robot, Maybe Projectile, Int)
applyAction dt (r, mProj, currId) action = case action of

  -- Movimiento lineal
  AI.MoveSpeed speed ->
    let newVel = updateVelocity speed (objDir (robotBase r))
    in (r { robotBase = (robotBase r) { objVel = newVel } }, mProj, currId)

  -- Rotación del chasis
  AI.Rotate radians ->
    let maxTurnSpeed = 4.5
        capped = signum radians * min (abs radians) (maxTurnSpeed * dt)
        base   = robotBase r
    in (r { robotBase = base { objDir = objDir base + capped } }, mProj, currId)

  -- Rotación de la torreta
  AI.RotateTurret radians ->
    let t = robotTurret r
    in (r { robotTurret = t { turretDir = turretDir t + radians } }, mProj, currId)

  -- Apuntar torreta a una posición específica
  AI.Aim targetPos ->
    let t = robotTurret r
        selfPos = objPos (robotBase r)
        ang = G.angleToTarget selfPos targetPos
    in (r { robotTurret = t { turretDir = ang } }, mProj, currId)

  -- Disparo de proyectil (si la torreta no está en cooldown)
  AI.Shoot ->
    let t = robotTurret r
    in if turretCooldown t <= 0
       then
         let base = robotBase r
             ang  = turretDir t
             (offX, offY) = (turretWidth * cos ang, turretWidth * sin ang)
             startPos = G.addVec (objPos base) (offX, offY)
             proj = Projectile
               { projBase = GameObject
                   { objId   = currId
                   , objPos  = startPos
                   , objDir  = ang
                   , objVel  = updateVelocity projectileSpeed ang
                   , objShape= G.createRectanglePolygon startPos projectileWidth projectileHeight ang
                   , objType = ProjectileType
                   }
               , projOwner = objId base
               , projDamage = 10
               , projType   = Bullet
               , projLifetime = projectileLifetime
               , projMaxLifetime = projectileLifetime
               }
         in ( r { robotTurret = t { turretCooldown = turretMaxCooldown t } }
            , Just proj
            , currId + 1
            )
       else (r, Nothing, currId)

  -- Guarda un valor en la memoria del robot
  AI.SetMemory k v ->
    (r { robotMemory = set k v (robotMemory r) }, mProj, currId)

  -- No hace nada
  AI.Idle -> (r, mProj, currId)
