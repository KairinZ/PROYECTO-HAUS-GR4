{-# LANGUAGE RecordWildCards #-}

module GameLogic where

import GameState (GameState(..), GameWorld(..), emptyGameState, initialWorld, countActiveRobots, createInitialFixedObstacles, createObstacle)
import Entities (Robot(..), GameObject(..), ObjectType(RobotType, ProjectileType), Obstacle(..), ObstacleType(..), AIType(..), RobotTurret(..), Projectile(..), ProjectileType(Bullet), RobotState(Alive), GameMap(..), obstacleGameObjectShape)
import GameTypes (GameConfig(..), GamePhase(..), BotConfig(..))
import GameConstants (screenWidth, screenHeight, robotWidth, robotHeight, oilSpillWidth, oilSpillHeight, explosiveBarrelWidth, explosiveBarrelHeight, turretWidth, projectileSpeed, projectileWidth, projectileHeight, projectileLifetime)
import qualified AI as AI
import Memory
import qualified Geometry as G (Point, Angle, createRectanglePolygon, angleToTarget, addVec, Polygon, distanceBetween)
import System.IO.Unsafe (unsafePerformIO)
import Data.Maybe (mapMaybe)
import Physics (updateVelocity)
import CollisionSAT (CollisionEvent, checkCollision)
import Graphics.Gloss.Interface.Pure.Game (Event)
import GamePhysics (updatePhysics, handleCollisions)
import GameUtils (findById, replaceRobot)
import Data.List (find, foldl')
import System.Random (randomRIO)
import Assets (Assets)
import Control.Monad (foldM)
import Stats (incrementShotsFired, addTimeAlive)

-- | Crea un nuevo 'GameWorld' a partir de la configuración seleccionada.
--   Inicializa el 'GameState' con los robots y cambia a la fase Playing.
startGameFromConfig :: Assets -> GameConfig -> Int -> Int -> Float -> Int -> GameWorld -> GameWorld
startGameFromConfig gameAssets conf areaWidth areaHeight maxDuration numTourns w@GameWorld{..} =
  let areaWidthF = fromIntegral areaWidth
      areaHeightF = fromIntegral areaHeight
      newGS = unsafePerformIO $ createInitialGameStateFromConfigIO gameAssets conf areaWidthF areaHeightF
  in w
       { phase                 = Playing
       , gameState             = newGS
       , config                = conf
       , nextId                = numRobots conf + 1
       , tournamentCount       = 0  -- Inicializar contador de torneos en 0
       , tournamentAreaWidth   = areaWidth
       , tournamentAreaHeight  = areaHeight
       , maxTournamentDuration = maxDuration
       , numTournaments        = numTourns
       , completedTournaments  = []
       }

-- | Construye el 'GameState' inicial de la partida con posiciones aleatorias.
createInitialGameStateFromConfigIO :: Assets -> GameConfig -> Float -> Float -> IO GameState
createInitialGameStateFromConfigIO gameAssets GameConfig{..} areaWidth areaHeight =
  do
    let robotDiagonalHalf = sqrt (robotWidth**2 + robotHeight**2) / 2
        minX = - (areaWidth  / 2) + robotDiagonalHalf
        maxX =   (areaWidth  / 2) - robotDiagonalHalf
        minY = - (areaHeight / 2) + robotDiagonalHalf
        maxY =   (areaHeight / 2) - robotDiagonalHalf

    let fixedObstacles = createInitialFixedObstacles
    let fixedPolys = map obstacleGameObjectShape fixedObstacles
    let gmap = gameMap (emptyGameState gameAssets areaWidth areaHeight)

    -- genera robots válidos uno a uno
    let genRobots 0 accPos accPolys = pure (reverse accPos)
        genRobots k accPos accPolys = do
          (p,a) <- randomValidRobotPoseIO 200 gmap (fixedPolys ++ accPolys)
          genRobots (k-1) ((p,a):accPos) (robotPolyAt p a : accPolys)

    randomPositions <- genRobots numRobots [] []

    let rs = zipWith3 (\(p,a) (BotConfig ai) i -> createRobot i p a ai)
                      randomPositions botConfigs [1..]

    -- Asegúrate de usar fixedObstacles y luego genera random obstacles evitando fixedPolys ++ robotsPolys.
    (randomObstacles, nextIdAfterRandom) <- generateRandomObstaclesIO (length fixedObstacles + length rs + 1) gmap (fixedPolys ++ map objShape (map robotBase rs))

    return $ (emptyGameState gameAssets areaWidth areaHeight) { robots = rs, obstacles = fixedObstacles ++ randomObstacles }

-- | Genera una lista de obstáculos aleatorios (charcos de aceite y barriles)
--   que no se superpongan con otros elementos existentes.
generateRandomObstaclesIO :: Int -> GameMap -> [G.Polygon] -> IO ([Obstacle], Int)
generateRandomObstaclesIO startId gameMap existingShapes = do
  numOilSpills <- randomRIO (1, 2)
  numBarrels   <- randomRIO (1, 2)

  (oilSpills, nextId1) <- generateObstaclesOfType numOilSpills OilSpillObstacle oilSpillWidth oilSpillHeight startId gameMap existingShapes
  (barrels, nextId2)   <- generateObstaclesOfType numBarrels ExplosiveBarrel explosiveBarrelWidth explosiveBarrelHeight nextId1 gameMap (existingShapes ++ map obstacleGameObjectShape oilSpills)

  return (oilSpills ++ barrels, nextId2)

-- | Función auxiliar para generar obstáculos de un tipo específico.
generateObstaclesOfType :: Int -> ObstacleType -> Float -> Float -> Int -> GameMap -> [G.Polygon] -> IO ([Obstacle], Int)
generateObstaclesOfType num type_ width height startId gameMap existingShapes =
  foldM (\(accObs, currentId) _ -> do
    (newObs, nextId) <- tryPlaceObstacle currentId type_ width height gameMap (existingShapes ++ map obstacleGameObjectShape accObs)
    return (accObs ++ [newObs], nextId))
    ([], startId) [1..num]

-- | Intenta colocar un obstáculo en una posición aleatoria sin superposiciones.
--   Si falla después de varios intentos, puede devolver un error (o reintentar de forma más inteligente).
tryPlaceObstacle :: Int -> ObstacleType -> Float -> Float -> GameMap -> [G.Polygon] -> IO (Obstacle, Int)
tryPlaceObstacle currentId type_ width height gameMap existingShapes = do
  findValidPosition maxAttempts
    where
      maxAttempts = 100
      minX = -(fromIntegral screenWidth / 2) + width / 2
      maxX = (fromIntegral screenWidth / 2) - width / 2
      minY = -(fromIntegral screenHeight / 2) + height / 2
      maxY = (fromIntegral screenHeight / 2) / 2 -- Limit Y to upper half initially for testing

      findValidPosition :: Int -> IO (Obstacle, Int)
      findValidPosition 0 = error $ "Could not find a valid position for a " ++ show type_ ++ " obstacle after " ++ show maxAttempts ++ " attempts."
      findValidPosition attempts = do
        x <- randomRIO (minX, maxX)
        y <- randomRIO (minY, maxY)
        let newPos = (x, y)
            newShape = G.createRectanglePolygon newPos width height 0
            newObstacle = createObstacle currentId newPos width height type_

            -- Check for collision with map walls
            collidesWithWalls = any (checkCollision newShape) (mapWalls gameMap)

            -- Check for collision with existing objects (fixed obstacles + already placed random obstacles + robots)
            collidesWithExisting = any (checkCollision newShape) existingShapes

        if not (collidesWithWalls || collidesWithExisting)
          then return (newObstacle, currentId + 1)
          else findValidPosition (attempts - 1)

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
      , robotStunTime  = 0.0 -- Initialize stun time to 0.0
      , robotStunImmunity = 0.0 -- Initialize stun immunity to 0.0
      }

-- | crea un polígono de robot en (pos, dir)
robotPolyAt :: G.Point -> G.Angle -> G.Polygon
robotPolyAt pos dir = G.createRectanglePolygon pos robotWidth robotHeight dir

-- | intenta N veces una posición válida que no colisione con walls ni con 'existingPolys'
randomValidRobotPoseIO :: Int -> GameMap -> [G.Polygon] -> IO (G.Point, G.Angle)
randomValidRobotPoseIO maxAttempts gmap existingPolys = try maxAttempts
  where
    robotDiag = sqrt (robotWidth**2 + robotHeight**2) / 2
    minX = - (fromIntegral screenWidth  / 2) + robotDiag
    maxX =   (fromIntegral screenWidth  / 2) - robotDiag
    minY = - (fromIntegral screenHeight / 2) + robotDiag
    maxY =   (fromIntegral screenHeight / 2) - robotDiag
    try 0 = error "Could not sample a valid spawn for robot"
    try n = do
      x <- randomRIO (minX, maxX)
      y <- randomRIO (minY, maxY)
      ang <- randomRIO (0, 2*pi)
      let poly = robotPolyAt (x,y) ang
          badWalls = any (checkCollision poly) (mapWalls gmap)
          badOthers = any (checkCollision poly) existingPolys
      if not (badWalls || badOthers)
        then pure ((x,y), ang)
        else try (n-1)

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
              gsTime         = accumulateAliveTime dt gs2   -- Acumular tiempo en vida
              gs3            = handleCollisions gsTime      -- 4️⃣ Gestionar colisiones
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
getActionsForRobot gs r dt =
  if robotStunTime r > 0.0 -- If stunned, return only Idle action
    then pure (objId (robotBase r), [AI.Idle])
    else do
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
                ts0 = tournamentStats currGs
                shooterId = objId (robotBase r)
                ts' = case mProj of
                        Just _  -> incrementShotsFired shooterId ts0
                        Nothing -> ts0
            in (currGs { robots = rs', projectiles = ps', tournamentStats = ts' }, nextId')
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

accumulateAliveTime :: Float -> GameState -> GameState
accumulateAliveTime dt gs =
  let aliveIds = [ objId (robotBase r)
                 | r <- robots gs
                 , robotHealth r > 0 || robotState r == Alive
                 ]
      ts0 = tournamentStats gs
      ts' = foldl' (\acc botId -> addTimeAlive botId dt acc) ts0 aliveIds
  in gs { tournamentStats = ts' }
