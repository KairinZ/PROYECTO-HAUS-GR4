module GameState
  ( GameState(..)
  , GameWorld(..)
  , countActiveRobots
  , emptyGameState
  , initialWorld
  , GameMap(..) -- Export GameMap
  , createInitialFixedObstacles -- Export para usar en GameLogic
  , createObstacle -- Export createObstacle for use in GameLogic
  ) where

import Entities hiding (Explosion) -- Ocultamos Explosion de Entities
import Assets (Assets)
import ExplosionTypes (Explosion)
import GameTypes (GamePhase(MainMenu), GameConfig(GameConfig, numRobots, botConfigs), BotConfig(BotConfig))
import GameConstants (screenWidth, screenHeight, robotWidth, robotHeight)
import Geometry (createRectanglePolygon, Point)

data GameState = GameState
  { robots      :: [Robot]
  , projectiles :: [Projectile]
  , explosions  :: [Explosion]
  , obstacles   :: [Obstacle]
  , time        :: Float
  , gameMap     :: GameMap
  , assets      :: Assets
  }

-- | Estado completo del mundo en un momento dado.
--   Contiene la fase del juego, el estado de simulación,
--   la configuración activa y un contador de IDs para proyectiles.
data GameWorld = GameWorld
  { phase     :: GamePhase    -- ^ Fase actual (menú, configuración o partida)
  , gameState :: GameState    -- ^ Estado del juego (robots, balas, explosiones)
  , config    :: GameConfig   -- ^ Configuración activa de la partida
  , nextId    :: Int          -- ^ Siguiente ID disponible para proyectiles o robots
  }

emptyGameState :: Assets -> GameState
emptyGameState gameAssets = GameState
  { robots      = []
  , projectiles = []
  , explosions  = []
  , obstacles   = []
  , time        = 0
  , gameMap     = createGameMap (fromIntegral screenWidth) (fromIntegral screenHeight)
  , assets      = gameAssets
  }

-- | Crea un GameMap con paredes alrededor de los bordes de la pantalla.
createGameMap :: Float -> Float -> GameMap
createGameMap width height =
  let wallThickness = 20.0 -- Ancho de las paredes
      halfWidth = width / 2
      halfHeight = height / 2

      -- Pared superior
      wallTop = createRectanglePolygon (0, halfHeight + wallThickness/2) (width + wallThickness*2) wallThickness 0
      -- Pared inferior
      wallBottom = createRectanglePolygon (0, -halfHeight - wallThickness/2) (width + wallThickness*2) wallThickness 0
      -- Pared izquierda
      wallLeft = createRectanglePolygon (-halfWidth - wallThickness/2, 0) wallThickness (height + wallThickness*2) 0
      -- Pared derecha
      wallRight = createRectanglePolygon (halfWidth + wallThickness/2, 0) wallThickness (height + wallThickness*2) 0

  in GameMap { mapWidth = width, mapHeight = height, mapWalls = [wallTop, wallBottom, wallLeft, wallRight] }

initialWorld :: GameWorld
initialWorld = GameWorld
  { phase     = MainMenu
  , gameState = error "GameState must be initialized with assets."
  , config    = GameConfig
    { numRobots  = 2
    , botConfigs = [BotConfig (Hunter { hunterDetectionRange = 400.0 }), BotConfig (Evasive { evasiveDetectionRange = 200.0 })]
    }
  , nextId    = 1
  }

countActiveRobots :: [Robot] -> Int
countActiveRobots rs = length [r | r <- rs, robotHealth r > 0]

-- | Tamaños de los obstáculos (en píxeles)
crateWidth, crateHeight :: Float
crateWidth = 50.0
crateHeight = 50.0

fenceWidth, fenceHeight :: Float
fenceWidth = 50.0
fenceHeight = 30.0

-- | Crea los obstáculos fijos iniciales del juego (cajas, vallas, barricadas).
--   Define 3 cajas y 3 vallas en posiciones específicas del mapa.
createInitialFixedObstacles :: [Obstacle]
createInitialFixedObstacles =
  let -- 3 cajas en posiciones específicas
      crate1 = createObstacle 1 (-150, 150) crateWidth crateHeight CrateObstacle
      crate2 = createObstacle 2 (150, -150) crateWidth crateHeight CrateObstacle
      crate3 = createObstacle 3 (0, 0) crateWidth crateHeight CrateObstacle
      
      -- 3 vallas en posiciones específicas
      fence1 = createObstacle 4 (70, -280) fenceWidth fenceHeight FenceObstacle
      fence2 = createObstacle 5 (0, 270) fenceWidth fenceHeight FenceObstacle
      fence3 = createObstacle 6 (-100, 200) fenceWidth fenceHeight FenceObstacle
      
      -- 2 barricadas de madera en posiciones específicas, con tamaño de caja
      barricade1 = createObstacle 7 (200, 50) crateWidth crateHeight BarricadeObstacle
      barricade2 = createObstacle 8 (-220, -50) crateWidth crateHeight BarricadeObstacle
      
  in [crate1, crate2, crate3, fence1, fence2, fence3, barricade1, barricade2]

-- | Crea un obstáculo en la posición especificada.
createObstacle :: Int -> Point -> Float -> Float -> ObstacleType -> Obstacle
createObstacle obsId pos w h obsType =
  Obstacle
    { obstacleId     = obsId
    , obstaclePos    = pos
    , obstacleWidth  = w
    , obstacleHeight = h
    , obstacleShape  = createRectanglePolygon pos w h 0  -- Los obstáculos no rotan (ángulo 0)
    , obstacleType   = obsType
    , obstacleCountdown = Nothing
    }
