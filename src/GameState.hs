module GameState
  ( GameState(..)
  , GameWorld(..)
  , countActiveRobots
  , emptyGameState
  , initialWorld
  , GameMap(..) -- Export GameMap
  ) where

import Entities hiding (Explosion) -- Ocultamos Explosion de Entities
import Assets (Assets)
import ExplosionTypes (Explosion)
import GameTypes (GamePhase(MainMenu), GameConfig(GameConfig, numRobots, botConfigs), BotConfig(BotConfig))
import GameConstants (screenWidth, screenHeight, robotWidth, robotHeight)
import Geometry (createRectanglePolygon)

data GameState = GameState
  { robots      :: [Robot]
  , projectiles :: [Projectile]
  , explosions  :: [Explosion]
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
