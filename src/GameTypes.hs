{-# LANGUAGE RecordWildCards #-}

module GameTypes where

import Entities
import Graphics.Gloss (Picture)
import ExplosionTypes (Explosion)

-- | Fases principales del flujo del juego.
data GamePhase
  = MainMenu     -- Menú principal (pantalla inicial)
  | ConfigScreen -- Pantalla de configuración de bots
  | Playing      -- Partida en curso
  deriving (Eq, Show)

-- | Configuración individual de cada bot.
--   Actualmente solo contiene el tipo de IA, pero puede ampliarse.
newtype BotConfig = BotConfig { botAI :: AIType }
  deriving (Show)

-- | Configuración global del torneo:
--   cuántos robots habrá y qué IA usa cada uno.
data GameConfig = GameConfig
  { numRobots  :: Int          -- ^ Número de robots activos
  , botConfigs :: [BotConfig]  -- ^ Lista de configuraciones individuales
  }
  deriving (Show)
