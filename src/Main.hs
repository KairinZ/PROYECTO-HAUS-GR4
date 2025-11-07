-- ===========================================================
-- Main.hs
-- ===========================================================
-- Módulo principal del juego “Haskell Tank Tournament”.
-- Se encarga de gestionar:
--   - El flujo general del juego (menús, configuración, combate)
--   - El ciclo principal de Gloss (renderizado, eventos, actualización)
--   - La inicialización del estado global (GameWorld)
-- ===========================================================

{-# LANGUAGE RecordWildCards #-}

module Main where

-- -----------------------------------------------------------
-- IMPORTS
-- -----------------------------------------------------------
-- Gloss: motor gráfico funcional usado para renderizar y manejar eventos.
import Graphics.Gloss hiding (Polygon, arc)
import Graphics.Gloss.Interface.Pure.Game hiding (Polygon, text)
import Graphics.Gloss.Interface.IO.Game (playIO)

-- Módulos del proyecto
import GameTypes (GamePhase(..), GameConfig(..))
import GameConstants
import GameMenus
import GameLogic
import Torneos (runGameLogicWithAutoTournaments, statsFilePath)
import GamePhysics
import GameRender
import CollisionSAT
import Assets -- Importamos Assets
import GameState (GameState(..), GameWorld(..), emptyGameState, initialWorld) -- Importamos GameWorld y initialWorld
import ConfigLoader (loadConfig, TournamentConfig(..)) -- Importamos el cargador de configuración
import GameTypes (GameConfig(..), BotConfig(..)) -- Importamos tipos de configuración
import Entities (AIType(..)) -- Importamos tipos de IA

-- Librerías estándar
import Prelude hiding (Left, Right)
import Control.Monad (when)
import System.Directory (doesFileExist, removeFile)

-- ===========================================================
-- 1) FUNCIÓN PRINCIPAL (MAIN)
-- ===========================================================
-- Gloss se encarga de manejar el ciclo de juego.
-- Aquí se definen las funciones que controlan:
--   - renderizado (render)
--   - manejo de eventos (handleEvent)
--   - actualización del estado (updateGame)
-- ===========================================================

main :: IO ()
main = do
  gameAssets <- loadAssets -- Cargamos los assets al inicio
  tournamentConfig <- loadConfig -- Cargamos la configuración del archivo
  
  -- Crear GameConfig desde TournamentConfig
  let gameConfig = GameConfig
        { numRobots = configNumBots tournamentConfig
        , botConfigs = map BotConfig (configBotTypes tournamentConfig)
        }
  
  -- Crear GameWorld inicial con la configuración del archivo
  let initialWorldWithConfig = initialWorld
        { config = gameConfig
        , tournamentAreaWidth = configAreaWidth tournamentConfig
        , tournamentAreaHeight = configAreaHeight tournamentConfig
        , maxTournamentDuration = configMaxTournamentDuration tournamentConfig
        , numTournaments = configNumTournaments tournamentConfig
        , gameState = emptyGameState gameAssets (fromIntegral $ configAreaWidth tournamentConfig) (fromIntegral $ configAreaHeight tournamentConfig)
        }
  resetStatsFile statsFilePath

  playIO
    window          -- Ventana principal
    backgroundColor -- Color de fondo
    fps             -- Fotogramas por segundo
    initialWorldWithConfig -- Estado inicial del juego con assets y configuración
    renderIO        -- Función para dibujar
    handleEventIO   -- Función para manejar eventos (teclado/ratón)
    updateGameIO    -- Función para actualizar el estado (cada tick)

resetStatsFile :: FilePath -> IO ()
resetStatsFile path = do
  exists <- doesFileExist path
  when exists (removeFile path)

renderIO :: GameWorld -> IO Picture
renderIO = pure . render

handleEventIO :: Event -> GameWorld -> IO GameWorld
handleEventIO ev w = do
  let w' = handleEvent ev w
  case ev of
    EventKey (Char 'r') Down _ _ -> resetStatsFile statsFilePath
    _                           -> pure ()
  let wasPlaying = phase w == Playing
      nowPlaying = phase w' == Playing
  when (not wasPlaying && nowPlaying) (resetStatsFile statsFilePath)
  pure w'

updateGameIO :: Float -> GameWorld -> IO GameWorld
updateGameIO dt w@GameWorld{..} =
  case phase of
    Playing -> runGameLogicWithAutoTournaments dt w
    _       -> pure w

-- ===========================================================
-- 2) DISPATCHERS POR FASE
-- ===========================================================
-- Este bloque actúa como “central de control”.
-- Dependiendo de la fase actual del juego (menú, configuración o partida),
-- redirige las funciones de renderizado, eventos y actualización
-- al módulo o bloque correspondiente.
-- ===========================================================

-- | Renderiza la escena según la fase actual del juego.
--   Devuelve una 'Picture' que Gloss mostrará en pantalla.
render :: GameWorld -> Picture
render w@GameWorld{..} = case phase of
  MainMenu     -> drawMainMenu               -- Pantalla principal
  ConfigScreen -> drawConfigScreen config    -- Pantalla de configuración
  Playing      -> drawGame w                 -- Juego activo (combate)

-- | Gestiona los eventos de entrada (ratón, teclado) según la fase.
--   La tecla 'r' reinicia el juego a su estado inicial.
handleEvent :: Event -> GameWorld -> GameWorld
handleEvent ev w@GameWorld{..}
  -- Si se pulsa la tecla 'r', se reinicia completamente el juego.
  | EventKey (Char 'r') Down _ _ <- ev = 
      let gameAssets = assets gameState
          areaWidth = fromIntegral tournamentAreaWidth
          areaHeight = fromIntegral tournamentAreaHeight
      in initialWorld 
        { gameState = emptyGameState gameAssets areaWidth areaHeight
        , config = config
        , tournamentAreaWidth = tournamentAreaWidth
        , tournamentAreaHeight = tournamentAreaHeight
        , maxTournamentDuration = maxTournamentDuration
        , numTournaments = numTournaments
        , completedTournaments = []
        }
  | otherwise = case phase of
      MainMenu     -> handleMenuEvents ev w      -- Eventos del menú principal
      ConfigScreen -> handleConfigEvents (assets gameState) startGameFromConfig ev w    -- Eventos en pantalla de configuración
      Playing      -> handleGameEvents ev w      -- Eventos en partida (aún vacío)

