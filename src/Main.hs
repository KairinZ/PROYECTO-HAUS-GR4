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

-- Módulos del proyecto
import GameTypes (GamePhase(..), GameConfig(..))
import GameConstants
import GameMenus
import GameLogic
import Torneos (runGameLogicWithAutoTournaments)
import GamePhysics
import GameRender
import CollisionSAT
import Assets -- Importamos Assets
import GameState (GameState(..), GameWorld(..), emptyGameState, initialWorld) -- Importamos GameWorld y initialWorld

-- Librerías estándar
import Prelude hiding (Left, Right)

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
  play
    window          -- Ventana principal
    backgroundColor -- Color de fondo
    fps             -- Fotogramas por segundo
    (initialWorld { gameState = emptyGameState gameAssets }) -- Estado inicial del juego con assets
    render          -- Función para dibujar
    handleEvent     -- Función para manejar eventos (teclado/ratón)
    updateGame      -- Función para actualizar el estado (cada tick)

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
  | EventKey (Char 'r') Down _ _ <- ev = initialWorld { gameState = emptyGameState (assets gameState) }
  | otherwise = case phase of
      MainMenu     -> handleMenuEvents ev w      -- Eventos del menú principal
      ConfigScreen -> handleConfigEvents (assets gameState) startGameFromConfig ev w    -- Eventos en pantalla de configuración
      Playing      -> handleGameEvents ev w      -- Eventos en partida (aún vacío)

-- | Actualiza el estado del juego cada frame.
--   Solo ejecuta la lógica cuando se está jugando.
updateGame :: Float -> GameWorld -> GameWorld
updateGame dt w@GameWorld{..} =
  case phase of
    Playing -> runGameLogicWithAutoTournaments dt w   -- Avanza la simulación con torneos automáticos
    _       -> w                   -- En menús, no cambia nada

