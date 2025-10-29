{-# LANGUAGE RecordWildCards #-}

module GameMenus where

import Graphics.Gloss hiding (Polygon, arc)
import qualified Graphics.Gloss.Interface.Pure.Game as Gloss (text)
import Graphics.Gloss.Interface.Pure.Game hiding (Polygon, text)
import GameTypes (GameConfig(..), GamePhase(..), BotConfig(..))
import GameConstants
import qualified Geometry as G
import Data.List (findIndex)
import Entities
import AI as AI
import Assets (Assets)
import GameState (GameWorld(..))

-- ---------- Helpers de interfaz ----------
-- | Dibuja un botón rectangular con texto centrado.
drawButton :: String -> Float -> Float -> Float -> Float -> Picture
drawButton txt x y w h = pictures
  [ translate x y $ color (greyN 0.2) $ rectangleSolid w h
  , translate x y $ color white       $ rectangleWire  w h
  , translate (x - w/2 + 10) (y - 10)
      $ scale 0.2 0.2
      $ color white
      $ Gloss.text txt
  ]

-- | Convierte un AIType a su representación de cadena simple (sin detalles de registro).
-- esto lo hacemos por que ahora el AItype tiene tambien el rango, y para que no salga en el menu todo, hacemos esto
aiTypeToString :: AIType -> String
aiTypeToString (Hunter {})  = "Hunter"
aiTypeToString (Evasive {}) = "Evasive"

-- | Determina si un clic del ratón está dentro de un rectángulo dado.
isClickInBox :: G.Point -> Float -> Float -> Float -> Float -> Bool
isClickInBox (mx, my) x y w h =
  mx >= x - w/2 && mx <= x + w/2 &&
  my >= y - h/2 && my <= y + h/2


-- ---------- Menú principal ----------
-- | Dibuja la pantalla inicial con el título y el botón “Start Game”.
drawMainMenu :: Picture
drawMainMenu = pictures
  [ translate (-390)  50
      $ scale 0.5 0.5
      $ color white
      $ Gloss.text "Haskell Tank Tournament"

  , drawButton "Start Game" 0 (-50) 200 40

  , translate (-200) (-200)
      $ scale 0.15 0.15
      $ color white
      $ Gloss.text "Press R at any time to return here."
  ]

-- | Gestiona los clics en el menú principal.
--   Si el jugador pulsa sobre “Start Game”, cambia a la pantalla de configuración.
handleMenuEvents :: Event -> GameWorld -> GameWorld
handleMenuEvents (EventKey (MouseButton LeftButton) Down _ (mx,my)) w =
  if isClickInBox (mx,my) 0 (-50) 200 40
     then w { phase = ConfigScreen }
     else w
handleMenuEvents _ w = w


-- ---------- Pantalla de configuración ----------
-- | Dibuja la interfaz donde se puede ajustar el número de bots
--   y elegir el tipo de IA de cada uno antes de iniciar el torneo.
drawConfigScreen :: GameConfig -> Picture
drawConfigScreen GameConfig{..} = pictures $
  [ translate (-200) 250 $ scale 0.3 0.3 $ color white $ Gloss.text "Configure Tournament"
  , translate (-200) 180 $ scale 0.2 0.2 $ color white $ Gloss.text "Number of Bots:"
  , drawButton "-"   (-50) 140 40 40
  , translate 40 180 $ scale 0.2 0.2 $ color white $ Gloss.text (show numRobots)
  , drawButton "+"    50  140 40 40
  ]
  ++ concatMap drawEntry (zip [0..] botConfigs)
  ++ [ drawButton "Launch Tournament" 0 (-250) 300 50 ]
  where
    -- Dibuja cada línea de configuración individual (uno por bot).
    drawEntry (i, BotConfig{..}) =
      let y = 100 - fromIntegral i * 80
      in [ translate (-250) (y-20)
             $ scale 0.2 0.2
             $ color white
             $ Gloss.text ("Bot " ++ show (i+1) ++ ":")
         , drawButton (aiTypeToString botAI) 0 (y - 10) 200 40
         ]

-- | Gestiona los clics dentro de la pantalla de configuración.
--   Permite:
--     - Aumentar o reducir el número de bots
--     - Cambiar el tipo de IA de cada uno
--     - Lanzar la partida
handleConfigEvents :: Assets -> (Assets -> GameConfig -> GameWorld) -> Event -> GameWorld -> GameWorld
handleConfigEvents gameAssets startGameFromConfigFunc (EventKey (MouseButton LeftButton) Down _ (mx,my)) w@GameWorld{..} =
  let GameConfig{..} = config

      -- (+) o (-) número de robots (entre 2 y 4)
      newNum =
        if      isClickInBox (mx,my) (-50) 140 40 40 then max 2 (numRobots - 1)
        else if isClickInBox (mx,my)  50  140 40 40 then min 4 (numRobots + 1)
        else numRobots

      -- Ajusta la lista de configuraciones si cambia el número de bots
      baseCfgs
        | newNum > numRobots = take newNum (botConfigs ++ repeat (BotConfig (Hunter { hunterDetectionRange = 400.0 })))
        | newNum < numRobots = take newNum botConfigs
        | otherwise          = botConfigs

      -- Detecta si el clic se hizo sobre una de las cajas de IA
      aiRects = [ (0, 100 - fromIntegral i * 80 - 10, 200, 40)
                | i <- [0 .. length baseCfgs - 1]
                ]
      clickedIdx = findIndex (\(x,y,w',h') -> isClickInBox (mx,my) x y w' h') aiRects

      -- Si se ha clicado un tipo de IA, se alterna (Hunter ↔ Evasive)
      updatedCfgs =
        case clickedIdx of
          Nothing  -> baseCfgs
          Just idx ->
            let (pre, BotConfig ai : post) = splitAt idx baseCfgs
            in pre ++ [BotConfig (cycleAI ai)] ++ post

      newCfg = config { numRobots = newNum, botConfigs = updatedCfgs }

  -- Si se pulsa el botón "Launch Tournament", se inicia la partida.
  in if isClickInBox (mx,my) 0 (-250) 300 50
     then startGameFromConfigFunc gameAssets newCfg
     else w { config = newCfg }
handleConfigEvents _ _ _ w = w

-- | Cambia el tipo de IA entre Hunter y Evasive.
cycleAI :: AIType -> AIType
cycleAI (Hunter { hunterDetectionRange = r }) = Evasive { evasiveDetectionRange = r }
cycleAI (Evasive { evasiveDetectionRange = r }) = Hunter { hunterDetectionRange = r }
