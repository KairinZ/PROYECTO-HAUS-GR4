{-# LANGUAGE OverloadedStrings #-}

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy (loadJuicyPNG)

-- ============================
--   Tipos de datos
-- ============================

data Obstacle = Obstacle { obsX :: Float, obsY :: Float } deriving Show

data World = World
  { posX :: Float
  , posY :: Float
  , velX :: Float
  , velY :: Float
  , onGround :: Bool
  , obstacles :: [Obstacle]
  , timeSinceLast :: Float
  , logo :: Picture
  } deriving Show

-- ============================
--   Constantes
-- ============================

groundY, gravity, jumpVel, moveSpeed, screenW, screenH, obstacleSpeed :: Float
groundY = -150
gravity = -800
jumpVel = 400
moveSpeed = 180
screenW = 300
screenH = 300
obstacleSpeed = 150  -- velocidad de los logos

-- ============================
--   Estado inicial
-- ============================

initialWorld :: Picture -> World
initialWorld logoImg = World
  { posX = -200
  , posY = groundY
  , velX = 0
  , velY = 0
  , onGround = True
  , obstacles = []
  , timeSinceLast = 0
  , logo = logoImg
  }

-- ============================
--   Dibujo
-- ============================

drawWorld :: World -> Picture
drawWorld w = Pictures
  [ backgroundText,Translate (posX w) (posY w) drawMonigote
  , Pictures (map (drawObstacle (logo w)) (obstacles w))
  ]  where
    backgroundText = Translate (-180) 250 $ Scale 0.15 0.15 $
      Color (makeColorI 0 0 0 40) (Text "Esquiva a ChatGPT")

drawMonigote :: Picture
drawMonigote = Pictures
  [ Color (makeColorI 255 224 189 255) (Translate 0 30 (circleSolid 15))
  , Color red (Translate 0 5 (rectangleSolid 20 30))
  , Color blue (Translate 0 (-25) (rectangleSolid 15 30))
  ]

drawObstacle :: Picture -> Obstacle -> Picture
drawObstacle img o =
  Translate (obsX o) (obsY o) $
    Scale 0.1 0.1 img  -- logos más pequeños

-- ============================
--   Eventos
-- ============================

handleEvent :: Event -> World -> World
-- Movimiento horizontal
handleEvent (EventKey (Char 'a') Down _ _) w = w { velX = -moveSpeed }
handleEvent (EventKey (Char 'd') Down _ _) w = w { velX =  moveSpeed }


-- Detener movimiento al soltar teclas
handleEvent (EventKey (Char 'a') Up _ _) w = w { velX = 0 }
handleEvent (EventKey (Char 'd') Up _ _) w = w { velX = 0 }


-- Salto
handleEvent (EventKey (Char 'w') Down _ _) w
  | onGround w = w { velY = jumpVel, onGround = False }
  | otherwise = w

handleEvent _ w = w

-- ============================
--   Actualización
-- ============================

updateWorld :: Float -> World -> World
updateWorld dt w =
  let vy' = velY w + gravity * dt
      y'  = posY w + vy' * dt
      x'  = posX w + velX w * dt

      (finalY, finalVy, grounded)
        | y' <= groundY = (groundY, 0, True)
        | otherwise     = (y', vy', False)

      -- Mantener dentro de los bordes
      xLim = max (-screenW+20) (min (screenW-20) x')

      -- Obstáculos moviéndose
      movedObs = [ o { obsX = obsX o - obstacleSpeed * dt }
                 | o <- obstacles w, obsX o > -screenW - 50 ]

      -- Generar nuevo obstáculo cada ~2.5 segundos
      t' = timeSinceLast w + dt
      (newObs, tFinal) =
        if t' > 2.5
          then (Obstacle screenW groundY : movedObs, 0)
          else (movedObs, t')

      -- Colisión simple
      collision = any (collides xLim finalY) newObs
   in if collision
        then w { posX = -200, posY = groundY, velX = 0, velY = 0
               , onGround = True, obstacles = [] }
        else w { posX = xLim, posY = finalY, velY = finalVy, onGround = grounded
               , obstacles = newObs, timeSinceLast = tFinal }

collides :: Float -> Float -> Obstacle -> Bool
collides px py o =
  abs (px - obsX o) < 35 && abs (py - obsY o) < 35

-- ============================
--   Programa principal
-- ============================

main :: IO ()
main = do
  maybeLogo <- loadJuicyPNG "chatgpt.png"
  let logoImg = maybe (Color green (circleSolid 15)) id maybeLogo
  play
    (InWindow "Esquiva a ChatGPT" (600, 600) (100, 100))
    white
    60
    (initialWorld logoImg)
    drawWorld
    handleEvent
    updateWorld
