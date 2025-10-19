-- Tarea 1 - Introducción a Gloss


import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game --añade funciones para manejar juegos, detectando eventos en teclado .


-----------------------------------
-- Selecciona qué ejemplo ejecutar
-----------------------------------
main :: IO ()
main = main1


-----------------------------------
-- Ejemplo 1: Dibujo estático
-----------------------------------
main1 :: IO ()
main1 = display
  (InWindow "Ejemplo 1 - Dibujo estático" (600, 400) (100, 100))
  white
  picture1

picture1 :: Picture
picture1 = Pictures
  [ Color red   (Translate (-150) 0 (Circle 80))
  , Color blue  (Translate 150 0 (rectangleSolid 120 80))
  , Color green (Rotate 45 (Polygon [(-50,-50),(50,-50),(0,50)]))
  , Translate 0 (-150) (Scale 0.2 0.2 (Text "Tarea 1 - Dibujo de figuras"))
  ]

  
-----------------------------------
-- Ejemplo 2: Animación básica
-----------------------------------
main2 :: IO ()
main2 = animate
  (InWindow "Ejemplo 2 - Animación" (600, 400) (100, 100))
  black
  frame2

frame2 :: Float -> Picture
frame2 t =
  let x = 200 * sin t   -- movimiento oscilante
      y = 100 * cos t
  in Pictures
     [ Color yellow (Translate x y (Circle 50))
     , Color white (Translate (-250) (-170) (Scale 0.15 0.15 (Text "Círculo animado con sin/cos")))
     ]


-----------------------------------
-- Ejemplo 3: Interacción con teclado (modo play)
-----------------------------------
-- Estado del "juego"
type World = (Float, Float)  -- posición (x, y)

main3 :: IO ()
main3 = play
  (InWindow "Ejemplo 3 - Interacción con teclado" (600, 400) (100, 100))
  white
  60            -- FPS
  (0, 0)        -- posición inicial
  render3
  handleEvent3
  update3

-- Dibuja el cuadrado en su posición actual
render3 :: World -> Picture
render3 (x, y) = Pictures
  [ Translate x y (Color red (rectangleSolid 60 60))
  , Translate (-250) (-170) (Scale 0.15 0.15 (Text "Usa las flechas para mover el cuadrado"))
  ]


-- Maneja eventos de teclado
handleEvent3 :: Event -> World -> World
handleEvent3 (EventKey (SpecialKey KeyRight) Down _ _) (x, y) = (x + 10, y)
handleEvent3 (EventKey (SpecialKey KeyLeft)  Down _ _) (x, y) = (x - 10, y)
handleEvent3 (EventKey (SpecialKey KeyUp)    Down _ _) (x, y) = (x, y + 10)
handleEvent3 (EventKey (SpecialKey KeyDown)  Down _ _) (x, y) = (x, y - 10)
handleEvent3 _ w = w

-- No se usa animación automática (pero podrías añadirla)
update3 :: Float -> World -> World
update3 _ w = w
