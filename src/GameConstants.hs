module GameConstants where

import Graphics.Gloss

-- ---------- Dimensiones de pantalla ----------
screenWidth, screenHeight :: Int
screenWidth  = 800
screenHeight = 600

-- ---------- Configuración de la ventana ----------
window :: Display
window = InWindow "Haskell Tank Tournament"
                  (screenWidth, screenHeight)
                  (100, 100)

-- ---------- Color de fondo ----------
backgroundColor :: Color
backgroundColor = makeColor 0.1 0.1 0.1 1.0

-- ---------- Frecuencia de refresco ----------
fps :: Int
fps = 60

-- ---------- Tamaños de entidades ----------
-- Cuerpo del robot
robotWidth, robotHeight :: Float
robotWidth  = 40
robotHeight = 30

robotOriginalWidth, robotOriginalHeight :: Float
robotOriginalWidth  = 76
robotOriginalHeight = 72

-- Torreta del robot
turretWidth, turretHeight :: Float
turretWidth  = 25
turretHeight = 8

-- Proyectiles
projectileWidth, projectileHeight, projectileSpeed :: Float
projectileWidth  = 18
projectileHeight = 6
projectileSpeed  = 300

-- Duración de vida del proyectil (segundos)
projectileLifetime :: Float
projectileLifetime = 2.5

-- ---------- Colisiones de robots ----------
robotCollisionRadius :: Float
robotCollisionRadius = 15.0

robotCollisionDamage :: Int
robotCollisionDamage = 2
