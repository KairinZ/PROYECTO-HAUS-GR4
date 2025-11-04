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

-- ---------- Daño por colisión con barricada ----------
barricadeCollisionDamage :: Int
barricadeCollisionDamage = 20

-- ---------- Barril explosivo ----------
explosiveBarrelRadius :: Float
explosiveBarrelRadius = 150.0

explosiveBarrelDamage :: Int
explosiveBarrelDamage = 30

explosiveBarrelCountdown :: Float
explosiveBarrelCountdown = 3.0

explosiveBarrelTriggerRadius :: Float
explosiveBarrelTriggerRadius = 80.0

explosiveBarrelWidth, explosiveBarrelHeight :: Float
explosiveBarrelWidth = 50.0
explosiveBarrelHeight = 50.0

-- Barrel countdown bar dimensions
barrelBarWidth, barrelBarHeight :: Float
barrelBarWidth = 35.0
barrelBarHeight = 6.0

-- ---------- Charco de aceite ----------
oilSpillWidth, oilSpillHeight :: Float
oilSpillWidth = 30.0
oilSpillHeight = 30.0