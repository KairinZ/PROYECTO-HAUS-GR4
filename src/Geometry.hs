module Geometry
  ( Point, Vector, Angle, Distance, Size, Position, Velocity, 
  Polygon(..) , distanceBetween, angleToTarget
  , deg2rad, rad2deg
  , subVec, addVec, vectorXScalar
  , dot, perp, isInBounds, moveTo, createRectanglePolygon
  ) where

import GameConstants (robotWidth, robotHeight) -- Necesario para createRectanglePolygon

-- Tipos básicos
type Point = (Float, Float)
type Vector = (Float, Float)
type Angle = Float
type Distance = Float
type Size = (Float, Float)
type Position = Point
type Velocity = Vector

-- Polígono para detección de colisiones
newtype Polygon = Polygon { 

  vertices::[Point] 
  } deriving (Show, Eq)


-- Funciones geométricas básicas
distanceBetween :: Position -> Position -> Distance
distanceBetween (x1, y1) (x2, y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

angleToTarget :: Position -> Position -> Angle
angleToTarget (x1, y1) (x2, y2) = atan2 (y2 - y1) (x2 - x1)

deg2rad, rad2deg :: Angle -> Angle
deg2rad deg = deg * pi / 180
rad2deg rad = rad * 180 / pi

subVec :: Vector -> Vector -> Vector
subVec (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

addVec :: Position -> Vector -> Position
addVec (px, py) (vx, vy) = (px + vx, py + vy)

vectorXScalar :: Float -> Vector -> Vector
vectorXScalar t (vx, vy) = (t * vx, t * vy)

dot :: Vector -> Vector -> Float
dot (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

perp :: Vector -> Vector
perp (x, y) = (-y, x)

isInBounds :: Point -> Size -> Bool
isInBounds (x, y) (w, h) = x >= 0 && y >= 0 && x <= w && y <= h

-- | Traslada un polígono a una nueva posición (centro).
moveTo :: Polygon -> Point -> Polygon
moveTo (Polygon vertices) newCenter =
  let (cx, cy) = polygonCenter (Polygon vertices)
      (nx, ny) = newCenter
      offset = (nx - cx, ny - cy)
      
      translatedVertices = map (addVec offset) vertices
  in Polygon translatedVertices

-- | Calcula el centro de un polígono.
polygonCenter :: Polygon -> Point
polygonCenter (Polygon vertices) =
  let numVertices = fromIntegral (length vertices)
      (sumX, sumY) = foldl (\(accX, accY) (vx, vy) -> (accX + vx, accY + vy)) (0.0, 0.0) vertices
  in (sumX / numVertices, sumY / numVertices)

-- | Crea un polígono rectangular rotado en torno a su centro.
--   Se usa tanto para robots como proyectiles.
createRectanglePolygon :: Point -> Float -> Float -> Angle -> Polygon
createRectanglePolygon (cx, cy) w h ang =
  let hw = w / 2
      hh = h / 2
      -- Vértices del rectángulo sin rotar (centrados en el origen)
      vs = [(-hw, -hh), (hw, -hh), (hw, hh), (-hw, hh)]
      -- Aplica rotación y traslación a cada vértice
      rot = map (rotateAndTranslate (cx, cy) ang) vs
  in Polygon rot

-- | Rota un punto alrededor del origen y luego lo traslada al centro indicado.
--   Utilizada internamente por 'createRectanglePolygon'.
rotateAndTranslate :: Point -> Angle -> Point -> Point
rotateAndTranslate (cx, cy) ang (px, py) =
  let c = cos ang
      s = sin ang
      x' = px * c - py * s
      y' = px * s + py * c
  in (x' + cx, y' + cy)