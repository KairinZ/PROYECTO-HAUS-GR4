module Geometry
  ( Point, Vector, Angle, Distance, Size, Position, Velocity, 
  Polygon(..) , distanceBetween, angleToTarget
  , deg2rad, rad2deg
  , subVec, addVec, vectorXScalar
  , dot, perp, isInBounds
  ) where

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