module CollisionSAT
  (  CollisionEvent(..)
  , checkCollision
  , detectRobotProjectileCollisions
  , detectRobotRobotCollisions
  , checkCollisions
  ) where

import Geometry
import Entities

data CollisionEvent
  = RobotRobot Int Int
  | RobotProjectile Int Int
  deriving (Show, Eq)

-- Funciones geométricas auxiliares
pointToVector :: Point -> Point -> Point
pointToVector (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)

perpendicular :: Point -> Point
perpendicular (x, y) = (-y, x)

dotProduct :: Point -> Point -> Float
dotProduct (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

projectPoint :: Point -> Point -> Float
projectPoint p axis = dotProduct p axis

projectPolygon :: Polygon -> Point -> (Float, Float)
projectPolygon (Polygon verts) axis =
  (minimum projs, maximum projs)
  where projs = map (`projectPoint` axis) verts

polygonEdges :: Polygon -> [Point]
polygonEdges (Polygon vs) =
  zipWith pointToVector vs (tail vs ++ [head vs])

-- SAT
checkCollision :: Polygon -> Polygon -> Bool
checkCollision polyA polyB = not $ any axisSeparates allAxes
  where
    allAxes = map perpendicular (polygonEdges polyA ++ polygonEdges polyB)
    axisSeparates axis =
      let (minA, maxA) = projectPolygon polyA axis
          (minB, maxB) = projectPolygon polyB axis
      in maxA < minB || maxB < minA

-- Detecta colisiones robot-proyectil
detectRobotProjectileCollisions :: [GameObject] -> [CollisionEvent]
detectRobotProjectileCollisions objs =
  [ RobotProjectile (objId r) (objId p)
  | r <- objs, objType r == RobotType
  , p <- objs, objType p == ProjectileType
  , checkCollision (objShape r) (objShape p)
  ]

-- Detecta colisiones entre robots (usando GameObject)
detectRobotRobotCollisions :: [GameObject] -> [CollisionEvent]
detectRobotRobotCollisions objs =
  [ RobotRobot (objId r1) (objId r2)
  | (i, r1) <- zip [0..] robotObjs
  , (j, r2) <- zip [0..] robotObjs
  , i < j
  , checkCollision (objShape r1) (objShape r2)
  ]
  where
    -- Filtramos solo los objetos que sean robots
    robotObjs = [o | o <- objs, objType o == RobotType]


-- Coordina todas las comprobaciones de colisión
checkCollisions :: [GameObject] -> [CollisionEvent]
checkCollisions objs =
  detectRobotRobotCollisions objs ++ detectRobotProjectileCollisions objs

