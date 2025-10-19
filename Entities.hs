module Entities
  ( GameObject(..), ObjectType(..)
  , Robot(..), RobotTurret(..)  
  , Projectile(..)
  , Explosion(..)
  , GameMap(..)
  ) where

import Geometry (Point, Vector, Angle, Size, Polygon)
import qualified Geometry as G
import Memory (Memory)

data ObjectType = RobotType | ProjectileType | ExplosionType
  deriving (Eq, Show)

-- Tipo base común para objetos del juego
data GameObject = GameObject
  { objId    :: Int
  , objPos   :: Point
  , objDir   :: Angle
  , objVel   :: Vector
  , objShape :: Polygon
  , objType ::  ObjectType
  } deriving (Show, Eq)

-- Robot con herencia de GameObject
data Robot = Robot
  { robotBase      :: GameObject
  , robotHealth    :: Int
  , robotMaxHealth :: Int
  , robotRange     :: Float
  , robotTurret    :: RobotTurret
  , robotImg       :: String
  , robotMemory :: Memory
  } deriving (Show, Eq)

data RobotTurret = RobotTurret
  { turretDir      :: Angle
  , turretCooldown :: Float
  , turretRange    :: Float
  , turretImg      :: String
  } deriving (Show, Eq)

-- Proyectil
data Projectile = Projectile
  { projBase  :: GameObject
  , projOwner :: Int
  , projSpeed :: Float
  } deriving (Show, Eq)

data Explosion = Explosion
  { expPos   :: Point
  , expRadius :: Float
  , expTime  :: Int
  } deriving (Show, Eq)

data GameMap = GameMap
  { mapSize       :: Size
  , mapBackground :: String
  } deriving (Show, Eq)
