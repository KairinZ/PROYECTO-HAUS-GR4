module Entities
  ( GameObject(..), ObjectType(..)
  , Robot(..), RobotTurret(..)
  , Projectile(..), ProjectileType(..)
  , Explosion(..)
  , GameMap(..)
  , AIType(..)
  ) where

import Geometry (Point, Vector, Angle, Polygon)
import Memory (Memory)

-- El tipo de IA ahora se define aquí para romper la dependencia circular.
data AIType = Hunter | Evasive
  deriving (Eq, Show, Enum, Bounded)

data ObjectType = RobotType | ProjectileType | ExplosionType
  deriving (Eq, Show)

-- Tipo base común para objetos del juego.
data GameObject = GameObject
  { objId    :: Int
  , objPos   :: Point
  , objDir   :: Angle   -- Dirección del chasis en radianes
  , objVel   :: Vector
  , objShape :: Polygon
  , objType  :: ObjectType
  } deriving (Show, Eq)

-- Robot con herencia de GameObject.
data Robot = Robot
  { robotBase      :: GameObject
  , robotHealth    :: Int
  , robotMaxHealth :: Int
  , robotRange     :: Float
  , robotTurret    :: RobotTurret
  , robotMemory    :: Memory
  , robotAIType    :: AIType
  } deriving (Show, Eq)

data RobotTurret = RobotTurret
  { turretDir      :: Angle  -- Dirección de la torreta en radianes
  , turretCooldown :: Float
  , turretMaxCooldown :: Float -- Cooldown máximo para resetear
  } deriving (Show, Eq)

-- Tipo de proyectil (para futuras ampliaciones).
data ProjectileType = Bullet
  deriving (Show, Eq)

-- Proyectil
data Projectile = Projectile
  { projBase      :: GameObject
  , projOwner     :: Int
  , projDamage    :: Int
  , projType      :: ProjectileType
  , projLifetime  :: Float -- Tiempo de vida restante
  , projMaxLifetime :: Float -- Tiempo de vida inicial
  } deriving (Show, Eq)

-- Explosión
data Explosion = Explosion
  { expPos      :: Point
  , expSize     :: Float
  , expLifetime :: Float -- Tiempo de vida restante
  } deriving (Show, Eq)

-- Mapa del juego
data GameMap = GameMap
  { mapWidth  :: Float
  , mapHeight :: Float
  , mapWalls  :: [Polygon]
  } deriving (Show, Eq)
