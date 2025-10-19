module GameState
  ( GameState(..)
  , countActiveRobots
  ) where

import Entities

data GameState = GameState
  { robots      :: [Robot]
  , projectiles :: [Projectile]
  , explosions  :: [Explosion]
  , time        :: Float
  , gameMap     :: GameMap
  } deriving (Show, Eq)

countActiveRobots :: [Robot] -> Int
countActiveRobots rs = length [r | r <- rs, robotHealth r > 0]
