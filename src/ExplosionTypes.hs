module ExplosionTypes where

data ExplosionType = Impact | Death deriving (Show, Eq)

data Explosion = Explosion
  { expPos      :: (Float, Float)
  , expFrameIdx :: Int
  , expType     :: ExplosionType
  }
  deriving (Show, Eq)
