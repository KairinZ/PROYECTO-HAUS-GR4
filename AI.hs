module AI
  ( BotAction(..),
  exampleBot
  ) where
import GameState 
import Entities 
import Physics

data BotAction
  = MoveSpeed Float
  | Rotate Float
  | Aim Float
  | Shoot
  | Idle
  deriving (Eq, Show)

type BotPlan  = [BotAction]
type BotBrain = GameState -> Robot -> BotPlan

exampleBot :: BotBrain
exampleBot gs me =
  let targetsInRange = filter (detectedAgent me) (robots gs)
  in if null targetsInRange
       then [MoveSpeed 50]    -- avanza recto
       else [Rotate 0, Shoot]

