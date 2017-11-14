module BreakoutGame ( BreakoutGame(..) ) where

data BreakoutGame = Game
  { ballLoc :: (Float, Float)
  , ballVel :: (Float, Float)
  , paddle :: (Float, Float)
  , paddleVel :: Float
  , keyLeft :: Bool
  , keyRight :: Bool
  , blocks :: [(Float, Float)]
  } deriving Show