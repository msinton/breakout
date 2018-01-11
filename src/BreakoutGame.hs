module BreakoutGame ( BreakoutGame(..) ) where

import Controls

data BreakoutGame = Game
  { ballLoc :: (Float, Float)
  , ballVel :: (Float, Float)
  , paddle :: (Float, Float)
  , paddleVel :: Float
  , controls :: Controls
  , blocks :: [(Float, Float)]
  , level :: Integer
  , levelsRemaining :: [[(Float, Float)]]
  , complete :: Bool
  , score :: Integer
  } deriving Show
