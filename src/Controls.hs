module Controls ( Controls(..) ) where

data Controls = Controls
  { keyLeft :: Bool
  , keyRight :: Bool
  , keyEnter :: Bool
  } deriving Show

