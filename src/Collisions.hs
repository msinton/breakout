module Collisions (

    topCollision, bottomCollision, sideCollision,

    verticalBlockCollision, horizontalBlockCollision,

    paddleCollision,

    Radius, Position

) where

import LayoutDimensions

type Radius = Float
type Position = (Float, Float)


-- | Given position and radius of the ball, return whether the ball is colliding with the walls of the game
topCollision, bottomCollision, sideCollision :: Position -> Radius -> Bool
topCollision    (_, y) radius = y + radius >=    height / 2
bottomCollision (_, y) radius = y - radius <= - (height / 2)
sideCollision    (x, _) radius = abs(x) + radius >=  width / 2

-- | Given position of block and position and radius of the ball, return whether the ball is colliding with the block
topBlockCollision, leftBlockCollision, rightBlockCollision, bottomBlockCollision :: Position -> Position -> Radius -> Bool
topBlockCollision block@(blockX, blockY) (x, y) radius = withinY && withinX
    where
        withinX = x + radius >= (blockX - blockWidth / 2) && x - radius <= (blockX + blockWidth / 2)
        withinY = y - radius <= (blockY + blockHeight / 2) && y >= (blockY + blockHeight / 2)
leftBlockCollision (blockX, blockY) (x, y) radius = withinY && withinX
    where
        withinX = x + radius >= (blockX - blockWidth / 2) && x <= (blockX - blockWidth / 2)
        withinY = y + radius >= (blockY - blockHeight / 2) && y - radius <= (blockY + blockHeight / 2)
rightBlockCollision (blockX, blockY) (x, y) radius = withinY && withinX
    where
        withinX = x - radius <= (blockX + blockWidth / 2) && x >= (blockX + blockWidth / 2)
        withinY = y + radius >= (blockY - blockHeight / 2) && y - radius <= (blockY + blockHeight / 2)
bottomBlockCollision (blockX, blockY) (x, y) radius = withinY && withinX
    where
        withinX = x + radius >= (blockX - blockWidth / 2) && x - radius <= (blockX + blockWidth / 2)
        withinY = y + radius >= (blockY - blockHeight / 2) && y <= (blockY - blockHeight / 2)

horizontalBlockCollision blockDimensions ballLocation radius = any id $ [leftBlockCollision, rightBlockCollision] <*> [blockDimensions] <*> [ballLocation] <*> [radius]

verticalBlockCollision blockDimensions ballLocation radius = any id $ [topBlockCollision, bottomBlockCollision] <*> [blockDimensions] <*> [ballLocation] <*> [radius]

-- | Given position of paddle and ball returns the fractional distance along the top of the paddle that the ball struck
-- | Where 0 indicates hitting the left-most side of the paddle, 1 indicates the right-most side, 1/2 is the center
paddleCollision :: Position -> Position -> Radius -> (Bool, Float)
paddleCollision (blockX, blockY) (x, y) radius = (collided, fraction)
    where
        withinX = x + radius >= (blockX - blockWidth / 2) && x - radius <= (blockX + blockWidth / 2)
        withinY = y - radius <= (blockY + blockHeight / 2) && y >= (blockY - blockHeight / 2)
        collided = withinX && withinY
        fraction = if collided then (x - (blockX - (blockWidth / 2 + radius))) / (blockWidth + radius * 2) else 0
