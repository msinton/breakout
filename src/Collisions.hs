module Collisions (

    topCollision, bottomCollision, sideCollision,

    verticalBlockCollision, horizontalBlockCollision,

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

-- | Given possition of block and position and radius of the ball, return whether the ball is colliding with the block
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


