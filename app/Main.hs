module Main(main) where

import Data.List
import Data.Fixed
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import BreakoutGame

blockWidth, blockHeight, borderSize :: Float
blockWidth = 70
blockHeight = 22
borderSize = 100

gridWidth, gridHeight, width, height :: Float
gridWidth = 7 * blockWidth
gridHeight = 500
width = gridWidth + 2 * borderSize
height = gridHeight + 2 * borderSize

window :: Display
window = InWindow "Breakout" (round width, round height) (0, 0)

background :: Color
background = black

paddleColor, paddleBorderColor :: Color
paddleColor = light $ light blue
paddleBorderColor = rose

paddleWidth = 70
paddleHeight = 22

blocksByGrid = [(0, 0), (0, 1), (0, 2), (1, 1), (2, 2), (3, 3), (4, 4), (4, 6)]

gridPosToPos (x, y) = (x * blockWidth - gridWidth / 2, y * (- blockHeight) + gridHeight / 2)

initialState :: BreakoutGame
initialState = Game
  { ballLoc = (0, 0)
  , ballVel = (200, -150)
  , paddle = (gridWidth / 2 - 10, - gridHeight + 2 * borderSize)
  , paddleVel = 0
  , keyLeft = False
  , keyRight = False
  , blocks = map gridPosToPos blocksByGrid
  }

blockColor = green
blockBorderColor = light $ light black

radius = 10

paddleBounce :: BreakoutGame -> BreakoutGame
paddleBounce game = game { ballVel = (vx', vy') }
  where
    (vx, vy) = ballVel game
    vy' = if topBlockCollision (paddle game) (ballLoc game) radius then -vy else vy
    vx' = if leftBlockCollision (paddle game) (ballLoc game) radius ||
             rightBlockCollision (paddle game) (ballLoc game) radius
          then -vx
          else vx

topBounce :: BreakoutGame -> BreakoutGame
topBounce game = game { ballVel = (vx, vy') }
  where
    (vx, vy) = ballVel game
    vy' = if topCollision (ballLoc game) radius then -vy else vy

sideBounce :: BreakoutGame -> BreakoutGame
sideBounce game = game { ballVel = (vx', vy) }
    where
      (vx, vy) = ballVel game
      vx' = if sideCollision (ballLoc game) radius then -vx else vx

blocksBounce :: BreakoutGame -> BreakoutGame
blocksBounce game 
	| leftCollision = game { ballVel = (-vx, vy) }
	| rightCollision = game { ballVel = (-vx, vy) }
	| topCollision = game { ballVel = (vx, -vy) }
	| bottomCollision = game { ballVel = (vx, -vy) }
	| otherwise = game
   where
     (vx, vy) = ballVel game
     leftCollision = any (id) [leftBlockCollision block (ballLoc game) radius | block <- (blocks game)]
     rightCollision = any (id) [rightBlockCollision block (ballLoc game) radius | block <- (blocks game)]
     topCollision = any (id) [topBlockCollision block (ballLoc game) radius | block <- (blocks game)]
     bottomCollision = any (id) [bottomBlockCollision block (ballLoc game) radius | block <- (blocks game)]



moveBall :: Float -> BreakoutGame -> BreakoutGame
moveBall seconds game = game { ballLoc = (x', y') }
  where
    -- Old locations and velocities.
    (x, y) = ballLoc game
    (vx, vy) = ballVel game

    -- New locations.
    x' = (mod' (width / 2 + x + vx * seconds) width) - width / 2
    y' = y + vy * seconds

a = gridWidth / 2 - 20
b = - gridHeight + 2 * borderSize

render :: BreakoutGame -> Picture
render game = pictures $ [
    ball
    , mkBlock paddleBorderColor paddleColor $ paddle game
    ] ++ [ mkBlock blockBorderColor blockColor b | b <- blocks game]
    where
        ball = uncurry translate (ballLoc game) $ color ballColor $ circleSolid radius
        ballColor = dark red

        -- borderColour -> colour -> position -> Picture
        mkBlock :: Color -> Color -> (Float, Float) -> Picture
        mkBlock borderCol col (x, y) = pictures
            [ translate x y $ color borderCol $ rectangleSolid paddleWidth paddleHeight
            , translate x y $ color col $ rectangleSolid (paddleWidth - 4) (paddleHeight - 4)
            ]


fps :: Int
fps = 60

type Radius = Float
type Position = (Float, Float)

-- | Given position and radius of the ball, return whether a collision occurred.
topCollision, bottomCollision, sideCollision :: Position -> Radius -> Bool
topCollision    (_, y) radius = y + radius >=    height / 2
bottomCollision (_, y) radius = y - radius <= - (height / 2)
sideCollision    (x, _) radius = abs(x) + radius >=  width / 2

topBlockCollision, leftBlockCollision, rightBlockCollision :: Position -> Position -> Radius -> Bool
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

blockLeft :: Position -> Float
blockLeft (blockX, blockY) = blockX - blockWidth / 2
blockRight (blockX, blockY) = blockX + blockWidth / 2
blockTop (blockX, blockY) = blockY - blockWidth / 2
blockBottom (blockX, blockY) = blockY - blockWidth / 2



handleKeys :: Event -> BreakoutGame -> BreakoutGame

handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) game =  game { keyLeft = True }
handleKeys (EventKey (SpecialKey KeyLeft) Up _ _) game =    game { keyLeft = False }
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) game = game { keyRight = True }
handleKeys (EventKey (SpecialKey KeyRight) Up _ _) game =   game { keyRight = False }

-- Do nothing for all other events.
handleKeys _ game = game

paddleMove seconds game = game { paddle = (x', y), paddleVel = vx' } where
      left = keyLeft game
      right = keyRight game
      (x, y) = paddle game
      vx = paddleVel game

      vx' = if left && right then 0
        else if left then max (-1000) $ min (vx - 30) (- 70)
        else if right then min 1000 $ max (vx + 30) 70
        else 0

      x' = max (- width / 2) $ min (width / 2) (x + vx' * seconds)


main :: IO ()
main = play window background fps initialState render handleKeys update
    where
        -- timePassed -> game state -> new state
        update :: Float -> BreakoutGame -> BreakoutGame
        update seconds = blocksBounce . sideBounce . topBounce . paddleBounce . moveBall seconds . paddleMove seconds

