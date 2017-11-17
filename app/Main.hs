module Main(main) where

import Data.List
import Data.Fixed
import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import BreakoutGame
import Collisions
import LayoutDimensions


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
  , score = 0
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
        | leftCollision = game { ballVel = (-vx, vy) , blocks = newBlocks, score = 1 + score game}
        | rightCollision = game { ballVel = (-vx, vy) , blocks = newBlocks, score = 1 + score game}
        | topCollision = game { ballVel = (vx, -vy) , blocks = newBlocks, score = 1 + score game}
        | bottomCollision = game { ballVel = (vx, -vy) , blocks = newBlocks, score = 1 + score game}
        | otherwise = game
   where
     (vx, vy) = ballVel game
     leftCollision = any (id) [leftBlockCollision block (ballLoc game) radius | block <- (blocks game)]
     rightCollision = any (id) [rightBlockCollision block (ballLoc game) radius | block <- (blocks game)]
     topCollision = any (id) [topBlockCollision block (ballLoc game) radius | block <- (blocks game)]
     bottomCollision = any (id) [bottomBlockCollision block (ballLoc game) radius | block <- (blocks game)]
     newBlocks = filter (/=(fromJust collidedBlock)) (blocks game)
     collidedBlock = find (blockCollision game radius) (blocks game)
     blockCollision game radius block = any id $ [leftBlockCollision, rightBlockCollision, topBlockCollision, bottomBlockCollision] <*> [block] <*> [ballLoc game] <*> [radius]

restartGame :: BreakoutGame -> BreakoutGame
restartGame game = if ballOut then game { ballLoc = ballLoc initialState, ballVel = ballVel initialState } else game
        where
	ballOut = bottomCollision (ballLoc game) radius


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
    , scoreText $ score game
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

scoreText :: Integer -> Picture
scoreText i = translate (width / 3) (height / 3) $ scale 0.25 0.25 $ color red $ text $ show i

fps :: Int
fps = 60


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
        update seconds = restartGame . blocksBounce . sideBounce . topBounce . paddleBounce . moveBall seconds . paddleMove seconds

