module Main(main) where

import Data.List
import Data.Fixed
import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import Controls
import BreakoutGame
import Collisions
import LayoutDimensions
import Levels

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
  , ballVel = (200, -200)
  , paddle = (gridWidth / 2 - 10, - gridHeight + 2 * borderSize)
  , paddleVel = 0
  , controls = Controls { keyLeft = False, keyRight = False, keyEnter = False }
  , blocks = map gridPosToPos $ head levels
  , level = 1
  , levelsRemaining = tail levels
  , score = 0
  , complete = False
  }

blockColor = green
blockBorderColor = light $ light black

radius = 10

paddleBounce :: BreakoutGame -> BreakoutGame
paddleBounce game = game { ballVel = (vx', vy') }
  where
    (vx, vy) = ballVel game

    (collided, fractDist) = paddleCollision (paddle game) (ballLoc game) radius
    (vx', vy') = if collided then (fractDist * 400 - 200, (1/2 - abs(fractDist -  1/2)) * 100 + 200) else (vx, vy)


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
        | horizontalCollision = game { ballVel = (-vx, vy) , blocks = newBlocks, score = 1 + score game}
        | verticalCollision = game { ballVel = (vx, -vy) , blocks = newBlocks, score = 1 + score game}
        | otherwise = game
   where
     (vx, vy) = ballVel game
     horizontalCollision = any (id) [horizontalBlockCollision block (ballLoc game) radius | block <- (blocks game)]
     verticalCollision = any (id) [verticalBlockCollision block (ballLoc game) radius | block <- (blocks game)]
     newBlocks = filter (/=(fromJust collidedBlock)) (blocks game)
     collidedBlock = find (blockCollision game radius) (blocks game)
     blockCollision game radius block = any id $ [horizontalBlockCollision, verticalBlockCollision] <*> [block] <*> [ballLoc game] <*> [radius]

restartGame :: BreakoutGame -> BreakoutGame
restartGame game = if ballOut then initialState else game
        where
        ballOut = bottomCollision (ballLoc game) radius

levelEnd :: BreakoutGame -> BreakoutGame
levelEnd game
    | null (blocks game) && null (levelsRemaining game) = game { complete = True }
    | null (blocks game) = nextLevel game
    | otherwise = game
    where
    nextLevel game = game
        { level = 1 + level game
        , blocks = map gridPosToPos $ head (levelsRemaining game)
        , levelsRemaining = tail (levelsRemaining game)
        }


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
    , levelText $ level game
    , scoreText $ score game
    ] ++ [ mkBlock blockBorderColor blockColor b | b <- blocks game
    ] ++ completedGameText
    where
        ball = uncurry translate (ballLoc game) $ color ballColor $ circleSolid radius
        ballColor = dark red

        -- borderColour -> colour -> position -> Picture
        mkBlock :: Color -> Color -> (Float, Float) -> Picture
        mkBlock borderCol col (x, y) = pictures
            [ translate x y $ color borderCol $ rectangleSolid paddleWidth paddleHeight
            , translate x y $ color col $ rectangleSolid (paddleWidth - 4) (paddleHeight - 4)
            ]

        completedGameText = if complete game then completeText (score game) else []

levelText :: Integer -> Picture
levelText i = translate (-(width - borderSize) / 2) ((height - borderSize) / 2) $ scale 0.2 0.2 $ color red $ text $ "level " ++ show i

scoreText :: Integer -> Picture
scoreText i = translate ((width - borderSize) / 2) ((height - borderSize) / 2) $ scale 0.2 0.2 $ color red $ text $ show i

completeText :: Integer -> [Picture]
completeText score =
    [ translate (- 100) (borderSize) $ scale 0.4 0.4 $ color yellow $ text "Well Done!"
    , translate (- 100) 0 $ scale 0.2 0.2 $ color yellow $ text $ show score
    , translate (- 100) (- borderSize) $ scale 0.1 0.1 $ color green $ text "Press Enter to restart"
    ]

fps :: Int
fps = 60


blockLeft :: Position -> Float
blockLeft (blockX, blockY) = blockX - blockWidth / 2
blockRight (blockX, blockY) = blockX + blockWidth / 2
blockTop (blockX, blockY) = blockY - blockWidth / 2
blockBottom (blockX, blockY) = blockY - blockWidth / 2


handleKeys :: Event -> BreakoutGame -> BreakoutGame
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) game@(Game {controls = controls})   = game { controls = controls { keyLeft = True } }
handleKeys (EventKey (SpecialKey KeyLeft) Up _ _) game@(Game {controls = controls})     = game { controls = controls { keyLeft = False } }
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) game@(Game {controls = controls})  = game { controls = controls { keyRight = True } }
handleKeys (EventKey (SpecialKey KeyRight) Up _ _) game@(Game {controls = controls})    = game { controls = controls { keyRight = False } }
handleKeys (EventKey (SpecialKey KeyEnter) Down _ _) game@(Game {controls = controls})  = game { controls = controls { keyEnter = True } }
handleKeys (EventKey (SpecialKey KeyEnter) Up _ _) game@(Game {controls = controls})    = game { controls = controls { keyEnter = False } }
handleKeys _ game = game -- Do nothing for all other events.

movePaddle seconds game = game { paddle = (x', y), paddleVel = vx' } where
      c = controls game
      left = keyLeft c
      right = keyRight c
      (x, y) = paddle game
      vx = paddleVel game
      vx' = if left && right then 0
        else if left then max (-1000) $ min (vx - 30) (- 70)
        else if right then min 1000 $ max (vx + 30) 70
        else 0
      x' = max (- width / 2) $ min (width / 2) (x + vx' * seconds)

dismissGameComplete :: BreakoutGame -> BreakoutGame
dismissGameComplete game =
    if (keyEnter (controls game)) then initialState else game

main :: IO ()
main = play window background fps initialState render handleKeys update
    where
        -- timePassed -> game state -> new state
        update :: Float -> BreakoutGame -> BreakoutGame
        update seconds game@(Game { complete = True }) = dismissGameComplete game
        update seconds game = restartGame .
            levelEnd .
            blocksBounce .
            sideBounce .
            topBounce .
            paddleBounce .
            movePaddle seconds .
            moveBall seconds $ game

