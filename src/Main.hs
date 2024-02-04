module Main (main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Environment (getScreenSize)
import Graphics.Gloss.Interface.IO.Interact
  ( Event (..),
    Key (..),
    KeyState (..),
    SpecialKey (..),
  )
import System.Random

-- import qualified Graphics.Gloss.Data.Point.Arithmetic as V
-- import Graphics.Gloss.Data.Vector

-- IntVec
type IntVec = (Int, Int)

opIntVec :: (Int -> Int -> Int) -> IntVec -> IntVec -> IntVec
opIntVec f (ax, ay) (bx, by) = (ax `f` bx, ay `f` by)

intVecToPoint :: IntVec -> Point
intVecToPoint (x, y) = (fromIntegral x, fromIntegral y) :: Point

infixl 6 |+
(|+) :: IntVec -> IntVec -> IntVec
(|+) = opIntVec (+)

infixl 6 |-
(|-) :: IntVec -> IntVec -> IntVec
(|-) = opIntVec (-)

-- infixl 7 |*
-- (|*) :: IntVec -> IntVec -> IntVec
-- (|*) = opIntVec (*)

infixl 7 |/
(|/) :: IntVec -> IntVec -> IntVec
(|/) = opIntVec div

infixl 7 |%
(|%) :: IntVec -> IntVec -> IntVec
(|%) = opIntVec mod

-- data definitions
data Direction = DirUp | DirDown | DirLeft | DirRight deriving (Eq, Show)

directionVector :: Direction -> IntVec
directionVector DirUp = (0, 1)
directionVector DirDown = (0, -1)
directionVector DirLeft = (-1, 0)
directionVector DirRight = (1, 0)

opposite :: Direction -> Direction
opposite DirUp = DirDown
opposite DirDown = DirUp
opposite DirLeft = DirRight
opposite DirRight = DirLeft

data Snake = Snake
  { cells :: [IntVec],
    direction :: Direction
  }
  deriving (Eq, Show)

data GameState = 
  Ready | Running | GameOver deriving (Eq, Show)

data World = World
  { 
    gameState :: GameState,
    stepTimeCur :: Float,
    stepTime :: Float,
    nextDirection :: Direction,
    snake :: Snake,
    applePos :: IntVec,
    randomGen :: StdGen
  }
  deriving (Eq, Show)

-- const values
gridSize :: IntVec
gridSize = (15, 15)

gridOffset :: IntVec
gridOffset = (gridSize |- (1, 1)) |/ (-2, -2)

tileSize :: Float
tileSize = 24

-- world initialization
genInitWorld :: StdGen -> World
genInitWorld gen =
  World{ 
    gameState = Ready,
    stepTimeCur = 0,
    stepTime = 0.6,
    nextDirection = DirRight,
    snake =
      Snake
        { cells = s,
          direction = DirRight
        },
    applePos = apple,
    randomGen = gen'
  }
  where
    s = [(x, 0) | x <- [0, -1, -2, -3]]
    (apple, gen') = genApple s gen

genApple :: [IntVec] -> StdGen -> (IntVec, StdGen)
genApple s gen = (available !! index, gen')
  where
    (xMn, yMn) = gridOffset
    (xMx, yMx) = gridOffset |+ gridSize |- (1, 1)
    available =
      [ (x, y) |
        x <- [xMn .. xMx],
        y <- [yMn .. yMx],
        (x, y) `notElem` s
      ]
    (index, gen') = uniformR (0, length available - 1) gen

-- world rendering
render :: World -> Picture
render world
  | gameState world == Ready = waitScreen
  | gameState world == GameOver = renderGameOver world
  | otherwise = renderWorld world

pressAnyKeyText :: Picture
pressAnyKeyText = 
  color white $ translate (-140) (-230) $ scale 0.3 0.3 $ Text "Press any key"

waitScreen :: Picture
waitScreen = Pictures [
  color white $ translate (-150) 30 $ scale 0.8 0.8 $ Text "Snake", pressAnyKeyText]

renderWorld :: World -> Picture
renderWorld world =
  scale tileSize tileSize $
    Pictures
      [ color green $ snakeSegments $ cells $ snake world,
        color white boarder,
        color red $ uncurry translate (intVecToPoint $ applePos world) $ circleSolid 0.4
      ]
  where
    boarder = uncurry rectangleWire $ intVecToPoint gridSize

renderGameOver :: World -> Picture
renderGameOver world = Pictures [
  renderWorld world, pressAnyKeyText, gameOverText]
  where
    gameOverText = color white $ translate (-175) 195 $ scale 0.5 0.5 $ Text "Game over"

snakeSegments :: [IntVec] -> Picture
snakeSegments segs =
  Pictures
    ( uncurry translate (head segsPoints) (circle 0.3)
        : [ uncurry translate segPoint $ rectangleWire 0.8 0.8 | segPoint <- segsPoints
          ]
    )
  where
    segsPoints = map intVecToPoint segs

-- event handeling
eventHandle :: Event -> World -> World
eventHandle (EventKey key Down _ _) world@World{gameState=Running} =
  case keyToDirection key of
    Just dir ->
      if direction (snake world) /= opposite dir
        then world {nextDirection = dir}
        else world
    Nothing -> world
eventHandle (EventKey _ Down _ _) World{randomGen=gen} =
  (genInitWorld gen) {gameState=Running}
eventHandle _ world = world

keyToDirection :: Key -> Maybe Direction
keyToDirection (SpecialKey KeyUp) = Just DirUp
keyToDirection (SpecialKey KeyDown) = Just DirDown
keyToDirection (SpecialKey KeyLeft) = Just DirLeft
keyToDirection (SpecialKey KeyRight) = Just DirRight
keyToDirection _ = Nothing

-- step handling
-- TODO: handle collision
stepHandle :: Float -> World -> World
stepHandle dt world@World{
    gameState = Running,
    stepTimeCur = stc,
    stepTime = st,
    snake = s,
    nextDirection = dir',
    applePos = apple,
    randomGen = gen
  }
  | (stc + dt) < st = world {stepTimeCur = stc + dt}
  | otherwise =
      world
        { 
          gameState = if isCollided then GameOver else Running,
          stepTimeCur = stc + dt - st,
          stepTime = if isGrow then st * 0.95 else st,
          snake = s',
          applePos = apple',
          randomGen = gen'
        }
  where
    snakeHead = stepSnakeHead dir' s
    isGrow = snakeHead == apple
    s' = (if isGrow then growSnake else stepSnake) dir' s
    isCollided = isCollidedSnake s'
    (apple', gen') = if isGrow then genApple (cells s') gen else (apple, gen)
stepHandle _ world = world

stepSnakeHead :: Direction -> Snake -> IntVec
stepSnakeHead dir' Snake {cells = cs} =
  ((head cs |+ directionVector dir' |- gridOffset) |% gridSize) |+ gridOffset

stepSnake :: Direction -> Snake -> Snake
stepSnake dir' s@Snake {cells = cs} =
  Snake {cells = stepSnakeHead dir' s : init cs, direction = dir'}

growSnake :: Direction -> Snake -> Snake
growSnake dir' s@Snake {cells = cs} =
  Snake {cells = stepSnakeHead dir' s : cs, direction = dir'}

isCollidedSnake :: Snake -> Bool
isCollidedSnake Snake{cells = h:cs} = h `elem` cs
isCollidedSnake Snake{cells = []} = False

-- main program
main :: IO ()
main = do
  screenSize <- getScreenSize
  let windowSize = (512, 512)
  let window = InWindow "Snake" windowSize ((screenSize |- windowSize) |/ (2, 2))
      fps = 60
      background = black
  genRandom <- newStdGen
  let initWorld = genInitWorld genRandom
  play window background fps initWorld render eventHandle stepHandle
