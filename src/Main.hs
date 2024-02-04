module Main(main) where

import Text.Printf (printf)
import System.Random

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact ( 
  Event(..), Key(..) , SpecialKey(..), KeyState(..))
import Graphics.Gloss.Interface.Environment ( getScreenSize )
-- import qualified Graphics.Gloss.Data.Point.Arithmetic as V
-- import Graphics.Gloss.Data.Vector

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

infixl 7 |*
(|*) :: IntVec -> IntVec -> IntVec
(|*) = opIntVec (*)

infixl 7 |%
(|%) :: IntVec -> IntVec -> IntVec
(|%) = opIntVec mod


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

data World = World {
  stepTimeCur :: Float,
  stepTime :: Float,
  snake :: [IntVec],
  direction :: Direction,
  prevDirection :: Direction,
  applePos :: IntVec,
  randomGen :: StdGen
} deriving (Eq, Show)


gridSize :: IntVec
gridSize = (15, 15)

gridOffset :: IntVec
gridOffset = ((gx-1) `div` (-2), (gy-1) `div` (-2))
  where (gx, gy) = gridSize

tileSize :: Float
tileSize = 24


snakeSegments :: [(Int, Int)] -> Picture
snakeSegments segs = Pictures (
  uncurry translate (head segsPoints) (circle 0.3) : [
  uncurry translate segPoint $ rectangleWire 0.8 0.8 | segPoint <- segsPoints
  ]) where
    segsPoints = map intVecToPoint segs


genInitWorld :: StdGen -> World
genInitWorld gen = World {
  stepTimeCur = 0,
  stepTime = 0.6,
  snake = s,
  direction = DirRight,
  prevDirection = DirRight,
  applePos = apple,
  randomGen = gen'
} where
  s = [(x,0) | x <- [0,-1,-2,-3]]
  (apple, gen') = genApple s gen

-- add snake bodies as blacklist

genApple :: [IntVec] -> StdGen -> (IntVec, StdGen)
genApple s gen = (available !! index, gen') where 
  (xMn, yMn) = gridOffset
  (xMx, yMx) = gridOffset |+ gridSize |- (1, 1)
  available = [(x, y) |
    x <- [xMn..xMx], y <- [yMn..yMx], (x, y) `notElem` s]
  (index, gen') = uniformR (0, length available - 1) gen


render :: World -> Picture
render world = Pictures [
  color white timeText,
  scale tileSize tileSize $ Pictures [
    color green $ snakeSegments $ snake world,
    color white boarder,
    color red $ uncurry translate (intVecToPoint $ applePos world) $ circleSolid 0.4
    ]
  ] where
    timeText = translate (-250) 250 $ scale 0.2 0.2 $ translate 0 (-150) $ Text (printf "%.3f" $ stepTimeCur world)
    boarder = uncurry rectangleWire gridFloat
      where gridFloat = intVecToPoint gridSize


eventHandle :: Event -> World -> World
eventHandle (EventKey key Down _ _) world = 
  case keyToDirection key of
    Just dir ->
      if prevDirection world /= opposite dir
      then world {direction=dir} else world
    Nothing -> world
eventHandle _ world = world

keyToDirection :: Key -> Maybe Direction
keyToDirection (SpecialKey KeyUp) = Just DirUp
keyToDirection (SpecialKey KeyDown) = Just DirDown
keyToDirection (SpecialKey KeyLeft) = Just DirLeft
keyToDirection (SpecialKey KeyRight) = Just DirRight
keyToDirection _ = Nothing


-- TODO: handle collision

stepHandle :: Float -> World -> World
stepHandle dt world@World{
  stepTimeCur=stc, stepTime=st, snake=s, direction=dir,
  applePos = apple, randomGen = gen}
  | (stc+dt) < st = world {stepTimeCur = stc+dt}
  | otherwise = world {
    stepTimeCur = stc+dt-st, stepTime = if grow then st * 0.95 else st,
    snake = s' , prevDirection = dir,
    applePos = apple', randomGen = gen'} where
      snakeHead = stepSnakeHead dir s
      grow = snakeHead == apple
      s' = snakeHead : (if grow then s else init s)
      (apple', gen') = if grow then genApple s' gen else (apple, gen)

stepSnakeHead :: Direction -> [IntVec] -> IntVec
stepSnakeHead dir s =
  ((head s |+ directionVector dir |- gridOffset) |% gridSize) |+ gridOffset


main :: IO ()
main = do
  (screenW, screenH) <- getScreenSize
  let 
    window = InWindow "Snake" (512, 512) ((screenW - 512) `div` 2, (screenH - 512) `div` 2)
    fps = 60
    background = black
  genRandom <- newStdGen
  let initWorld = genInitWorld genRandom
  play window background fps initWorld render eventHandle stepHandle

