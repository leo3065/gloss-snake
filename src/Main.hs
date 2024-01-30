module Main(main) where

import Text.Printf (printf)

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact ( Event(..) )
import Graphics.Gloss.Interface.Environment ( getScreenSize )
-- import qualified Graphics.Gloss.Data.Point.Arithmetic as V
-- import Graphics.Gloss.Data.Vector

type IntVec = (Int, Int)

(|+) :: (Integral a, Integral b) => (a, b) -> (a, b) -> (a, b)
(ax, ay) |+ (bx, by) = (ax+bx, ay+by)

(|-) :: (Integral a, Integral b) => (a, b) -> (a, b) -> (a, b)
(ax, ay) |- (bx, by) = (ax-bx, ay-by)

(|%) :: (Integral a, Integral b) => (a, b) -> (a, b) -> (a, b)
(ax, ay) |% (bx, by) = (ax `mod` bx, ay `mod` by)


data Direction = DirUp | DirDown | DirLeft | DirRight deriving (Eq, Show)

directionVector :: Direction -> IntVec
directionVector DirUp = (0, 1)
directionVector DirDown = (0, -1)
directionVector DirLeft = (-1, 0)
directionVector DirRight = (1, 0)
data World = World {
  stepTimeCur :: Float,
  stepTime :: Float,
  snake :: [IntVec],
  direction :: Direction
} deriving (Eq, Show)


gridSize :: IntVec
gridSize = (15, 15)

gridOffset :: IntVec
gridOffset = ((gx-1) `div` (-2), (gy-1) `div` (-2))
  where (gx, gy) = gridSize

tileSize :: Float
tileSize = 24


intVecToPoint :: IntVec -> Point
intVecToPoint (x, y) = (fromIntegral x, fromIntegral y) :: Point 

snakeSegments :: [(Int, Int)] -> Picture
snakeSegments segs = Pictures (
  uncurry translate (head segsPoints) (circle 0.3) : [
  uncurry translate segPoint $ rectangleWire 0.8 0.8 | segPoint <- segsPoints
  ]) where
    segsPoints = map intVecToPoint segs


initWorld :: World
initWorld = World {
  stepTimeCur = 0,
  stepTime = 0.8,
  snake = [(x,0) | x <- [0,-1,-2,-3]],
  direction = DirRight
}

render :: World -> Picture
render world = Pictures [
  color white timeText,
  scale tileSize tileSize $ Pictures [
    color green $ snakeSegments $ snake world,
    color white boarder
    ]
  ] where
    timeText = translate (-250) 250 $ scale 0.2 0.2 $ translate 0 (-150) $ Text (printf "%.3f" $ stepTimeCur world)
    boarder = uncurry rectangleWire gridFloat
      where gridFloat = intVecToPoint gridSize


eventHandle :: Event -> World -> World
eventHandle _event world = world {snake = snake world}


stepHandle :: Float -> World -> World
stepHandle dt world@World{
  stepTimeCur = st', stepTime = st, snake = snak, direction=dir}
  | (st'+dt) < st = world {stepTimeCur = st'+dt}
  | otherwise = world {
    stepTimeCur = st'+dt-st, snake = stepSnake dir snak}


stepSnake :: Direction -> [IntVec] -> [IntVec]
stepSnake dir snak =
  ((head snak |+ directionVector dir |- gridOffset) |% gridSize) |+ gridOffset
  : init snak


main :: IO ()
main = do
  (screenW, screenH) <- getScreenSize
  let 
    window = InWindow "Snake" (512, 512) ((screenW - 512) `div` 2, (screenH - 512) `div` 2)
    fps = 30
    background = black
  play window background fps initWorld render eventHandle stepHandle