module Main(main) where

import Text.Printf (printf)

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact ( Event(..) )
import Graphics.Gloss.Interface.Environment ( getScreenSize )
-- import qualified Graphics.Gloss.Data.Point.Arithmetic as V
-- import Graphics.Gloss.Data.Vector

data World = World {
  elapedTime :: Float,
  lastEvent :: [String]
}

initWorld :: World
initWorld = World {
  elapedTime = 0,
  lastEvent = [""]
}

multilineText :: [String] -> Picture
multilineText texts = Pictures [
  translate 0 (-l*150) $ Text s | (s, l) <- zip texts [1..]]

render :: World -> Picture
render world = Pictures [
  color white timeText,
  color white eventText
  ] where
    timeText = translate (-200) 200 $ scale 0.2 0.2 $ translate 0 (-150) $ Text (printf "%.3f" $ elapedTime world)
    eventText = translate (-200) 0 $ scale 0.2 0.2 $ translate 0 (-150) $ multilineText (lastEvent world)

showEvent :: Event -> [String]
showEvent (EventKey key state modifier pos) = 
  ["EventKey", show key, show state, show modifier, show pos]
showEvent e = [show e]

eventHandle :: Event -> World -> World
eventHandle event world = world {lastEvent = showEvent event}

stepHandle :: Float -> World -> World
stepHandle dt world = world {elapedTime = elapedTime world+dt}

main :: IO ()
main = do
  (screenW, screenH) <- getScreenSize
  let 
    window = InWindow "Gloss Test" (512, 512) ((screenW - 512) `div` 2, (screenH - 512) `div` 2)
    fps = 30
    background = black
  play window background fps initWorld render eventHandle stepHandle
