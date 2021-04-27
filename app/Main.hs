module Main where

import UI

import Control.Monad (forever, void)
import Control.Concurrent (threadDelay, forkIO)
import qualified Graphics.Vty as V
import Linear.V2 (V2(..))
import Brick.BChan (newBChan, writeBChan)
import Brick
  ( App(..), BrickEvent(..), EventM, Next
  , neverShowCursor, customMain
  , continue, halt
  )

app :: App Game Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

main :: IO ()
main = do
  chan <- newBChan 10
  _ <- forkIO $ forever $ do
        writeBChan chan Tick
        threadDelay 100000 -- decides how fast your game moves
  g <- initGame
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  void $ customMain initialVty builder (Just chan) app g


initGame :: IO Game
initGame = do
  let g  = Game
        { canon  = V2 10 3
        , dead   = False
        , paused = False
        , shots = []
        }
  return g

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (AppEvent Tick)                       = continue $ step g
handleEvent g (VtyEvent (V.EvKey V.KRight []))      = continue $ moveRight g
handleEvent g (VtyEvent (V.EvKey V.KLeft []))       = continue $ moveLeft g
handleEvent g (VtyEvent (V.EvKey V.KEsc []))        = halt g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'f') [])) = continue $ shoot g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'p') [])) = continue $ pause g
handleEvent g _                                     = continue g

pause :: Game -> Game
pause g = g {paused = not $paused g}

shoot :: Game -> Game
shoot g = if paused g then g
  else g {shots = n:s }
    where s = shots g
          (V2 x y) = canon g
          n = V2 x (y + 1)

moveRight ::  Game -> Game
moveRight g = if paused g then g
  else g {canon = V2 xn y }
    where (V2 x y) = canon g
          xn = (x + 1) `mod` width

moveLeft ::  Game -> Game
moveLeft g = if paused g then g
  else g {canon = V2 xn y }
    where (V2 x y) = canon g
          xn = (x - 1) `mod` width

step :: Game -> Game
step g = if paused g then g
  else g { shots = map incy' s}
  where incy' (V2 x y) = (V2 x (y + 2)) 
        s = shots g
