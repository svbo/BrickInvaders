module Main where

import UI
import Data
import GameHandler

import Control.Monad (forever, void)
import Control.Concurrent (threadDelay, forkIO)
import qualified Graphics.Vty as V
import Linear.V2 (V2(..), _x, _y)
import Control.Lens ((^.))
import Brick.BChan (newBChan, writeBChan)
import Brick
  ( App(..), BrickEvent(..), EventM, Next
  , neverShowCursor, customMain
  , continue, halt
  )

-- | Initialize the app
app :: App Game Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const attributeMap
          }

-- | Main entry point, set tickspeed and start the game
main :: IO ()
main = do
  chan <- newBChan 10
  _ <- forkIO $ forever $ do
        writeBChan chan Tick
        threadDelay 100000 -- Tick speed
  g <- initGame
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  void $ customMain initialVty builder (Just chan) app g

-- | Handle gameticks and keyboard inputs
handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (AppEvent Tick)                       = continue $ step g
handleEvent g (VtyEvent (V.EvKey V.KRight []))      = continue $ move (+ 1) g
handleEvent g (VtyEvent (V.EvKey V.KLeft []))       = continue $ move (subtract 1) g
handleEvent g (VtyEvent (V.EvKey V.KEsc []))        = halt g
handleEvent g (VtyEvent (V.EvKey (V.KChar ' ') [])) = continue $ shoot g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'p') [])) = continue $ pause g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'r') [])) = continue $ restart g
handleEvent g _                                     = continue g

-- | Trigger all handlers and update the game
step :: Game -> Game
step g = if stopped g then g
  else do
      let movedShots = map (\v -> V2 (v ^._x) (v ^._y + 1)) $shots g -- move shots
      let movedAlienShots = map (\v -> V2 (v ^._x) (v ^._y - 1)) $alienShots g -- move alien shots
      let a = handleAliens g movedShots -- update aliens
      let b = handleBlocker g movedShots movedAlienShots -- update blockers 
      let s = handleShots g movedShots -- update shots
      let l = handleLives g movedAlienShots -- update lives
      let as = handleAlienShots g movedAlienShots -- update alien shots
      let d = handleDirection g a -- update alien direction
      let u = handleUfo g movedShots -- update ufo
      let sc = handleScore g movedShots -- update score
      let gUpd = g {aliens = a, ufo = u, shots = s, alienShots = as,
            count = nextCount g, aShotCount = nextAShotCount g, ufoCount = nextUfoCount g,
            blockers = b, alienDir = d, score = sc ,lives = l, status = if l == 0 then Lost else status g}
      levelUp gUpd -- update level
