module Main where

import UI
import Data

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

app :: App Game Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const attributeMap
          }

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

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (AppEvent Tick)                       = continue $ step g
handleEvent g (VtyEvent (V.EvKey V.KRight []))      = continue $ move (+ 1) g
handleEvent g (VtyEvent (V.EvKey V.KLeft []))       = continue $ move (subtract 1) g
handleEvent g (VtyEvent (V.EvKey V.KEsc []))        = halt g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'f') [])) = continue $ shoot g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'p') [])) = continue $ pause g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'r') [])) = continue $ restart g
handleEvent g _                                     = continue g

pause :: Game -> Game
pause g = g {paused = not $paused g}

restart :: Game -> Game
restart _ = game

shoot :: Game -> Game
shoot g = if stopped g then g
  else g {shots = n:s }
    where s = shots g
          n = fmap (\(V2 x y)  -> V2 x (y + 1)) canon g 

move :: (Int -> Int) -> Game -> Game
move f g = if stopped g then g
  else g {canon = V2 x $canon g ^._y }
    where x = f(canon g ^._x) `mod` width

step :: Game -> Game
step g = if stopped g then g
  else do 
      let s = map (\v -> (V2 (v ^._x) (v ^._y + 1))) $shots g --move shots
      let a = killAliens $hitAliens s $moveAliens g
      g { aliens = a, shots = [x | x <- s, not $(x ^._y) > height], count = nextCount $count g }

moveAliens :: Game -> [Alien]
moveAliens g = if count g < 10 then aliens g
    else map (\(Alien c h) -> Alien (V2 (c ^._x) (c ^._y - 1)) h) $aliens g

killAliens :: [Alien] -> [Alien]
killAliens a = [x | x <- a, not (0 == hits x)]

hitAliens :: [Coord] -> [Alien] -> [Alien]
hitAliens s a = map (\(Alien c h) -> if c `elem` s then Alien c (h -1) else Alien c h) a

nextCount :: Int -> Int
nextCount c = case c < 10 of
          True  -> c + 1
          False -> 0
