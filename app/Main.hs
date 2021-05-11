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
handleEvent g (VtyEvent (V.EvKey (V.KChar ' ') [])) = continue $ shoot g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'p') [])) = continue $ pause g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'r') [])) = continue $ restart g
handleEvent g _                                     = continue g

pause :: Game -> Game
pause g = g {paused = not $paused g}

restart :: Game -> Game
restart _ = game $levels!!0

shoot :: Game -> Game
shoot g = if stopped g || length s >= lShots l then g
  else g {shots = n:s }
    where s = shots g
          n = fmap (\(V2 x y)  -> V2 x (y + 1)) canon g 
          l = level g

move :: (Int -> Int) -> Game -> Game
move f g = if stopped g then g
  else g {canon = V2 x $canon g ^._y }
    where x = f(canon g ^._x) `mod` width

step :: Game -> Game
step g = if stopped g then g
  else do 
      let movedShots = map (\v -> (V2 (v ^._x) (v ^._y + 1))) $shots g -- move shots
      let a = handleAliens g movedShots
      let b = handleBlocker g movedShots
      let s = handleShots g movedShots
      let d = handleDirection g a
      let gUpd = g {aliens = a, shots = s, count = nextCount g, blockers = b, alienDir = d}
      levelUp gUpd

levelUp :: Game -> Game
levelUp g = if not (null $aliens g) then g
            else if length levels > n then game $levels!!n
            else g {paused = True}
            where n = lNext $level g

handleAliens :: Game -> [Coord] -> [Alien]
handleAliens g s = do
      let a = map (\(Alien c h) -> if c `elem` s then Alien c (h -1) else Alien c h) $aliens g -- check for hits
      let a1 = [x | x <- a, not (0 == hits x)] -- remove dead aliens 
      if count g > 0 then a1
      else map (moveAlien (alienDir g)) a1

moveAlien :: Direction -> Alien -> Alien
moveAlien R (Alien c h) = Alien (V2 (c ^._x+1) (c ^._y )) h
moveAlien L (Alien c h) = Alien (V2 (c ^._x-1) (c ^._y )) h
moveAlien D (Alien c h) = Alien (V2 (c ^._x) (c ^._y-1)) h

handleBlocker :: Game -> [Coord] -> [Coord]
handleBlocker g s = [x | x <- blockers g, not (x `elem` s)] -- remove blockers which hit

handleShots :: Game ->  [Coord] -> [Coord]
handleShots g s =  do
      let s1 = [x | x <- s, not (x `elem` alientLocations g || x `elem` blockers g)] -- remove shots which hit
      [x | x <- s1, not $(x ^._y) > height] -- remove shots which are out

handleDirection :: Game -> [Alien] -> Direction
handleDirection g a = if count g > 0 then alienDir g
  else do
    let isLeft = any (\(Alien c _) -> c ^._x <= 5) a
    let isRight = any (\(Alien c _) -> c ^._x >= width-5) a
    case alienDir g of
      D -> if isLeft then R else L
      L -> if isLeft then D else L
      R -> if isRight then D else R

nextCount :: Game -> Int
nextCount g = case count g < lSpeed (level g) of
          True  -> count g + 1
          False -> 0
