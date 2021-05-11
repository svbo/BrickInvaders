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
restart _ = game $head levels

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
      let movedShots = map (\v -> V2 (v ^._x) (v ^._y + 1)) $shots g -- move shots
      let movedAlienShots = map (\v -> V2 (v ^._x) (v ^._y - 1)) $alienShots g -- move Alien shots
      let a = handleAliens g movedShots
      let b = handleBlocker g movedShots movedAlienShots
      let s = handleShots g movedShots
      let as = handleAlienShots g movedAlienShots
      let d = handleDirection g a
      let gUpd = g {aliens = a, shots = s, alienShots = as, count = nextCount g, aShotCount = nextAShotCount g, blockers = b, alienDir = d}
      levelUp gUpd

levelUp :: Game -> Game
levelUp g
  | not (null $aliens g) = g
  | length levels > n = game $ levels !! n
  | otherwise = g {paused = True}
  where
      n = lNext $level g

handleAliens :: Game -> [Coord] -> [Alien]
handleAliens g s = do
      let a = map (\(Alien c h) -> if c `elem` s then Alien c (h -1) else Alien c h) $aliens g -- check for hits
      let a1 = [x | x <- a, 0 /= hits x] -- remove dead aliens 
      if count g > 0 then a1
      else map (moveAlien (alienDir g)) a1

moveAlien :: Direction -> Alien -> Alien
moveAlien R (Alien c h) = Alien (V2 (c ^._x+1) (c ^._y )) h
moveAlien L (Alien c h) = Alien (V2 (c ^._x-1) (c ^._y )) h
moveAlien D (Alien c h) = Alien (V2 (c ^._x) (c ^._y-1)) h

handleBlocker :: Game -> [Coord] -> [Coord] -> [Blocker]
handleBlocker g s as = do
      let b1 = map (\(Blocker c h) -> if c `elem` s || c `elem` as then Blocker c (h -1) else Blocker c h) $blockers g -- check for hits
      [x |x <- b1, bHealth x /= 0] -- remove blockers which hit

handleShots :: Game ->  [Coord] -> [Coord]
handleShots g s =  do
      let s1 = [x | x <- s, not (x `elem` alientLocations g || x `elem` allBlockerLocations g)] -- remove shots which hit
      [x | x <- s1, x ^._y <= height] -- remove shots which are out

handleAlienShots :: Game ->  [Coord] -> [Coord]
handleAlienShots g s =  do
      let s1 = [x | x <- s, not (x == canon g || x `elem` allBlockerLocations g)] -- remove shots which hit
      let s2 = [x | x <- s1, (x ^._y) >= 0] -- remove shots which are out
      if aShotCount g > 0 then s2
      else s2 ++ [fmap (\(V2 x y)  -> V2 x (y - 1)) coord (aliens g !! (length (aliens g) `div`2) )] --add new shot


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
nextCount g = if count g < lSpeed (level g) then count g + 1 else 0

nextAShotCount :: Game -> Int
nextAShotCount g = if aShotCount g < lAShotSpeed (level g) then aShotCount g + 1 else 0
