module GameHandler where

import Data
import Linear.V2 (V2(..), _x, _y)
import Control.Lens ((^.))

-- | Pause the game
pause :: Game -> Game
pause g = case status g  of
      Active -> g {status = Paused}
      Paused -> g {status = Active}
      _ -> g

-- | Restart the game
restart :: Game -> Game
restart _ = game 0 3 (generateLevel 0)

-- | Add new shot from the canon to the game
shoot :: Game -> Game
shoot g = if stopped g || length s >= lShots l then g
  else g {shots = n:s }
    where s = shots g
          n = fmap (\(V2 x y)  -> V2 x (y + 1)) canon g
          l = level g

-- | Move the canon left or right
move :: (Int -> Int) -> Game -> Game
move f g = if stopped g then g
  else g {canon = V2 x $canon g ^._y }
    where x = f(canon g ^._x) `mod` width


-- | Change to next level if all aliens are dead
levelUp :: Game -> Game
levelUp g
  | not (null $aliens g) = g
  | n <= 25 = game (score g) (lives g) (generateLevel n)
  | otherwise = g {status = Won}
  where
      n = lNext $level g

-- | Remove dead aliens and trigger alien movement
handleAliens :: Game -> [Coord] -> [Alien]
handleAliens g s = do
      let a = moveAndKill (aliens g) s
      if count g > 0 then a
      else do
            let a' = map (moveAlien (alienDir g)) a -- moove aliens
            moveAndKill a' s -- check for hits again after mooving aliens
      
moveAndKill :: [Alien] -> [Coord] -> [Alien]
moveAndKill a s = [x | x <- a', 0 /= hits x] -- remove dead aliens 
      where a' = map (\(Alien c h) -> if c `elem` s then Alien c (h -1) else Alien c h) a  -- check for hits

-- | Move aliens according to current direction
moveAlien :: Direction -> Alien -> Alien
moveAlien R (Alien c h) = Alien (V2 (c ^._x+1) (c ^._y )) h
moveAlien L (Alien c h) = Alien (V2 (c ^._x-1) (c ^._y )) h
moveAlien D (Alien c h) = Alien (V2 (c ^._x) (c ^._y-1)) h

-- | update blockers health or remove them if their health is 0 or if they're in the way of an alien
handleBlocker :: Game -> [Coord] -> [Coord] -> [Blocker]
handleBlocker g s as = do
      let b1 = map (\(Blocker c h) -> if c `elem` s || c `elem` as then Blocker c (h -1) else Blocker c h) $blockers g -- check for hits
      let b2 = map (\(Blocker c h) -> if c `elem` alientLocations g then Blocker c 0 else Blocker c h) b1 --check for aliens
      [x |x <- b2, bHealth x /= 0] -- remove blockers which hit

-- | Update the players score (1P. for Aliens / 10P. for UFOs)
handleScore :: Game -> [Coord] -> Int
handleScore g s = score g + length h + length u * 10
      where h = [x | x <- s, x `elem` alientLocations g]
            u = [x | x <- s, x `elem` ufoLocations g]

-- | Remove shots which hit something or are out of level bounds
handleShots :: Game ->  [Coord] -> [Coord]
handleShots g s =  do
      let s1 = [x | x <- s, not (x `elem` alientLocations g || x `elem` allBlockerLocations g || x `elem` ufoLocations g)] -- remove shots which hit
      [x | x <- s1, x ^._y <= height] -- remove shots which are out

-- | Remove alien shots which hit something or are out of level bounds and add new alien shot 
handleAlienShots :: Game ->  [Coord] -> [Coord]
handleAlienShots g s =  do
      let s1 = [x | x <- s, not (x == canon g || x `elem` allBlockerLocations g)] -- remove shots which hit
      let s2 = [x | x <- s1, x ^._y >= 0] -- remove shots which are out
      if aShotCount g > 0 then s2
      else s2 ++ [fmap (\(V2 x y)  -> V2 x (y - 1)) coord (aliens g !! (length (aliens g) `div`2) )] --add new shot

-- | Change alien direction if they get near the level bounds
handleDirection :: Game -> [Alien] -> Direction
handleDirection g a = if count g > 0 then alienDir g
  else do
    let isLeft = any (\(Alien c _) -> c ^._x <= 5) a
    let isRight = any (\(Alien c _) -> c ^._x >= width-5) a
    case alienDir g of
      D -> if isLeft then R else L
      L -> if isLeft then D else L
      R -> if isRight then D else R

-- | Remove dead ufos or ufos which are out of level bounds and add a new ufo when aliens go down
handleUfo :: Game ->  [Coord] -> [Ufo]
handleUfo g s = do
      let a = map (\(Ufo c h) -> if c `elem` s then Ufo c (h -1) else Ufo c h) $ufo g -- check for hits
      let a1 = [x | x <- a, 0 /= uHits x && width > uCoord x ^._x] -- remove dead and outside ufo 

      if alienDir g == D && null (ufo g) then a1 ++ createUfo -- new ufo when aliens go down and no ufo present
      else if ufoCount g > 0 then a1
      else map (\(Ufo c h) -> Ufo (V2 (c ^._x+1) (c ^._y )) h) a1

-- | Update players lives if he's hit by an alien and finish game if aliens are at the bottom
handleLives:: Game -> [Coord] -> Int
handleLives g as
      | a = 0
      | null h = lives g
      | otherwise = lives g - 1
            where h = [x | x <- as, x == canon g] --canon is hit
                  a = any (\c -> c ^._y <= 1) (alientLocations g) --game over when aliens at bottom

-- | Main game tick counter
nextCount :: Game -> Int
nextCount g = if count g < lSpeed (level g) then count g + 1 else 0

-- | Game tick counter for new alien shot
nextAShotCount :: Game -> Int
nextAShotCount g = if aShotCount g < lAShotSpeed (level g) then aShotCount g + 1 else 0

-- | Game tick counter for ufo movement speed
nextUfoCount :: Game -> Int
nextUfoCount g = if ufoCount g < lUfoSpeed (level g) then ufoCount g + 1 else 0
