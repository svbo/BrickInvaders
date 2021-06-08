module Data where

import Linear.V2 (V2(..))

type Name = ()
type Coord = V2 Int

data Tick = Tick
data Cell = CanonCell | EmptyCell | ShotCell | AlienCell | UfoCell | BlockerCell0 | BlockerCell1 | BlockerCell2 | AlienShotCell
data Status = Active | Paused | Lost | Won deriving (Show, Eq)
data Direction = L | R | D deriving (Show, Eq)

data Level = Level
  { lNext   :: Int
  , lAliens :: [Alien]
  , lSpeed  :: Int
  , lAShotSpeed :: Int
  , lUfoSpeed  :: Int
  , lShots  :: Int
  } deriving (Show)

data Alien = Alien 
  { coord :: Coord
  , hits  :: Int
  } deriving (Show, Eq)

data Ufo = Ufo 
  { uCoord :: Coord
  , uHits  :: Int
  } deriving (Show, Eq)

data Blocker = Blocker
  { bCoord  :: Coord
  , bHealth :: Int
  } deriving (Show)

data Game = Game
  { canon     :: Coord
  , status    :: Status
  , lives     :: Int
  , level     :: Level 
  , shots     :: [Coord]
  , alienShots:: [Coord]
  , aliens    :: [Alien]
  , ufo       :: [Ufo]
  , ufoCount  :: Int
  , count     :: Int
  , aShotCount:: Int 
  , blockers  :: [Blocker]
  , alienDir  :: Direction
  , score     :: Int
  } deriving (Show)

-- | Initialize the game with the default values
game ::Int -> Int -> Level ->  Game 
game s li l = Game
        { canon     = V2 10 0
        , status    = Active
        , shots     = []
        , alienShots= []
        , lives     = li
        , level     = l
        , aliens    = lAliens l
        , ufo       = []
        , ufoCount  = 1
        , count     = 1
        , aShotCount= 1
        , blockers  = getBlockers 5 ++ getBlockers 15 ++ getBlockers 25
        , alienDir  = R
        , score     = s
        }

-- | Returns a group of blockers at the given start X-Pos
-- x: start X-Pos of a single bocker
getBlockers ::Int -> [Blocker]
getBlockers x = b1 ++ b2
  where b1 = [Blocker (V2 (x+i) 2) 3| i <- [0..5]]
        b2 = [Blocker (V2 (x+1+i) 3) 3| i <- [0..3]]

-- | Returns an ufo
createUfo:: [Ufo]
createUfo = [Ufo (V2 0 height-1) 1]

-- | Starts the game with the first level
initGame :: IO Game
initGame = do
  let l = generateLevel 0
  return $game 0 3 l

-- | Definition of the game canvas
height, width :: Int
height = 15
width = 35

-- | Return a list of all alien locations
alientLocations::Game -> [Coord]
alientLocations g = map coord $aliens g

-- | Returns a list of all ufo locations
ufoLocations :: Game -> [Coord]
ufoLocations g = map uCoord $ufo g

--- |Returns a List of all blocker locations with given health 
blockerLocations::Game -> Int -> [Coord]
blockerLocations g h = [bCoord x | x <- blockers g, bHealth x == h]

-- |Returns a List of all blocker locations 
allBlockerLocations::Game -> [Coord]
allBlockerLocations g = map bCoord $blockers g

-- | Returns if the game is paused or over
stopped :: Game -> Bool
stopped g = s == Lost || s == Paused || s == Won
  where s = status g

-- | Level definitions
generateLevel:: Int -> Level
generateLevel l = Level 
          { lNext   = l + 1
          , lAliens = createAliensForLevel l
          , lSpeed  = getLevelSpeed l
          , lAShotSpeed = getLevelSpeed l * 4
          , lUfoSpeed =  getLevelSpeed l - 1
          , lShots  = 1 + l `div` 5
          }

-- |Returns a group of Aliens according to the difficulty of the given level
createAliensForLevel:: Int -> [Alien]
createAliensForLevel l
    | l < 5      = createAliens p n 2 14 1 
    | l < 10     = createAliens p n 2 14 1 ++ createAliens (p + 1) n 2 13 2
    | l < 15     = createAliens p n 2 14 1 ++ createAliens (p + 1) n 2 13 2 ++ createAliens p n 2 12 3
    | l < 20     = createAliens p n 2 14 1 ++ createAliens (p + 1) n 2 13 2 ++ createAliens p n 2 12 3  ++ createAliens (p + 1) n 2 11 4
    | otherwise  = createAliens p n 2 14 1 ++ createAliens (p + 1) n 2 13 2 ++ createAliens p n 2 12 3  ++ createAliens (p + 1) n 2 11 4  ++ createAliens p n 2 10 5
    where n = 5 + l - (l `div` 5 * 5)
          p = 15 - n

-- | Returns a group of aliens 
-- f: X-Pos of first alien 
-- n: amount of aliens 
-- o: Offset the aliens have to their left neighbor 
-- y: Y-Pos of the alien group
-- h: Amount of hits it takes to kill the alien
createAliens:: Int -> Int -> Int -> Int -> Int -> [Alien]
createAliens f n o y h = [Alien (V2 (f+x*o) y) h | x <- [0..n]]

-- |Returns a number which is smaller the higher the level
getLevelSpeed:: Int -> Int
getLevelSpeed l
    | l < 2 = 10
    | l < 4 = 9
    | l < 6 = 8
    | l < 8 = 7
    | l < 15 = 6
    | l < 20 = 5
    | otherwise = 4
