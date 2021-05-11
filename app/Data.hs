module Data where

import Linear.V2 (V2(..))

type Name = ()
type Coord = V2 Int

data Tick = Tick
data Cell = CanonCell | EmptyCell | ShotCell | AlienCell | BlockerCell
data Direction = L | R | D deriving Show

data Level = Level
  { lNext   :: Int
  , lAliens :: [Alien]
  , lSpeed  :: Int
  , lShots  :: Int
  } deriving (Show)

data Alien = Alien 
  { coord :: Coord
  , hits  :: Int
  } deriving (Show)

data Game = Game
  { canon     :: Coord
  , paused    :: Bool
  , over      :: Bool
  , level     :: Level 
  , shots     :: [Coord]
  , aliens    :: [Alien]
  , count     :: Int
  , blockers  :: [Coord]
  , alienDir  :: Direction
  } deriving (Show)

game ::Level ->  Game 
game l = Game
        { canon     = V2 10 0
        , paused    = False
        , shots     = []
        , over      = False
        , level     = l
        , aliens    = lAliens l
        , count     = 1
        , blockers  = getBlockers 5 ++ getBlockers 15 ++ getBlockers 25
        , alienDir  = R
        }

-- Int == x start value of a single bocker
getBlockers ::Int -> [Coord]
getBlockers x = b1
  where b1 = [V2 (x+i) 2| i <- [0..5]]

levels :: [Level]
levels = [Level 
          { lNext   = 1
          , lAliens = createAliens 10 5 2 14 1 
          , lSpeed  = 10
          , lShots  = 1
          },
          Level 
          { lNext   = 2
          , lAliens = createAliens 10 5 2 14 1 ++ createAliens 11 5 2 13 1
          , lSpeed  = 9
          , lShots  = 1
          } ]

--FromX -> nrOf -> offset -> Y -> hits
createAliens:: Int -> Int -> Int -> Int -> Int -> [Alien]
createAliens f n o y h = [Alien (V2 (f+x*o) y) h | x <- [0..n]]


initGame :: IO Game
initGame = do
  let l = levels!!0
  return $game l

height, width :: Int
height = 15
width = 35

alientLocations::Game -> [Coord]
alientLocations g = map coord a
            where a = aliens g

stopped :: Game -> Bool
stopped g = paused g || over g