module Data where

import Linear.V2 (V2(..))

type Name = ()
type Coord = V2 Int

data Tick = Tick

data Cell = CanonCell | EmptyCell | ShotCell | AlienCell | BlockerCell0 | BlockerCell1 | BlockerCell2 | AlienShotCell

data Direction = L | R | D deriving Show

data Level = Level
  { lNext   :: Int
  , lAliens :: [Alien]
  , lSpeed  :: Int
  , lAShotSpeed :: Int
  , lShots  :: Int
  } deriving (Show)

data Alien = Alien 
  { coord :: Coord
  , hits  :: Int
  } deriving (Show)

data Blocker = Blocker
  { bCoord  :: Coord
  , bHealth :: Int
  } deriving (Show)

data Game = Game
  { canon     :: Coord
  , paused    :: Bool
  , over      :: Bool
  , level     :: Level 
  , shots     :: [Coord]
  , alienShots:: [Coord]
  , aliens    :: [Alien]
  , count     :: Int
  , aShotCount:: Int 
  , blockers  :: [Blocker]
  , alienDir  :: Direction
  } deriving (Show)

game ::Level ->  Game 
game l = Game
        { canon     = V2 10 0
        , paused    = False
        , shots     = []
        , alienShots= []
        , over      = False
        , level     = l
        , aliens    = lAliens l
        , count     = 1
        , aShotCount= 1
        , blockers  = getBlockers 5 ++ getBlockers 15 ++ getBlockers 25
        , alienDir  = R
        }

-- Int == x start value of a single bocker
getBlockers ::Int -> [Blocker]
getBlockers x = b1 ++ b2
  where b1 = [Blocker (V2 (x+i) 2) 3| i <- [0..5]]
        b2 = [Blocker (V2 (x+1+i) 3) 3| i <- [0..3]]

--FromX -> nrOf -> offset -> Y -> hits
createAliens:: Int -> Int -> Int -> Int -> Int -> [Alien]
createAliens f n o y h = [Alien (V2 (f+x*o) y) h | x <- [0..n]]

initGame :: IO Game
initGame = do
  let l = head levels
  return $game l

height, width :: Int
height = 15
width = 35

alientLocations::Game -> [Coord]
alientLocations g = map coord $aliens g

--All blocker locations for blockers with given health 
blockerLocations::Game -> Int -> [Coord]
blockerLocations g h = [bCoord x | x <- blockers g, bHealth x == h]

allBlockerLocations::Game -> [Coord]
allBlockerLocations g = map bCoord $blockers g

stopped :: Game -> Bool
stopped g = paused g || over g

--Defined Levels
levels :: [Level]
levels = [Level 
          { lNext   = 1
          , lAliens = createAliens 10 5 2 14 1 
          , lSpeed  = 10
          , lAShotSpeed = 40
          , lShots  = 1
          },
          Level 
          { lNext   = 2
          , lAliens = createAliens 10 5 2 14 1 ++ createAliens 11 5 2 13 1
          , lSpeed  = 9
          , lAShotSpeed = 30
          , lShots  = 1
          },
          Level 
          { lNext   = 3
          , lAliens = createAliens 10 6 2 14 1 ++ createAliens 11 6 2 13 1
          , lSpeed  = 8
          , lAShotSpeed = 25
          , lShots  = 1
          },
          Level 
          { lNext   = 4
          , lAliens = createAliens 10 7 2 14 1 ++ createAliens 11 7 2 13 1
          , lSpeed  = 8
          , lAShotSpeed = 20
          , lShots  = 1
          },
          Level 
          { lNext   = 5
          , lAliens = createAliens 10 5 2 14 1 ++ createAliens 11 5 2 13 1 ++ createAliens 10 5 2 12 1
          , lSpeed  = 7
          , lAShotSpeed = 20
          , lShots  = 1
          } ]
