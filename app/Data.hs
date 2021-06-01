module Data where

import Linear.V2 (V2(..))

type Name = ()
type Coord = V2 Int

data Tick = Tick

data Cell = CanonCell | EmptyCell | ShotCell | AlienCell | UfoCell | BlockerCell0 | BlockerCell1 | BlockerCell2 | AlienShotCell

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
  } deriving (Show)

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
  , paused    :: Bool
  , over      :: Bool
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
game ::Int -> Level ->  Game 
game s l = Game
        { canon     = V2 10 0
        , paused    = False
        , shots     = []
        , alienShots= []
        , over      = False
        , lives     = 3
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

-- | Returns a group of aliens 
-- f: X-Pos of first alien 
-- n: amount of aliens 
-- o: Offset the aliens have to their left neighbor 
-- y: Y-Pos of the alien group
-- h: Amount of hits it takes to kill the alien
createAliens:: Int -> Int -> Int -> Int -> Int -> [Alien]
createAliens f n o y h = [Alien (V2 (f+x*o) y) h | x <- [0..n]]

-- | Returns an ufo
createUfo:: [Ufo]
createUfo = [Ufo (V2 0 height-1) 1]

-- | Starts the game with the first level
initGame :: IO Game
initGame = do
  let l = head levels
  return $game 0 l

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
stopped g = paused g || over g

-- | Level definitions
levels :: [Level]
levels = [Level 
          { lNext   = 1
          , lAliens = createAliens 10 5 2 14 1 
          , lSpeed  = 10
          , lAShotSpeed = 40
          , lUfoSpeed = 7
          , lShots  = 1
          },
          Level 
          { lNext   = 2
          , lAliens = createAliens 10 5 2 14 1 ++ createAliens 11 5 2 13 1
          , lSpeed  = 9
          , lAShotSpeed = 30
          , lUfoSpeed = 7
          , lShots  = 1
          },
          Level 
          { lNext   = 3
          , lAliens = createAliens 10 6 2 14 1 ++ createAliens 11 6 2 13 1
          , lSpeed  = 8
          , lAShotSpeed = 25
          , lUfoSpeed = 7
          , lShots  = 1
          },
          Level 
          { lNext   = 4
          , lAliens = createAliens 10 7 2 14 1 ++ createAliens 11 7 2 13 1
          , lSpeed  = 8
          , lAShotSpeed = 20
          , lUfoSpeed = 7
          , lShots  = 1
          },
          Level 
          { lNext   = 5
          , lAliens = createAliens 10 5 2 14 1 ++ createAliens 11 5 2 13 1 ++ createAliens 10 5 2 12 1
          , lSpeed  = 7
          , lAShotSpeed = 20
          , lUfoSpeed = 7
          , lShots  = 1
          } ]
