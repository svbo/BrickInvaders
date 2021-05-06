module Data where

import Linear.V2 (V2(..))

type Name = ()
type Coord = V2 Int

data Tick = Tick
data Cell = CanonCell | EmptyCell | ShotCell | AlienCell

data Alien = Alien 
  { coord :: Coord
  , hits  :: Int
  } deriving (Show)

data Game = Game
  { canon  :: Coord
  , paused :: Bool
  , over   :: Bool
  , level  :: Int 
  , shots  :: [Coord]
  , aliens :: [Alien]
  , count  :: Int
  } deriving (Show)

game :: Game 
game =  Game
        { canon  = V2 10 0
        , paused = False
        , shots  = []
        , over   = False
        , level  = 1
        , aliens = [Alien (V2 10 19) 2]
        , count = 0
        }

initGame :: IO Game
initGame = do
  return game

height, width :: Int
height = 20
width = 50

alientLocations::Game -> [Coord]
alientLocations g = map coord a
            where a = aliens g

stopped :: Game -> Bool
stopped g = paused g || over g