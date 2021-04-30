module Data where

import Linear.V2 (V2(..))

type Name = ()
type Coord = V2 Int
data Tick = Tick
data Cell = CanonCell | EmptyCell | ShotCell | AlienCell

data Game = Game
  { canon  :: Coord
  , dead   :: Bool
  , paused :: Bool
  , over   :: Bool
  , level  :: Int 
  , shots  :: [Coord]
  , aliens :: [Coord]
  , count  :: Int
  } deriving (Show)

initGame :: IO Game
initGame = do
  let g  = Game
        { canon  = V2 10 0
        , dead   = False
        , paused = False
        , shots  = []
        , over   = False
        , level  = 1
        , aliens = [V2 10 19]
        , count = 0
        }
  return g

height, width :: Int
height = 20
width = 50