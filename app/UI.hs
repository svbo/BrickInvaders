{-# LANGUAGE OverloadedStrings #-}
module UI where

import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V
import Linear.V2 (V2(..))
import Brick
  ( AttrMap,  Widget
  , vBox, hBox, withBorderStyle, str
  , attrMap, withAttr, AttrName, on
  )

type Name = ()
type Coord = V2 Int
data Tick = Tick
data Cell = CanonCell | EmptyCell | ShotCell


data Game = Game
  { canon  :: Coord
  , dead   :: Bool
  , paused :: Bool
  , shots :: [Coord] --cleanup??
  } deriving (Show)


height, width :: Int
height = 20
width = 20

drawUI :: Game -> [Widget Name]
drawUI g = [C.center $drawGrid g]

drawGrid :: Game -> Widget Name
drawGrid g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Brick Invaders !!!")
  $ vBox rows
  where
    rows         = [hBox $ cellsInRow r | r <- [height-1,height-2..0]]
    cellsInRow y = [drawCoord (V2 x y) | x <- [0..width-1]]
    drawCoord    = drawCell . cellAt g

cellAt :: Game -> Coord -> Cell
cellAt g c
      | c == canon g        = CanonCell
      | c `elem` shots g    = ShotCell
      | otherwise           = EmptyCell

drawCell :: Cell -> Widget Name
drawCell CanonCell = withAttr canonAttr cw
drawCell ShotCell = withAttr shotAttr cw
drawCell EmptyCell = withAttr emptyAttr cw

cw :: Widget Name
cw = str "  "

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (canonAttr, V.blue `on` V.blue),
    (shotAttr, V.red `on` V.red)
  ]

canonAttr, shotAttr, emptyAttr :: AttrName
canonAttr = "canonAttr"
shotAttr  = "shotAttr"
emptyAttr = "emptyAttr"
