{-# LANGUAGE OverloadedStrings #-}
module UI where

import Data
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V
import Linear.V2 (V2(..))
import Brick
  ( AttrMap,  Widget
  , vBox, hBox, withBorderStyle, str
  , attrMap, withAttr, AttrName, fg, on
  , emptyWidget, padRight, padTop, padAll, padBottom 
  , hLimit, (<+>), Padding(..), (<=>)
  )

drawUI :: Game -> [Widget Name]
drawUI g = [ C.center $ padRight (Pad 2) (drawStats g) <+> drawGrid g ]

drawStats :: Game -> Widget Name
drawStats g = hLimit 18
  $ vBox [ drawGameOver (over g)
         , drawLevel (lNext $level g)
         , drawInfo
         ]

drawLevel :: Int -> Widget Name
drawLevel n = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str " Level ")
  $ C.hCenter
  $ padAll 1
  $ str $ show n

drawInfo :: Widget Name
drawInfo = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str " Info ")
  $ C.hCenter
  $ padAll 1
  $ str "Fire:    space" <=> str "Pause:   p    " <=> str "Restart: r    "

drawGameOver :: Bool -> Widget Name
drawGameOver o = if o
     then withAttr gameOverAttr $ padTop (Pad 2) $ padBottom (Pad 2) $ C.hCenter $ str "GAME OVER"
     else emptyWidget

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
      | c == canon g               = CanonCell
      | c `elem` shots g           = ShotCell
      | c `elem` alientLocations g = AlienCell
      | c `elem` blockers g        = BlockerCell
      | otherwise                  = EmptyCell

drawCell :: Cell -> Widget Name
drawCell CanonCell   = withAttr canonAttr $str "▄▲▄" <=> str "▀▀▀"
drawCell ShotCell    = withAttr shotAttr $str " ▲ " <=> str " ║ "
drawCell AlienCell   = withAttr alienAttr $str "@§@" <=> str "/\"\\"
drawCell BlockerCell = withAttr blockerAttr $str "   " <=> str "   "
drawCell EmptyCell   = withAttr emptyAttr $str "   " <=> str "   "

attributeMap :: AttrMap
attributeMap = attrMap V.defAttr
  [ (canonAttr, fg V.blue `V.withStyle` V.bold),
    (shotAttr, fg V.red `V.withStyle` V.bold),
    (alienAttr, fg V.green `V.withStyle` V.bold),
    (gameOverAttr, fg V.red `V.withStyle` V.bold),
    (blockerAttr, V.green `on` V.green)
  ]

canonAttr, shotAttr, emptyAttr, gameOverAttr, alienAttr, blockerAttr :: AttrName
canonAttr = "canonAttr"
shotAttr  = "shotAttr"
emptyAttr = "emptyAttr"
alienAttr = "alienAttr"
blockerAttr = "blockerAttr"
gameOverAttr = "gameOver"