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
  $ vBox [ drawGameOver $over g
         , drawLives (lives g) (over g)
         , drawLevel $lNext $level g
         , drawInfo
         ]

drawLevel :: Int -> Widget Name
drawLevel n = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str " Level ")
  $ C.hCenter
  $ padAll 1
  $ str $ show n

drawLives :: Int -> Bool -> Widget Name
drawLives n o = if o then emptyWidget else
  withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str " Lives ")
  $ C.hCenter
  $ padAll 1
  $ str $ show n

drawGameOver :: Bool -> Widget Name
drawGameOver o = if o
     then withAttr gameOverAttr $ padTop (Pad 2) $ padBottom (Pad 2) $ C.hCenter $ str "GAME OVER"
     else emptyWidget

drawInfo :: Widget Name
drawInfo = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str " Info ")
  $ C.hCenter
  $ padAll 1
  $ str "Fire:    space" <=> str "Pause:   p    " <=> str "Restart: r    "

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
      | c `elem` alienShots g      = AlienShotCell
      | c `elem` alientLocations g = AlienCell
      | c `elem` blockerLocations g 3 = BlockerCell0
      | c `elem` blockerLocations g 2 = BlockerCell1
      | c `elem` blockerLocations g 1 = BlockerCell2
      | otherwise                  = EmptyCell

drawCell :: Cell -> Widget Name
drawCell CanonCell      = withAttr canonAttr $str "▄▲▄" <=> str "▀▀▀"
drawCell ShotCell       = withAttr shotAttr $str " ▲ " <=> str " ║ "
drawCell AlienShotCell  = withAttr alienShotAttr $str " ↡ " <=> str " ▼ "
drawCell AlienCell      = withAttr alienAttr $str "@§@" <=> str "/\"\\"
drawCell BlockerCell0 = withAttr blockerAttr $str "   " <=> str "   "
drawCell BlockerCell1 = withAttr blockerAttr $str " ▀ " <=> str "  ▄"
drawCell BlockerCell2 = withAttr blockerAttr $str "▄▀ " <=> str "▄▀▄"
drawCell EmptyCell      = withAttr emptyAttr $str "   " <=> str "   "

attributeMap :: AttrMap
attributeMap = attrMap V.defAttr
  [ (canonAttr, fg V.blue `V.withStyle` V.bold),
    (shotAttr, fg V.red `V.withStyle` V.bold),
    (alienShotAttr, fg V.white `V.withStyle` V.bold),
    (alienAttr, fg V.green `V.withStyle` V.bold),
    (gameOverAttr, fg V.red `V.withStyle` V.bold),
    (blockerAttr, V.black `on` V.white)
  ]

canonAttr, shotAttr, emptyAttr, gameOverAttr, alienAttr, blockerAttr, alienShotAttr :: AttrName
canonAttr = "canonAttr"
shotAttr  = "shotAttr"
alienShotAttr = "alienShotAttr"
emptyAttr = "emptyAttr"
alienAttr = "alienAttr"
blockerAttr = "blockerAttr"
gameOverAttr = "gameOver"