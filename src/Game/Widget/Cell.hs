module Game.Widget.Cell
    ( Cell
    , cell
    ) where

import FRP.Yampa

import Game.Widget.Widget
import Game.Rectangle
import Game.Control
import Game.IO
import Color


type Cell = Widget Bool (Event ())

cell :: Cell
cell = proc WidgetInput{..} -> do
    let gi@GameInput{..} = wiGameInput

    isHowered <- howerSF -< (wiRegion, giMousePosition)
    isLeftDown <- leftMouseDownSF -< gi
    isRightDown <- rightMouseDownSF -< gi

    let alive = (isHowered && isLeftDown) || (wiValue && not (isHowered && isRightDown))

    let scaleTarget = if alive then 1 else 0
    scale <- follow 0 1e-12 -< scaleTarget

    let woRectangles = RectangleG (scaleAroundCenter scale wiRegion) white Fill
                     : [ RectangleG wiRegion black Outline | isHowered ]

    let woValue = if wiValue /= alive then Event () else NoEvent

    returnA -< WidgetOutput{..}
