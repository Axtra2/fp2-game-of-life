module Game.Widget.Slider
    ( slider
    ) where

import FRP.Yampa

import Game.Widget.Widget
import Game.Rectangle
import Game.Control
import Game.IO
import Color

import Control.Lens.Operators
import Linear.Affine
import Linear


slider :: Double -> Widget () Double
slider initialValue = proc WidgetInput{..} -> do
    let gi@GameInput{..} = wiGameInput

    isHowered <- howerSF -< (wiRegion, giMousePosition)
    isLeftDown <- leftMouseDownSF -< gi

    let sx = topLeft wiRegion ^. _x
    let sw = size wiRegion ^. _x

    rec
        let target = if isHowered && isLeftDown then
                    let mx = giMousePosition ^. _x in
                    (mx - sx) / sw
                else
                    woValue
        woValue <- follow initialValue 1e-12 <<< iPre initialValue -< target

    let handleW = 20
    let handleH = size wiRegion ^. _y
    let handleX = sx + sw * woValue
    let handleY = center wiRegion ^. _y
    let handleRegion = Region (P $ V2 handleX handleY) (V2 handleW handleH)
    let woRectangles = RectangleG wiRegion white Fill
                     : [RectangleG handleRegion grey Fill]

    returnA -< WidgetOutput{..}
