module Game.Widget.Widget
    ( Widget
    , WidgetInput(..)
    , WidgetOutput(..)
    ) where

import FRP.Yampa

import Game.IO
import Game.Rectangle


type Widget a b = SF (WidgetInput a) (WidgetOutput b)

data WidgetInput a = WidgetInput
    { wiGameInput :: GameInput
    , wiRegion :: Region
    , wiValue :: a
    }

data WidgetOutput a = WidgetOutput
    { woRectangles :: [RectangleG]
    , woValue :: a
    }
