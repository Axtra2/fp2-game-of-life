module Color
    ( Color
    , white
    , black
    , grey
    , backgroundColor
    , fieldBackgroundColor
    ) where

import Linear
import Data.Word

type Color = V4 Word8

white :: Color
white = V4 255 255 255 255

black :: Color
black = V4 0 0 0 255

grey :: Color
grey = V4 127 127 127 255

backgroundColor :: Color
backgroundColor = V4 120 40 51 255

fieldBackgroundColor :: Color
fieldBackgroundColor = V4 0 0 0 30
