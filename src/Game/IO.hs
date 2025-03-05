module Game.IO
    ( GameInput(..)
    , GameOutput(..)
    , MousePosition
    , WindowSize
    , defaultGameInput
    , leftMouseDownSF
    , rightMouseDownSF
    , windowRect
    ) where

import FRP.Yampa

import Game.Rectangle

import Linear.Affine
import Linear


type MousePosition = Point V2 Double
type WindowSize = V2 Double

data GameInput = GameInput
    { giWindowClosed :: Event ()
    , giQPressed :: Event ()
    , giSpacePressed :: Event ()
    , giLeftMouseDown :: Event ()
    , giLeftMouseUp :: Event ()
    , giRightMouseDown :: Event ()
    , giRightMouseUp :: Event ()
    , giMousePosition :: MousePosition
    , giWindowSize :: WindowSize
    }

data GameOutput = GameOutput
    { goShouldStop :: Bool
    , goRectangles :: [RectangleG]
    }

defaultGameInput :: GameInput
defaultGameInput = GameInput
    { giWindowClosed = NoEvent
    , giQPressed = NoEvent
    , giSpacePressed = NoEvent
    , giLeftMouseDown = NoEvent
    , giLeftMouseUp = NoEvent
    , giRightMouseDown = NoEvent
    , giRightMouseUp = NoEvent
    , giMousePosition = zero
    , giWindowSize = zero
    }

windowRect :: GameInput -> Region
windowRect GameInput{..} = Region (zero .+^ ((*0.5) <$> giWindowSize)) giWindowSize

leftMouseDownSF :: SF GameInput Bool
leftMouseDownSF = proc GameInput{..} -> do
    let updates = mergeEvents
            [ giLeftMouseDown `tag` (|| True)
            , giLeftMouseUp `tag` (&& False)
            ]
    isDown <- accumHold False -< updates
    returnA -< isDown

rightMouseDownSF :: SF GameInput Bool
rightMouseDownSF = proc GameInput{..} -> do
    let updates = mergeEvents
            [ giRightMouseDown `tag` (|| True)
            , giRightMouseUp `tag` (&& False)
            ]
    isDown <- accumHold False -< updates
    returnA -< isDown
