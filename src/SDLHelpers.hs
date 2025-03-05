module SDLHelpers
    ( windowClosedEvent
    , keyDownEvent
    , mouseDownEvent
    , mouseUpEvent
    , updateTime
    ) where

import SDL hiding (Event)
import qualified SDL (Event)

import FRP.Yampa
import qualified FRP.Yampa as Yampa (Event(Event, NoEvent))

import Data.IORef

windowClosedEvent :: SDL.Event -> Yampa.Event ()
windowClosedEvent sdlEvent =
    case eventPayload sdlEvent of
        WindowClosedEvent _ -> Yampa.Event ()
        _ -> Yampa.NoEvent

keyDownEvent :: Keycode -> SDL.Event -> Yampa.Event ()
keyDownEvent keycode sdlEvent =
    case eventPayload sdlEvent of
        KeyboardEvent ke ->
            if
                keyboardEventKeyMotion ke == Pressed &&
                keysymKeycode (keyboardEventKeysym ke) == keycode
            then
                Yampa.Event ()
            else
                Yampa.NoEvent
        _ -> Yampa.NoEvent

mouseDownEvent :: MouseButton -> SDL.Event -> Yampa.Event ()
mouseDownEvent mouseButton sdlEvent =
    case eventPayload sdlEvent of
        MouseButtonEvent mbe -> case mouseButtonEventMotion mbe of
            Pressed ->
                if mouseButtonEventButton mbe == mouseButton then
                    Yampa.Event ()
                else
                    Yampa.NoEvent
            _ -> Yampa.NoEvent
        _ -> Yampa.NoEvent

mouseUpEvent :: MouseButton -> SDL.Event -> Yampa.Event ()
mouseUpEvent mouseButton sdlEvent =
    case eventPayload sdlEvent of
        MouseButtonEvent mbe -> case mouseButtonEventMotion mbe of
            Released ->
                if mouseButtonEventButton mbe == mouseButton then
                    Yampa.Event ()
                else
                    Yampa.NoEvent
            _ -> Yampa.NoEvent
        _ -> Yampa.NoEvent


updateTime :: IORef Time -> IO DTime
updateTime timeRef = do
    curTime <- SDL.time
    prevTime <- readIORef timeRef
    writeIORef timeRef curTime
    pure $ curTime - prevTime
