{-# LANGUAGE Arrows #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Game
    ( gameMain
    ) where

import SDL (($=))
import qualified SDL
import qualified SDL.Font

import FRP.Yampa hiding (event)

import Data.IORef
import Linear (V2(..), V4(..))

gameMain :: IO ()
gameMain = do
    SDL.initializeAll
    SDL.Font.initialize

    window <- SDL.createWindow "Game of Life" SDL.defaultWindow
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
    font <- SDL.Font.load "assets/Roboto-Regular.ttf" 32

    timeRef <- newIORef 0

    let
        initialize :: IO GameInput
        initialize = do
            curTime <- SDL.ticks
            writeIORef timeRef curTime

            mousePosition <- SDL.getRelativeMouseLocation
            pure GameInput
                { windowClosed = NoEvent
                , qPressed = NoEvent
                , mousePosition = toInteger <$> mousePosition
                }

        sense :: Bool -> IO (DTime, Maybe GameInput)
        sense _ = do
            curTime <- SDL.ticks
            prevTime <- readIORef timeRef
            writeIORef timeRef curTime
            let dt = curTime - prevTime

            sdlEvent <- SDL.pollEvent
            event <- traverse processEvent sdlEvent

            pure (realToFrac dt, event)

        actuate :: Bool -> GameOutput -> IO Bool
        actuate _ go = do

            _ <- SDL.rendererDrawColor renderer $= V4 120 40 51 255
            SDL.clear renderer

            surface <- SDL.Font.blended font (V4 255 255 255 255) "Hello, World!"
            texture <- SDL.createTextureFromSurface renderer surface
            _ <- SDL.copy renderer texture Nothing Nothing

            SDL.present renderer

            SDL.destroyTexture texture
            SDL.freeSurface surface

            pure $ shouldStop go

        process :: SF GameInput GameOutput
        process = proc gi -> do
            qEvent <- arr qPressed -< gi
            closedEvent <- arr windowClosed -< gi
            let shouldStopEvent = mergeEvents [closedEvent, qEvent]
            shouldStop <- hold False -< shouldStopEvent `tag` True
            returnA -< GameOutput
                { shouldStop
                }

    reactimate
        initialize
        sense
        actuate
        process

processEvent :: SDL.Event -> IO GameInput
processEvent sdlEvent = do
    let windowClosed = filterWindowClosedEvent sdlEvent
        qPressed = filterKeyDownEvent SDL.KeycodeQ sdlEvent

    mousePosition <- SDL.getRelativeMouseLocation
    pure GameInput
        { windowClosed
        , qPressed
        , mousePosition = toInteger <$> mousePosition
        }

filterWindowClosedEvent :: SDL.Event -> Event ()
filterWindowClosedEvent sdlEvent =
    case SDL.eventPayload sdlEvent of
        SDL.WindowClosedEvent _ -> Event ()
        _ -> NoEvent

filterKeyDownEvent :: SDL.Keycode -> SDL.Event -> Event ()
filterKeyDownEvent keycode sdlEvent =
    case SDL.eventPayload sdlEvent of
        SDL.KeyboardEvent ke ->
            if
                SDL.keyboardEventKeyMotion ke == SDL.Pressed &&
                SDL.keysymKeycode (SDL.keyboardEventKeysym ke) == keycode
            then
                Event ()
            else
                NoEvent
        _ -> NoEvent

data GameInput = GameInput
    { windowClosed :: Event ()
    , qPressed :: Event ()
    , mousePosition :: V2 Integer
    }

data GameOutput = GameOutput
    { shouldStop :: Bool
    }
