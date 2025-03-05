{-# LANGUAGE OverloadedStrings #-}

module Game.Driver
    ( gameDriver
    ) where

import FRP.Yampa hiding (event, (^-^))

import qualified SDL
import SDL (($=))

import Game.Rectangle
import Game.Game
import Game.IO
import SDLHelpers
import Color

import Data.Foldable (traverse_)
import Data.IORef


windowConfig :: SDL.WindowConfig
windowConfig =
    let SDL.WindowConfig{..} = SDL.defaultWindow in
    SDL.WindowConfig{SDL.windowResizable = True, ..}

gameDriver :: IO ()
gameDriver = do
    SDL.initializeAll

    window <- SDL.createWindow "Game of Life" windowConfig
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

    SDL.HintRenderVSync $= SDL.EnableVSync
    SDL.rendererDrawBlendMode renderer $= SDL.BlendAlphaBlend

    timeRef <- newIORef 0

    let
        initialize :: IO GameInput
        initialize = do
            _ <- updateTime timeRef
            updateInput window Nothing

        sense :: Bool -> IO (DTime, Maybe GameInput)
        sense _ = do
            dt <- updateTime timeRef
            mbSdlEvent <- SDL.pollEvent
            gi <- updateInput window mbSdlEvent
            pure (dt, Just gi)

        actuate :: Bool -> GameOutput -> IO Bool
        actuate _ GameOutput{..} = do

            _ <- SDL.rendererDrawColor renderer $= backgroundColor
            SDL.clear renderer

            traverse_ (renderRectangleG renderer) goRectangles
            SDL.present renderer

            pure goShouldStop

    reactimate
        initialize
        sense
        actuate
        gameSF

updateInput :: SDL.Window -> Maybe SDL.Event -> IO GameInput
updateInput window mbSdlEvent = do
    let GameInput{..} = defaultGameInput

    mousePosition <- SDL.getAbsoluteMouseLocation
    windowSize <- SDL.get $ SDL.windowSize window
    let giMousePosition = fromIntegral <$> mousePosition
    let giWindowSize = fromIntegral <$> windowSize

    pure $ case mbSdlEvent of
        Just sdlEvent -> GameInput
            { giWindowClosed   = windowClosedEvent sdlEvent
            , giQPressed       = keyDownEvent SDL.KeycodeQ sdlEvent
            , giSpacePressed   = keyDownEvent SDL.KeycodeSpace sdlEvent
            , giLeftMouseDown  = mouseDownEvent SDL.ButtonLeft sdlEvent
            , giLeftMouseUp    = mouseUpEvent SDL.ButtonLeft sdlEvent
            , giRightMouseDown = mouseDownEvent SDL.ButtonRight sdlEvent
            , giRightMouseUp   = mouseUpEvent SDL.ButtonRight sdlEvent
            , ..
            }
        Nothing -> GameInput{..}
