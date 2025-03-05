module Game.Game
    ( gameSF
    ) where

import FRP.Yampa

import qualified Game.GOL as GOL
import Game.Widget.Widget
import Game.Widget.Slider
import Game.Widget.Cell
import Game.Control
import Game.Rectangle
import Game.IO
import Color

import Data.Array


initialTPS :: Double
initialTPS = 4

maxTPS :: Double
maxTPS = 60

minTPS :: Double
minTPS = 1

initialField :: GOL.Field
initialField =
    let rows = 12 in
    let cols = 16 in
    GOL.withDims (rows,cols)
        //
        [ ((rows `div` 2 - 1, cols `div` 2    ), True)
        , ((rows `div` 2 - 1, cols `div` 2 + 1), True)
        , ((rows `div` 2    , cols `div` 2 - 1), True)
        , ((rows `div` 2    , cols `div` 2 + 1), True)
        , ((rows `div` 2 + 1, cols `div` 2 + 1), True)

        , ((rows `div` 2 - 1, cols `div` 2 - 4), True)
        , ((rows `div` 2    , cols `div` 2 - 4), True)
        , ((rows `div` 2 + 1, cols `div` 2 - 4), True)
        ]

gameSF :: SF GameInput GameOutput
gameSF = proc gi@GameInput{..} -> do
    let windowRegion = windowRect gi

    let (upperRegion,sliderRegion) = hsplit windowRegion (PixelsFromBottom 30)
    let (nRows,nCols) = GOL.dims initialField
    let fieldRegion = fit upperRegion (fromIntegral nCols / fromIntegral nRows)

    let sliderInput = WidgetInput { wiGameInput = gi, wiRegion = sliderRegion, wiValue = () }
    sliderOutput <- slider ((initialTPS - minTPS) / (maxTPS - minTPS)) -< sliderInput
    let tickTime = 1 / (minTPS + woValue sliderOutput * (maxTPS - minTPS))

    paused <- accumHold False -< giSpacePressed `tag` not
    tick <- repeatedlySF GOL.update -< tickTime

    rec
        cellsOutputs <- par route [cell | _ <- indices initialField] -< (gi, fieldRegion, field)
        let updates = mergeBy (.) (tick `gate` paused) (collectUpdates field cellsOutputs)
        field <- dAccumHold initialField -< updates

    let fieldRectangles =
            [ RectangleG fieldRegion fieldBackgroundColor Fill
            , RectangleG fieldRegion black Outline
            ]
    let cellsRectangles = woRectangles =<< cellsOutputs
    let sliderRectangles = woRectangles sliderOutput

    goShouldStop <- shouldStopSF -< gi
    let goRectangles = fieldRectangles ++ cellsRectangles ++ sliderRectangles

    returnA -< GameOutput{..}

route :: (GameInput, Region, GOL.Field) -> [sf] -> [(WidgetInput Bool, sf)]
route (gi, region, field) sfs =
    let sfsArray = listArray (bounds field) sfs in
    [ (WidgetInput gi cellRegion alive, sfsArray ! i)
    | (i,alive) <- assocs field
    , let cellRegion = gridCell region (GOL.dims field) i
    ]

collectUpdates :: GOL.Field -> [WidgetOutput (Event ())] -> Event (GOL.Field -> GOL.Field)
collectUpdates field co =
    let updates :: [((Int,Int), Bool)]
        updates =
            let eventsArray = listArray (bounds field) (woValue <$> co) in
            [(i, not $ field ! i) | (i, Event ()) <- assocs eventsArray]
    in
    if null updates then
        NoEvent
    else
        Event (// updates)

shouldStopSF :: SF GameInput Bool
shouldStopSF = proc GameInput{..} -> do
    let shouldStopEvent = mergeEvents [giWindowClosed, giQPressed]
    shouldStop <- hold False -< shouldStopEvent `tag` True
    returnA -< shouldStop
