module Game.Control
    ( follow
    , howerSF
    , repeatedlySF
    ) where

import FRP.Yampa
import FRP.Yampa.EventS

import Game.Rectangle
import Game.IO


howerSF :: Rectangle r => SF (r, MousePosition) Bool
howerSF = arr $ uncurry contains

follow :: Double -> Double -> SF Double Double
follow initialValue r = loopPre (initialValue, 0) helper
    where
        helper :: SF (Double, (Double, Time)) (Double, (Double, Time))
        helper = proc (target,(prev,prevT)) -> do
            curT <- time -< ()
            let dt = curT - prevT
            let output =
                    if abs (prev - target) < 1e-3 then
                        target
                    else
                        (prev - target) * (r ** dt) + target
            returnA -< (output, (output, curT))

delayedSF :: a -> SF Time (Event a)
delayedSF a = proc t -> do
    curTime <- time -< ()
    returnA -< if curTime > t then Event a else NoEvent

repeatedlySF :: a -> SF Time (Event a)
repeatedlySF a = recur (delayedSF a >>> once)
