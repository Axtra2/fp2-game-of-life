module Game.Rectangle
    ( Rectangle(..)
    , Region(..)
    , RectangleG(..)
    , RectangleGMode(..)
    , HSplitParam(..)
    , VSplitParam(..)
    , hsplit
    , vsplit
    , contains
    , gridCell
    , fit
    , scaleAroundCenter
    , renderRectangle
    , renderRectangleG
    ) where

import qualified SDL
import SDL (($=))

import Color

import Control.Lens.Operators
import Linear.Affine
import Linear


class Rectangle r where
    center :: r -> Point V2 Double
    size :: r -> V2 Double
    topLeft :: r -> Point V2 Double
    bottomRight :: r -> Point V2 Double
    halfSize :: r -> V2 Double

    center r = topLeft r .+^ halfSize r
    size r = (*2) <$> halfSize r
    topLeft r = center r .-^ halfSize r
    bottomRight r = center r .+^ halfSize r
    halfSize r = (*0.5) <$> size r

contains :: Rectangle r => r -> Point V2 Double -> Bool
contains rect (P (V2 x y)) =
    let tl = topLeft rect in
    let br = bottomRight rect
    in x > tl^._x
    && y > tl^._y
    && x < br^._x
    && y < br^._y

data Region = Region
    { regCenter :: Point V2 Double
    , regSize :: V2 Double
    }

instance Rectangle Region where
    center = regCenter
    size = regSize

fromTopLeft :: Point V2 Double -> V2 Double -> Region
fromTopLeft tl sz = Region (tl .+^ ((*0.5) <$> sz)) sz

scaleAroundCenter :: Double -> Region  -> Region
scaleAroundCenter scale Region{..} = Region regCenter ((*scale) <$> regSize)

gridCell :: Rectangle r => r -> (Int,Int) -> (Int,Int) -> Region
gridCell r (nRows,nCols) (row,col) =
    let (V2 w h) = size r in
    let cellSize@(V2 cellW cellH) = V2 (w / fromIntegral nCols) (h / fromIntegral nRows) in
    let c = topLeft r .+^ V2 (cellW * (0.5 + fromIntegral col)) (cellH * (0.5 + fromIntegral row)) in
    Region c cellSize

fit :: Rectangle r => r -> Double -> Region
fit windowRect aspectRatio =
    let (V2 w h) = size windowRect in
    let windowAspectRatio = w / h in
    if windowAspectRatio > aspectRatio then
        Region (center windowRect) (V2 (aspectRatio * h) h)
    else
        Region (center windowRect) (V2 w (w / aspectRatio))

data VSplitParam = PixelsFromLeft Int
                 | PixelsFromRight Int
                 | RatioFromLeft Double
                 | RatioFromRight Double

data HSplitParam = PixelsFromBottom Int
                 | PixelsFromTop Int
                 | RatioFromBottom Double
                 | RatioFromTop Double

vsplit :: Rectangle r => r -> VSplitParam -> (Region,Region)
vsplit r param = 
    let (P (V2 x y)) = topLeft r in
    let (V2 w h) = size r in
    let w' = case param of
            PixelsFromLeft ps -> fromIntegral ps
            PixelsFromRight ps -> w - fromIntegral ps
            RatioFromLeft ratio -> w * ratio
            RatioFromRight ratio -> w - w * ratio
    in
    ( fromTopLeft (P $ V2 x y) (V2 w' h)
    , fromTopLeft (P $ V2 (x + w') y) (V2 (w - w') h)
    )

hsplit :: Rectangle r => r -> HSplitParam -> (Region,Region)
hsplit r param =
    let (P (V2 x y)) = topLeft r in
    let (V2 w h) = size r in
    let h' = case param of
            PixelsFromBottom ps -> h - fromIntegral ps
            PixelsFromTop ps -> fromIntegral ps
            RatioFromBottom ratio -> h - h * ratio
            RatioFromTop ratio -> h * ratio
    in
    ( fromTopLeft (P $ V2 x y) (V2 w h')
    , fromTopLeft (P $ V2 x (y + h')) (V2 w (h - h'))
    )

data RectangleG = RectangleG
    { rectgRegion :: Region
    , rectgColor :: Color
    , rectgMode :: RectangleGMode
    }

data RectangleGMode = Fill | Outline

instance Rectangle RectangleG where
    center = center . rectgRegion
    size = size . rectgRegion

renderRectangle :: Rectangle r => SDL.Renderer -> r -> Color -> RectangleGMode -> IO ()
renderRectangle renderer r color mode = renderRectangleG renderer $ RectangleG (Region (center r) (size r)) color mode

renderRectangleG :: SDL.Renderer -> RectangleG -> IO ()
renderRectangleG renderer r@RectangleG{..} = do
    SDL.rendererDrawColor renderer $= rectgColor
    let rect = round <$> SDL.Rectangle (topLeft r) (size r)
    case rectgMode of
        Fill -> SDL.fillRect renderer (Just rect)
        Outline -> SDL.drawRect renderer (Just rect)
