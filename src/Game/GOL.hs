module Game.GOL
    ( Field
    , withDims
    , dims
    , update
    ) where

import Data.Array


type Field = (Array (Int, Int) Bool)

withDims :: (Int, Int) -> Field
withDims (rows,cols) = listArray ((0, 0), (rows-1, cols-1)) [False | _ <- [1..(rows*cols)]]

dims :: Field -> (Int,Int)
dims field =
    let ((minRow, minCol), (maxRow, maxCol)) = bounds field in
    let nRows = maxRow - minRow + 1 in
    let nCols = maxCol - minCol + 1 in
    (nRows,nCols)

update :: Field -> Field
update field =
    array (bounds field)
        [ (i, nextState (field ! i) (countAliveNeighbours i))
        | i <- indices field
        ]
    where
        nextState :: Bool -> Int -> Bool
        nextState currentState nAliveNeighbours =
            if currentState then
                nAliveNeighbours > 1 && nAliveNeighbours < 4
            else
                nAliveNeighbours == 3

        countAliveNeighbours :: (Int,Int) -> Int
        countAliveNeighbours (row,col) =
            length
                [ ()
                | i <- [-1..1]
                , j <- [-1..1]
                , let row' = row + i
                , let col' = col + j
                , inRange (bounds field) (row', col')
                , i /= j || i /= 0
                , field ! (row + i, col + j)
                ]
