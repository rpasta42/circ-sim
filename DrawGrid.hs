module DrawGrid (
   DrawGrid(DrawGridChar, DrawGridInt)
 , gridNumRows
 , gridNumCols
 , newDrawGrid
 , overwriteGrid
 , drawGridToDisplayStr
) where

import Utils
import qualified Data.Matrix as M
import qualified Data.List as L

data DrawGrid = DrawGridChar (M.Matrix Char)
              | DrawGridInt (M.Matrix Int)
                  deriving (Show)

gridNumRows :: DrawGrid -> Int
gridNumRows (DrawGridChar g) = M.nrows g
gridNumRows (DrawGridInt g) = M.nrows g

gridNumCols :: DrawGrid -> Int
gridNumCols (DrawGridChar g) = M.ncols g
gridNumCols (DrawGridInt g) = M.ncols g


newDrawGrid :: Int -> Int -> DrawGrid
--newDrawing x y = DrawGridInt . M.fromLists $ L.replicate x (map (\_->0) [1..y])
newDrawGrid x y = DrawGridChar . M.fromLists
                     $ L.replicate x (map (\_->' ') [1..y])

overwriteGrid :: DrawGrid -> DrawGrid -> ShapeCoord -> DrawGrid
--overwriteGrid drawGrid drawElem drawCoord
overwriteGrid (DrawGridChar drawGrid) (DrawGridChar drawElem) drawCoord =
   DrawGridChar (mReplace drawGrid drawElem drawCoord)

drawGridToDisplayStr (DrawGridChar x) = M.toLists x
--drawGridToDisplayStr (DrawGridInt x) = M.toLists x

