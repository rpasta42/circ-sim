module DrawGrid (
   DrawGrid(DrawGridChar, DrawGridInt, getCharGrid, getIntGrid)
 , DrawGridInfo(DrawGridInfo, getDrawGridDimensions, getDrawGridPadding)
 , gridNumRows
 , gridNumCols
 , newDrawGrid
 , overwriteGrid
 , drawGridToDisplayStr
 , strToDrawGrid
) where

import Utils
import qualified Data.Matrix as M
import qualified Data.List as L
import qualified Data.Text as T

data DrawGrid = DrawGridChar { getCharGrid :: M.Matrix Char }
              | DrawGridInt { getIntGrid :: M.Matrix Int }
                  deriving (Show)

data DrawGridInfo = DrawGridInfo
   { getDrawGridDimensions :: TileCoord2
   , getDrawGridPadding :: TileCoord2
   }



gridNumRows :: DrawGrid -> Int
gridNumRows (DrawGridChar g) = M.nrows g
gridNumRows (DrawGridInt g) = M.nrows g

gridNumCols :: DrawGrid -> Int
gridNumCols (DrawGridChar g) = M.ncols g
gridNumCols (DrawGridInt g) = M.ncols g


newDrawGrid :: Int -> Int -> DrawGrid
--newDrawing x y = DrawGridInt . M.fromLists $ L.replicate x (map (\_->0) [1..y])
newDrawGrid x y = DrawGridChar . M.fromLists
                     $ L.replicate y (map (\_->' ') [1..x])

overwriteGrid :: DrawGrid -> DrawGrid -> ShapeCoord -> DrawGrid
--overwriteGrid drawGrid drawElem drawCoord
overwriteGrid (DrawGridChar drawGrid) (DrawGridChar drawElem) drawCoord =
   DrawGridChar (matrixReplace drawGrid drawElem drawCoord)

drawGridToDisplayStr (DrawGridChar x) = M.toLists x
--drawGridToDisplayStr (DrawGridInt x) = M.toLists x


--------- ####


strToDrawGrid :: String -> DrawGrid
strToDrawGrid str =
   let lines = map T.unpack $ T.split (\x -> x == '\n') (T.pack str)
   in DrawGridChar $ M.fromLists lines --newDrawing 10 10



