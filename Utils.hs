module Utils (
   mReplace
 , ShapeCoord
) where

import qualified Data.Matrix as M

--stores top left and bottom right corners
type ShapeCoord = (Int, Int, Int, Int)


--Matrix Replace: replace part of matrix with another
mReplace :: M.Matrix a -> M.Matrix a -> ShapeCoord -> M.Matrix a
mReplace drawGrid drawElem drawCoordGrid =
   mReplace' drawGrid drawElem drawCoordGrid (1,1) (0,0)

mReplace' :: M.Matrix a -> M.Matrix a
          -> ShapeCoord
          -> (Int, Int)
          -> (Int, Int)
          -> M.Matrix a
mReplace' drawGrid drawElem
          drawCoordGrid@(gridStartY, gridStartX, gridEndY, gridEndX) --grid shape
          drawCoordElem@(elemX, elemY) --draw element start
          tracker@(currX, currY)
   | currY+gridStartY > gridEndY = drawGrid
   | currX+gridStartX > gridEndX = mReplace' drawGrid drawElem
                                             drawCoordGrid drawCoordElem
                                             (0, currY+1)
   | otherwise =
      let currElem = M.getElem (currX+elemX) (currY+elemY) drawElem
          newGrid = M.setElem currElem (gridStartX+currX, gridStartY+currY) drawGrid
      in mReplace' newGrid drawElem drawCoordGrid drawCoordElem (currX+1, currY)



--tests:

ml1 = [ "aaaaaaaaa"
      , "aaaaaaaaa"
      , "aaaaaaaaa"
      , "aaaaaaaaa"
      ]

ml2 = [ "bbb"
      , "bbb"
      , "bbb"
      ]

m1 = M.fromLists ml1
m2 = M.fromLists ml2

m3 = mReplace m1 m2 (1, 1, 3, 2)
m4 = mReplace m1 m2 (4, 2, 6, 4)


