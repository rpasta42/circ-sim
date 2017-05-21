module Utils (
   extractEither
 , extractJust
 , matrixReplace
 , matrixFilter
 , ShapeCoord
 , TileCoord2
 , TileCoord3
 , TileMap
 , TileMatrix
) where

import qualified Data.Matrix as M


-- # generic stuff

extractEither (Left y) = error y
extractEither (Right y) = y

extractJust (Just y) = y

-- # Path finder
type TileCoord2 = (Int, Int)
type TileCoord3 = (Int, Int, Int)

type TileMap a = [[a]]
type TileMatrix a = M.Matrix a


-- # stuff for matrix


matrixFilter :: M.Matrix a -> (a -> Bool) -> [(Int, Int)]
matrixFilter m pred = reverse $ matrixFilter' m pred 1 1 []

matrixFilter' m pred x y acc
   | x > M.nrows m = matrixFilter' m pred 1 (y+1) acc
   | y > M.ncols m = acc
   | otherwise =
      let elem = M.getElem x y m
          goodElem = pred elem
          newAcc = if goodElem
                   then (y,x) : acc
                   else acc
      in matrixFilter' m pred (x+1) y newAcc


--Matrix Replace: replace part of matrix with another

--stores top left and bottom right corners
type ShapeCoord = (Int, Int, Int, Int)

matrixReplace :: M.Matrix a -> M.Matrix a -> ShapeCoord -> M.Matrix a
matrixReplace drawGrid drawElem drawCoordGrid =
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



-- ## tests:


-- # matrixReplace

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

m3 = matrixReplace m1 m2 (1, 1, 3, 2)
m4 = matrixReplace m1 m2 (4, 2, 6, 4)


-- # matrixFilter

filterMatrixLst =
   [ [30, 4, 5]
   , [4, 12, 2]
   , [21, 9, 5]
   , [2, 3, 15]
   ]

filterMatrix = M.fromLists filterMatrixLst

filteredM1 = matrixFilter filterMatrix
                         (\x -> x == 2)
--[(3,2), (1,4)]


filteredM2 = matrixFilter filterMatrix
                          (\x -> x > 5)
--[(3,4), (2,3), (


