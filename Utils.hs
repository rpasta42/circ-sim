module Utils (
   extractEither
 , extractJust
 , matrixReplace
 , matrixFilter1
 , matrixFilter2
 , ShapeCoord
 , TileCoord2
 , TileCoord3
 , TileMap
 , TileMatrix
 , listSingletonExtract
 , listHasAtLeast1
) where

import qualified Data.Matrix as M


-- # generic stuff

type CircError = String

listSingletonExtract :: [a] -> Either CircError a
listSingletonExtract lst
   | length lst == 1    = Right $ Prelude.head lst
   | otherwise          = Left "list Singleton extraction failed"

listHasAtLeast1 :: [a] -> Either CircError [a]
listHasAtLeast1 lst
   | length lst >= 1    = Right lst
   | otherwise          = Left "listHasAtLeast1Extract: less than 1 in the list"

extractEither (Left y) = error y
extractEither (Right y) = y

extractJust (Just y) = y

-- # Path finder
type TileCoord2 = (Int, Int)
type TileCoord3 = (Int, Int, Int)

type TileMap a = [[a]]
type TileMatrix a = M.Matrix a


-- # stuff for matrix


matrixFilter1 :: M.Matrix a -> (a -> Bool) -> [(Int, Int)]
matrixFilter1 m pred = reverse $ matrixFilter1' m pred 1 1 []

matrixFilter1' m pred x y acc
   | x > M.nrows m = matrixFilter1' m pred 1 (y+1) acc
   | y > M.ncols m = acc
   | otherwise =
      let elem = M.getElem x y m --backwards
          goodElem = pred elem
          newAcc = if goodElem
                   then (y,x) : acc
                   else acc
      in matrixFilter1' m pred (x+1) y newAcc


matrixFilter2 :: M.Matrix a -> (M.Matrix a -> (Int,Int) -> Bool) -> [(Int, Int)]
matrixFilter2 m pred = reverse $ matrixFilter2' m pred 1 1 []

matrixFilter2' m pred x y acc
   | x > M.nrows m = matrixFilter2' m pred 1 (y+1) acc
   | y > M.ncols m = acc
   | otherwise =
      let goodElem = pred m (x, y) --this is reversed
          newAcc = if goodElem
                   then (y,x) : acc
                   else acc
      in matrixFilter2' m pred (x+1) y newAcc



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

-- # matrixReplace test

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


-- # matrixFilter test

filterMatrixLst =
   [ [30, 4, 5]
   , [4, 12, 2]
   , [21, 9, 5]
   , [2, 3, 15]
   ]

filterMatrix = M.fromLists filterMatrixLst

filteredM1 = matrixFilter1 filterMatrix
                         (\x -> x == 2)
--[(1,4), (3,2)]


filteredM2 = matrixFilter1 filterMatrix
                          (\x -> x > 5)
--[(1,1), (1,3), (2,2), (2,3), (3,4)]


