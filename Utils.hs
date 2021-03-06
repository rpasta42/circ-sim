module Utils
( extractEither
, extractJust
, isJust
, matrixReplace
, matrixFilter1
, matrixFilter2
, matrixMap
, ShapeCoord
, TileCoord2
, TileCoord3
, TileMap
, TileMatrix
, listSingletonExtract
, listHasAtLeast1
, CircError
, coord2MaxX
, coord2MaxY
, listEitherToEitherList
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

listEitherToEitherList :: [Either a b] -> Either a [b]
listEitherToEitherList lst = helper' lst []
   where helper' [] acc = Right acc
         helper' ((Right x):xs) acc = helper' xs (x:acc)
         helper' ((Left x):xs) _ = Left x

{-
listEitherToEitherList lst = helper' lst (Right [])
   where helper' [] acc = acc
         helper' _ (Left a) = Left a
         helper' ((Right x):xs) (Right acc) = helper' xs $ Right (x:acc)
         helper' ((Left x):xs) (Right acc) = Left x
-}



extractEither (Left y) = error y
extractEither (Right y) = y

extractJust (Just y) = y

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

-- # Path finder
type TileCoord2 = (Int, Int)
type TileCoord3 = (Int, Int, Int)

type TileMap a = [[a]]
type TileMatrix a = M.Matrix a

coord2MaxY :: [TileCoord2] -> Int
coord2MaxX = fst . foldr1 (\(x,_) acc@(accX,_) -> if x > accX then (x,0) else acc)
coord2MaxX :: [TileCoord2] -> Int
coord2MaxY = snd . foldr1 (\(_,y) acc@(_,accY) -> if y > accY then (0,y) else acc)


-- # stuff for matrix

matrixFilter1 :: M.Matrix a -> (a -> Bool) -> [(Int, Int)]
matrixFilter1 m pred = reverse $ matrixFilter1' m pred 1 1 []

matrixFilter1' m pred x y acc
   | x > M.nrows m = matrixFilter1' m pred 1 (y+1) acc
   | y > M.ncols m = acc
   | otherwise =
      let elem = M.getElem x y m --TODO: backwards
          goodElem = pred elem
          newAcc = if goodElem
                   then (y,x) : acc
                   else acc
      in matrixFilter1' m pred (x+1) y newAcc


matrixFilter2 :: M.Matrix a -> (M.Matrix a -> (Int,Int) -> Bool) -> [(Int, Int)]
matrixFilter2 m pred = reverse $ matrixFilter2' m pred 1 1 []

matrixFilter2' m pred x y acc
   | x > M.ncols m = matrixFilter2' m pred 1 (y+1) acc
   | y > M.nrows m = acc
   | otherwise =
      let goodElem = pred m (x, y)
          newAcc = if goodElem
                   then (x,y) : acc
                   else acc
      in matrixFilter2' m pred (x+1) y newAcc


matrixMap :: M.Matrix a
          -> (a -> TileCoord2 -> M.Matrix a -> M.Matrix b -> b)
          -> b
          -> M.Matrix b
matrixMap m mapFunc defaultVal =
   matrixMap' m mapFunc 1 1
              (M.matrix (M.nrows m)
                        (M.ncols m)
                        (\(x,y) -> defaultVal))

matrixMap' :: M.Matrix a
           -> (a -> TileCoord2 -> M.Matrix a -> M.Matrix b -> b)
           -> Int -> Int
           -> M.Matrix b
           -> M.Matrix b
matrixMap' m f x y accM
   | x > M.ncols m = matrixMap' m f 1 (y+1) accM
   | y > M.nrows m = accM
   | otherwise     =
      let mappedElem = f (M.getElem y x m) (x, y) m accM
          newAccM = M.setElem mappedElem (y, x) accM
      in matrixMap' m f (x+1) y newAccM



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



-- # end stuff for matrix

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
--TODO: add checks that x1<x2, y1<y2, and both are less than total grid size
m5 = matrixReplace m1 m2 (9, 1, 11, 2)

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


