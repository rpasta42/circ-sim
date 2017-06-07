module PathFinder2
( findAllPaths
, getShortestPath
, findAdjacent --debug
, displayPaths
, tileMapInitFromMap
, tileMapInitFromMatrix
, TileError
, TileMatrixPred
, TileMatrixFuncs(TileMatrixFuncs,isTileEmpty,isTileStart,isTileEnd)
, TileMapInfo
, TileMapData(tileMap, tileMatrix, tileMatrixFuncs, tileMapInfo)
) where

import Data.Char (intToDigit) --for testing
import Utils
import qualified Data.Matrix as M
import qualified Data.List as L
import qualified Data.Sequence as S
import qualified Data.Foldable as F
--import qualified GHC.Exts as E

import Debug.Trace
type TileError = String
type TileMatrixPred a = (TileMatrix a -> TileCoord2 -> Bool)

debug = True

data TileMatrixFuncs a =
   TileMatrixFuncs { isTileEmpty :: TileMatrixPred a
                   , isTileStart :: TileMatrixPred a
                   , isTileEnd   :: TileMatrixPred a
                   }

instance Show (TileMatrixFuncs a) where
   show _ = "TileMatrixFuncs a"

data TileMapInfo = TileMapInfo { tileStartPos :: TileCoord2
                               , tileEndPos :: TileCoord2
                               , emptyTiles :: [TileCoord2]
                               } deriving (Show)

data TileMapData a =
   TileMapData { tileMap :: TileMap a
               , tileMatrix :: TileMatrix a
               , tileMatrixFuncs :: TileMatrixFuncs a
               , tileMapInfo :: TileMapInfo
               } deriving (Show)

--tileMapToMatrix/tileMatrixToMap/tileMapInitFromMap/tileMapInitFromMatrix
tileMapToMatrix = M.fromLists
tileMatrixToMap = M.toLists

tileMapInitFromMap tileMap = tileMapInit tileMap (tileMapToMatrix tileMap)
tileMapInitFromMatrix tileMatrix = tileMapInit (tileMatrixToMap tileMatrix) tileMatrix


tileMapInit :: TileMap a -> TileMatrix a -> TileMatrixFuncs a
            -> Either TileError (TileMapData a)

tileMapInit tileMap tileMatrix tileFuncs =
   if debug
   then trace "initialized TileMapData"  --("tm data: " ++ (show tmData))
              tmData
   else tmData
      where tmData = tileMapInit' tileMap tileMatrix tileFuncs

tileMapInit' tileMap tileMatrix tileFuncs =
   do tileStartPos <- listSingletonExtract $ matrixFilter2 tileMatrix isStart
      tileEndPos <- listSingletonExtract $ matrixFilter2 tileMatrix isEnd
      emptyTiles <- listHasAtLeast1 $ matrixFilter2 tileMatrix isEmpty
      return $ TileMapData tileMap tileMatrix tileFuncs
                           $ TileMapInfo tileStartPos tileEndPos emptyTiles
         where isEmpty = isTileEmpty tileFuncs
               isStart = isTileStart tileFuncs
               isEnd   = isTileEnd tileFuncs


--filterUnique = L.nub --L.reverse . L.nub . L.reverse

--delete/replace/etc

myDelete1 coord coords =
   let deleted = L.delete coord coords
   in if length deleted == length coords
      then deleted
      else myDelete coord deleted

myDelete2 coord coords = reverse $ helper [] coords
   where helper acc [] = acc
         helper acc (x:xs) =
            let newAcc = if x == coord then acc else x:acc
            in helper newAcc xs

myDelete3 coord coords = coords

myDelete4 coord coords = L.delete coord coords

myDelete :: (Eq a) => a -> [a] -> [a]
myDelete = myDelete2


deleteLstIndex i lst =
   let splitted@(a,b) = splitAt i lst
       (_:secondPartB) = b
   in a ++ secondPartB

replaceLstIndex i lst newItem =
   let splitted@(a,b) = splitAt i lst
       (_:secondPartB) = b
   in a ++ (newItem:b)


findAdjacent :: TileMapData a -> TileCoord3 -> [TileCoord3]
findAdjacent tData adjacentTo@(x, y, z) =
   let midLeft = (x-1, y,   z+1)
       topMid  = (x,   y-1, z+1)
       botMid  = (x,   y+1, z+1)
       midRight= (x+1, y,   z+1)
   in filter (not . isBad) [midLeft, topMid, botMid, midRight]
       where tFuncs = tileMatrixFuncs tData
             tMatrix = tileMatrix tData
             isCoordEmpty = isTileEmpty tFuncs
             isCoordEnd = isTileEnd tFuncs
             isBad (x_, y_, _) =
                     x_ < 1 || y_ < 1
                  || x_ >= (M.ncols tMatrix) || y_ >= (M.nrows tMatrix)
                  ||  (not $ (isCoordEmpty tMatrix (x_, y_) || isCoordEnd tMatrix (x_, y_)))
                  -- || isCoordStart tMatrix (x_, y_)) --TODO: wait...wtf is this??



--TODO: TileMapData should take a
findAllPaths :: TileMapData Char -> Either TileError [TileCoord3]
findAllPaths tMapData = findAllPaths' tMapData [] [coord2To3 tEndPos] --0 1
   where tEndPos = tileStartPos $ tileMapInfo tMapData


findAllPaths' _ checked [] = Left $ "All checked, no good coords" ++ show checked

findAllPaths' tData checked unchecked@(unX:unXs) =
   let tMapInfo = tileMapInfo tData
       --tMatrix = tileMatrix tData
       emptyCoords = emptyTiles tMapInfo
       finishCoord@(finishX, finishY) = tileEndPos tMapInfo
       startCoord@(startX, startY) = tileStartPos tMapInfo

       doTheThing =
         let nextCoord@(nextX, nextY, nextZ) = unX
             newChecked = addCoordToCoords nextCoord checked

             adjacentCoords = findAdjacent tData nextCoord
             goodAdjacent = getGoodUncheckedCoords adjacentCoords newChecked
             newUnchecked' = addCoordsToCoords goodAdjacent unXs
             newUnchecked = sortUncheckedCoords newUnchecked' finishCoord

         in {- trace ("\n\nkk unchecked:" ++ (show newUnchecked) ++ "\nnew checked" ++ (show newChecked)) -}
                  findAllPaths' tData newChecked newUnchecked
   in if {-trace "hi" $ EndComment-} haveFinishCoord finishCoord checked
      then Right checked
      else doTheThing
           --if length checked > 1000
           --then trace "length checked too long: > 1000" $ Right checked
           --else doTheThing


--helpers for findAllPaths'

pow :: (Integral a, Floating a) => a -> a -> a
pow x 0 = x
pow x p
   | p `rem` 2 == 0 = pow (x*x) (p / 2)
   | otherwise = x * pow x (p-1)

pointDistance :: (Floating a) => TileCoord2 -> TileCoord2 -> a
pointDistance (x1, y1) (x2, y2) = sqrt $ fromIntegral s1 + s2
   where s1 = ((fromIntegral x2 - fromIntegral x1) ^ 2)
         s2 = ((fromIntegral y2 - fromIntegral y1) ^ 2)

sortUncheckedCoords :: [TileCoord3] -> TileCoord2 -> [TileCoord3]
--sortUncheckedCoords newUnchecked' tEndPos = newUnchecked'
sortUncheckedCoords unchecked tEndPos@(x,y) = L.sortBy sortFunc unchecked
   where sortFunc :: TileCoord3 -> TileCoord3 -> Ordering
         sortFunc a@(x1, y1, _) b@(x2, y2, _) =
            let distance1 = pointDistance (x1, y1) tEndPos
                distance2 = pointDistance (x2, y2) tEndPos
            in compare distance1 distance2




addCoordToCoords :: TileCoord3 -> [TileCoord3] -> [TileCoord3]
addCoordToCoords coord@(x,y,z) coords =
   let checkedIndexMaybe = L.foldl' (\acc (coord_, index)
                                       -> if isJust acc
                                          then acc
                                          else if coord3Eq coord coord_
                                               then (Just (coord_, index))
                                               else Nothing)
                                    Nothing
                                    (zip coords [0..])
   in case checkedIndexMaybe of
         Nothing -> coords ++ [coord]
         (Just (coord_@(_, _, z_), index)) ->
            if z < z_
            then replaceLstIndex index coords coord
            else coords

addCoordsToCoords :: [TileCoord3] -> [TileCoord3] -> [TileCoord3]
addCoordsToCoords [] coords = coords
addCoordsToCoords (x:xs) coords = addCoordsToCoords xs (addCoordToCoords x coords)

isGoodUncheckedCoord :: TileCoord3 -> [TileCoord3] -> Bool
isGoodUncheckedCoord coord@(x,y,z) checkedCoords =
   L.foldl (\ acc coord_@(_, _, z_) ->
               if not acc
               then False
               else if coord3Eq coord coord_
                     then z < z_
                     else True)
            True
            checkedCoords

getGoodUncheckedCoords :: [TileCoord3] -> [TileCoord3] -> [TileCoord3]
getGoodUncheckedCoords newCoords checkedCoords = helper newCoords []
   where helper [] acc = acc
         helper (x:xs) acc =
            if isGoodUncheckedCoord x checkedCoords
            then helper xs (x:acc)
            else {- trace "not good" $ -} helper xs acc

haveFinishCoord finishCoord@(finishX, finishY) coords =
   L.foldl (\acc (x, y, _)
               -> if acc
                  then True
                  else (x == finishX && y == finishY))
           False
           coords

allEmptyChecked coords emptyCoords = length coords == length emptyCoords
   {-let isCoordInChecked coord@(x,y) =
         L.foldl' (\acc (x_, y_, _) -> if acc then True else x_ == x && y_ == y)
                  False
                  coords
       leftOver = filter (not . isCoordInChecked) emptyCoords
   in length leftOver == 0-}



getShortestPath :: [TileCoord3] -> Either TileError [TileCoord3]
getShortestPath pathStepList@(_:_) = helper' [] pathStepList
   where helper' [] [] = Left "getShortestPath error"
         helper' [] pathStepList =
            let maxCoord@(_, _, maxCoordZ) = maxCoordByZ pathStepList
            in helper' [maxCoord]
                  $ filter (\(_, _, z_) -> z_ /= maxCoordZ) pathStepList
         helper' accSteps@(_:_) [] = Right accSteps
         helper' accSteps@((accPrev@(prevX,prevY,prevZ)):accRest) pathStepList =
            let currStep@(_, _, currZ) =
                  foldr (\foldStep@(foldX, foldY, foldZ) foldAcc ->
                           if foldZ+1 == prevZ && (abs $ foldX-prevX) <= 1 && (abs $ foldY-prevY) <= 1
                           then foldStep
                           else foldAcc)
                        accPrev
                        pathStepList
            in if currZ <= 0
               then Right accSteps
               else helper' (currStep : accSteps)
                     $ filter (\(_,_,z_) -> z_ < currZ) pathStepList



kkDigit x = if x > 9 then kkDigit $ x-10 else intToDigit x

displayPaths :: TileMapData Char
             -> [TileCoord3]
             -> Either String (TileMatrix Char)
displayPaths tData goodPath =
   let tMatrix = tileMatrix tData
       tMap = tileMap tData
   in Right $ helper' tMatrix goodPath
      where helper' mMap [] = mMap
            helper' mMap ((y,x,z):xs) = helper' (M.setElem (kkDigit z) (x, y) mMap) xs



--coord3Eq/coord2To3/maxCoordByZ

--compares x,y and excludes z
coord3Eq :: TileCoord3 -> TileCoord3 -> Bool
coord3Eq (x1, y1, _) (x2, y2, _) = x1 == x2 && y1 == y2

coord2To3 :: TileCoord2 -> TileCoord3
coord2To3 (x, y) = (x, y, 0)

maxCoordByZ :: [TileCoord3] -> TileCoord3
maxCoordByZ = foldr (\ coord@(x,y,z) acc@(_, _, accZ)
                        -> if z > accZ then coord else acc)
                    (0, 0, -1000) --TODO


