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
myDelete = myDelete4

{-
can do allEmptyChecked by length checkedTileCoords == length emptyTileCoords
problem: if there's no path, allEmptyChecked will return false
-}


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
             isCoordStart = isTileStart tFuncs
             isBad (x_, y_, _) =
                     x_ < 1 || y_ < 1
                  || x_ >= (M.ncols tMatrix) || y_ >= (M.nrows tMatrix)
                  ||  (not $ isCoordEmpty tMatrix (x_, y_)
                  || isCoordStart tMatrix (x_, y_)) --TODO: wait...wtf is this??


--TODO: TileMapData should take a
findAllPaths :: TileMapData Char -> Either TileError [TileCoord3]
findAllPaths tMapData = findAllPaths' tMapData [coord2To3 tEndPos] 0 1
   where tEndPos = tileEndPos $ tileMapInfo tMapData

--TODO: TileMapData should take a
findAllPaths' :: TileMapData Char -> [TileCoord3] -> Int -> Int -> Either TileError [TileCoord3]
findAllPaths' tData coords nextPosIndex currZ =
   let tMapInfo = tileMapInfo tData
       tMatrix = tileMatrix tData
       emptyCoords = emptyTiles tMapInfo
       finishCoord@(finishX, finishY) = tileEndPos tMapInfo
       startCoord@(startX, startY) = tileStartPos tMapInfo
       allEmptyChecked = --length coords == length emptyCoords
         let isTileCoordInChecked coord@(x, y) =
               foldr (\(x_, y_, _) acc -> if acc then True else x_ == x && y_ == y)
                     False
                     coords --shouldnt this be take nextPosIndex coords (wait nvm)
             leftOver = filter (not . isTileCoordInChecked) emptyCoords
         in length leftOver == 0

       haveFinishTileCoord cccoords =
         foldr (\(x, y, _) acc
                  -> if acc
                     then True
                     else (x == startX && y == startY))
               False
               cccoords

       doTheThing =
          let nextCoord@(nextX, nextY, nextZ) = (coords !! nextPosIndex)
              adjacentCoords = findAdjacent tData nextCoord
              newCoords = trace ("adjacent:" ++ show adjacentCoords) $ coords ++ adjacentCoords
              isGoodWeightZ coord@(_, _, z) =
                foldr (\coord_@(_, _, z_) acc
                         -> if coord3Eq coord coord_ && z >= z_ then False else acc)
                      --TODO: (L.delete coord coords)  and z >= z_ or coords and z > z_
                      True
                      (myDelete coord coords)

              goodCoords = filter isGoodWeightZ newCoords
              --goodCoords = filter isGoodWeightZ adjacentCoords ++ coords

          in if (trace (show goodCoords) False)  --(nextPosIndex+1 >= (length goodCoords))
                || ((length goodCoords == length coords
                    && (not $ haveFinishTileCoord goodCoords)
                    && nextZ > currZ))
             then Left $ ("No new coords found"
                         ++ ";\tlength coords: " ++ (show $ length coords)
                         ++ ";\tlength goodCoords: " ++ (show $ length goodCoords)
                         ++ ";\told z: " ++ (show currZ)
                         ++ ";\tnext z: " ++ (show nextZ)
                         ++ "\ncoords: " ++ (show coords)
                         ++ "\ngoodCoords: " ++ (show goodCoords)
                         ++ "\ndisplay coords: \n"
                         ++ (let (Right x) = displayPaths tData coords in L.intercalate "\n" . M.toLists $ x)
                         ++ "\ndisplay good coords: \n"
                         ++ (let (Right x) = displayPaths tData goodCoords in L.intercalate "\n" . M.toLists $ x))

             else findAllPaths' tData goodCoords (nextPosIndex+1) nextZ

   in if haveFinishTileCoord coords
      then Right coords
      else if allEmptyChecked
           then Left $ "no path found"
                       ++ "; length coords: " ++ (show $ length coords)
                       ++ "; coords: " ++ (show coords)

           else if length coords > 80
                then trace "length too long" Right coords
                else doTheThing
           --else doTheThing




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


