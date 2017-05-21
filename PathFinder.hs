module PathFinder
( pathFinder
, findAllPaths
, Coord
, getShortestPath
, extractEither
, displayMatrixMapPaths
, TileMap
, TileMapInfo
, tileMapToInfo
) where

import Utils
import Data.List
import qualified Data.Matrix as M
import Data.Char (intToDigit, chr, ord)

type TileMatrix a = M.Matrix a
type TileMap a = [[a]]
type Coord = (Int, Int, Int)

data TileMapInfo a = TileMapInfo { tileMap :: TileMap a
                                 , tileMatrix :: TileMatrix a
                                 , tileStart :: a
                                 , tileEnd :: a
                                 , tileEmpty :: a
                                 , tileFull :: a
                                 , tileStartPos :: Coord
                                 , tileEndPos :: Coord
                                 }


--matches lhData5 in func_solv_prob.hs
getTileMap5 :: TileMap Char
getTileMap5 = [
   "xxxxxxxxxxxxxxxxxxxxxxx",
   "xx...x.xxxx.x...x.xxxxx",
   "xx.x.x.xxxx.x.x.x.x...x",
   "xx.x..........x.....x.x",
   "x..xxx.xxxx.xxxxx.xxx.x",
   "xsx......xx.x...x.xxxox",
   "x...xx.x.xx.x.x.x.xx..x",
   "xxxxxx.x......x.x.xx.xx",
   "xxxxxx.xxxx.xxx.x.x..xx",
   "xxxxxx.xxxx.xxx.....xxx",
   "xxxxxxxxxxxxxxxxxxxxxxx"
 -- 1234567890123456789012
   ]

--TODO: this one is messed up
--matches lhData6 in func_solv_prob.hs
getTileMap6 :: TileMap Char
getTileMap6 = [
   "xxxxxxxxxxxxxxxxxxxxxxx",
   "xx...x.xxxx.x...x.xxxxx",
   "xx.x.x.xxxx.x.x.x.x...x",
   "xx.x..........x.....x.x",
   "x..xxx.xxxx.xxxxx.xxx.x",
   "xsx......xx.xxxxx.xxxox",
   "x...xx.x.xx.xxxxx.xx..x",
   "xxxxxx.x.............xx",
   "xxxxxx.xxxx.xxxxx.xxxxx",
   "xxxxxx.xxxx.xxxxx.xxxxx",
   "xxxxxxxxxxxxxxxxxxxxxxx"
 -- 1234567890123456789012
   ]


findTile :: (Eq a) => a -> TileMap a -> Maybe Coord
findTile destChar tileMap = findTile' destChar tileMap 0

findTile' :: (Eq a) => a -> TileMap a -> Int -> Maybe Coord
findTile' destChar [] _         = Nothing
findTile' destChar (row:rows) y =
   let indexMaybe = elemIndex destChar row
   in case indexMaybe of
      Nothing -> findTile' destChar rows (y+1)
      Just x -> Just (x, y, 0)


findTileEither :: (Eq a) => a -> TileMap a -> Either String Coord
findTileEither destChar tileMap =
   case retOption of
      (Just x) -> (Right x)
      Nothing -> Left "findTile failed"
      where retOption = findTile destChar tileMap

tileMapToInfo :: (Eq a)
              => TileMap a
              -> a -> a -> a -> a
              -> Either String (TileMapInfo a)
tileMapToInfo tMap startTileVal endTileVal emptyTileVal fullTileVal = do
   startPos <- findTileEither startTileVal tMap --Right "No starting position"
   endPos <- findTileEither endTileVal tMap --Right "No end position"
   return $ TileMapInfo tMap (M.fromLists tMap) startTileVal endTileVal emptyTileVal fullTileVal startPos endPos


--startTile = starting position, endTile = ending position,
--emptyTile = empty path, fullTile = occupied


findAllPaths :: (Eq a) => TileMapInfo a -> Either String [Coord]
findAllPaths tileMapInfo@(TileMapInfo tMap tMatrix startTileVal endTileVal emptyTileVal fullTileVal startPos endPos) =
   findAllPaths' tileMapInfo
             (allEmptyTileCoords tMap emptyTileVal)

findAllPaths' :: (Eq a) => TileMapInfo a -> [Coord] -> Either String [Coord]
findAllPaths' (TileMapInfo tMap tMatrix startVal endVal emptyVal fullVal startPos endPos) emptyTileCoords = helper [endPos] 0 1
   where helper coords nextPosIndex currZ =
            if haveFinishCoord coords startPos
            then Right coords
            else if (allEmptyChecked coords emptyTileCoords) -- || (nextPosIndex > 2)
                 then Left "No path"
                 else
                     let (nextPos@(nextPosX, nextPosY, nextPosZ)) = (coords !! nextPosIndex)
                         adjacent = findAdjacent tMatrix
                                                 nextPos
                                                 [emptyVal, startVal] --, endVal]
                                                 fullVal
                         newCoords = coords ++ adjacent
                         isGoodWeight coord@(x, y, z) = --TODO: z>= z_ then 6th step in getTileMap1 is bad. if z>z_ then getTileMap3 fails
                           foldr (\(x_, y_, z_) acc -> if (x == x_ && y == y_ && z >= z_) then False else acc) --z >= z_
                                 True
                                 (delete coord coords) --newCoords) --coords) --newCoords)
                         goodCoords = (filter isGoodWeight newCoords)
                     --in helper goodCoords (nextPosIndex+1)
                     in if length goodCoords == length coords && (not $ haveFinishCoord goodCoords startPos) && nextPosZ > currZ
                        then Left $ "No new coords found length coords: " ++ (show $ length goodCoords) ++ " old z: " ++
                                     (show $ currZ) ++ " next z:" ++ (show $ nextPosZ)
                        else helper goodCoords (nextPosIndex+1) nextPosZ

                     --in if null goodCoords then Right "No adjacent movable" else helper goodCoords (nextPosIndex+1)


haveFinishCoord coords endCoord@(x,y,_) =
   foldr (\(x_,y_,_) acc -> if x_ == x && y_ == y then True else acc)
         False
         coords

--can do this based on number of empty and checked coords, don't need to check each one
--allEmptyChecked checkedCoords emptyCoords = length checkedCoords >= length emptyCoords

allEmptyChecked checkedCoords emptyCoords =
   let isCoordInChecked coord@(x, y, _) =
         foldr (\(x_, y_, _) acc -> if x_ == x && y_ == y then True else acc)
               False
               checkedCoords
       leftOverCoords = filter (not . isCoordInChecked) emptyCoords
   in length leftOverCoords == 0


allEmptyTileCoords :: (Eq a) => TileMap a -> a -> [Coord]
allEmptyTileCoords tileMap emptyTileVal =
   foldr (\(row, y) acc -> (getEmptyRowElems y row) ++ acc) [] (zip tileMap [0..])
      where getEmptyRowElems y row =
               foldr (\(elem, x) acc -> if elem == emptyTileVal then (x, y, 0) : acc else acc)
                     []
                     (zip row [0..])

findAdjacent :: (Eq a) => TileMatrix a -> Coord -> [a] -> a -> [Coord]
findAdjacent tileMap adjacentTo@(x, y, z) nonFullTile fullTile =
   let --topLeft = (x-1, y-1, z+1)
       midLeft = (x-1, y,   z+1)
       --botLeft = (x-1, y+1, z+1)
       topMid  = (x,   y-1, z+1)
       botMid  = (x,   y+1, z+1)
       --topRight= (x+1, y-1, z+1)
       midRight= (x+1, y,   z+1)
       --botRight= (x+1, y+1, z+1)
   in filter (not . isBad) [midLeft, topMid, botMid, midRight] --, botLeft, topLeft, topRight, botRight]
      where isBad (x, y, _) =
               let tileContents = M.getElem (y+1) (x+1) tileMap
               in (x < 0 || y < 0 || x > (M.ncols tileMap - 1) || y > (M.ncols tileMap - 1) ||
                  (tileContents == fullTile) || (not $ tileContents `elem` nonFullTile))

--getShortestPath returns shortest path
--pathStepList is a result from findAllPaths. this contains every possible step

{-|
getShortestPath [] = []
getShortestPath pathStepList@(x:xs) =
   let maxStep@(_, _, maxStepZ) =
         foldr (\currStep@(x, y, currZ) accStep@(_, _, accZ) -> if currZ > accZ then currStep else accStep)
               x
               xs
   in maxStep : getShortestPath (filter (\(_, _, z_) -> z_ /= maxStepZ) xs)
-}

getShortestPath :: [Coord] -> Either String [Coord]
getShortestPath pathStepList = getShortestPath' [] pathStepList

getShortestPath' [] [] = Left "getShortestPath error" --before was []
getShortestPath' [] pathStepList@(x:xs) =
   let maxStep@(_, _, maxStepZ) =
         foldr (\currStep@(x, y, currZ) accStep@(_, _, accZ)
                  -> if currZ > accZ then currStep else accStep)
               x
               xs
   in getShortestPath' [maxStep]
         $ filter (\(_, _, z_) -> z_ /= maxStepZ) pathStepList

getShortestPath' accSteps@(accPrev:accRest) [] = Right accSteps
getShortestPath' accSteps@((accPrev@(prevX,prevY,prevZ)):accRest) pathStepList =
   let currStep@(_, _, currZ) =
         foldr (\foldStep@(foldX, foldY, foldZ) foldAcc ->
                  if foldZ+1 == prevZ && (abs $ foldX-prevX) <= 1 && (abs $ foldY-prevY) <= 1
                  then foldStep
                  else foldAcc)
               accPrev
               pathStepList
   in if currZ <= 0
      then Right accSteps
      else getShortestPath' (currStep : accSteps)
            $ filter (\(_,_,z_) -> z_ < currZ) pathStepList


------



--Matrix with character of chr'd numbers for paths (to get step number, use ord (M.getElem x y map) (ord z) - 300)
getMatrixMapChrPaths tileMap goodPath =
   let matrixMap = M.fromLists tileMap
   in helper' matrixMap goodPath
      where helper' mMap [] = mMap
            helper' mMap ((y,x,z):xs) = helper' (M.setElem (chr $ 300+z) (x+1, y+1) mMap) xs


kkDigit x = if x > 9 then '0' else intToDigit x

--Matrix with character digits of numbers for path
displayMatrixMapPaths :: TileMap Char
                      -> [Coord]
                      -> Either String (TileMatrix Char)
displayMatrixMapPaths tileMap goodPath =
   let matrixMap = M.fromLists tileMap
   in Right $ helper' matrixMap goodPath
      where helper' mMap [] = mMap
            helper' mMap ((y,x,z):xs) = helper' (M.setElem (kkDigit z) (x+1, y+1) mMap) xs

pathFinder :: (Eq a) => [[a]] -> a -> a -> a -> a -> Either String [(Int, Int, Int)]
pathFinder tMap startElement endElement emptyElement fullElement =
   let allPathsSteps = tileMapToInfo tMap startElement endElement emptyElement fullElement
                       >>= findAllPaths
   in helper' allPathsSteps
      where helper' (Left x) = Left x
            helper' (Right x) = getShortestPath x


