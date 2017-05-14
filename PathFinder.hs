module PathFinder
( pathFinder
) where

import Data.List
import qualified Data.Matrix as M
import Data.Char (intToDigit, chr, ord)

type TileMatrix a = M.Matrix a
type TileMap a = [[a]]
type Coord = (Int, Int, Int)


getTileMap1 :: TileMap Char
getTileMap1 = [   --y
   "xxxxxxxxxx", --0
   "x...xx.x.x", --1
   "x.x..x...x", --2
   "xsxx...x.x", --3
   "x.x..x...x", --4
   "x...xx.x.x", --5
   "x.x..x.x.x", --6
   "x.xx...x.x", --7
   "x..o.x...x", --8
   "xxxxxxxxxx"  --9
   ]
----0123456789 x

getTileMap2 :: TileMap Char
getTileMap2 = [   --y
   "xxxxxxxxxx", --0
   "x...xx.x.x", --1
   "x.x..xs..x", --2
   "x.xx...x.x", --3
   "x.x..x...x", --4
   "x...xx.x.x", --5
   "x.x..x.x.x", --6
   "x.xx...x.x", --7
   "x..o.x...x", --8
   "xxxxxxxxxx"  --9
   ]
----0123456789 x

getTileMap3 :: TileMap Char
getTileMap3 = [   --y
   "xxxxxxxxxx", --0
   "x...xx.x.x", --1
   "x.x..xs..x", --2
   "x.xx..xx.x", --3
   "x.x..x...x", --4
   "x...xx.x.x", --5
   "x.x..x.x.x", --6
   "x.xx...x.x", --7
   "x..o.x...x", --8
   "xxxxxxxxxx"  --9
   ]
----0123456789 x

getTileMap4 :: TileMap Char
getTileMap4 = [   --y
   "xxxxxxxxxx", --0
   "x...xx.x.x", --1
   "x.x..xsx.x", --2
   "x.xx..xx.x", --3
   "x.x..x...x", --4
   "x...xx.x.x", --5
   "x.x..x.x.x", --6
   "x.xx...x.x", --7
   "x..o.x...x", --8
   "xxxxxxxxxx"  --9
   ]
----0123456789 x


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




getTileMap = getTileMap6

findTile :: (Eq a) => a -> TileMap a -> Maybe Coord
findTile destChar tileMap = findTile' destChar tileMap 0

findTile' :: (Eq a) => a -> TileMap a -> Int -> Maybe Coord
findTile' destChar [] _         = Nothing
findTile' destChar (row:rows) y =
   let indexMaybe = elemIndex destChar row
   in case indexMaybe of
      Nothing -> findTile' destChar rows (y+1)
      Just x -> Just (x, y, 0)


--startTile = starting position, endTile = ending position,
--emptyTile = empty path, fullTile = occupied
findPath :: (Eq a) => TileMap a -> a -> a -> a -> a -> Either [Coord] String
findPath tileMap startTileVal endTileVal emptyTileVal fullTileVal =
   let startPosMaybe = findTile startTileVal tileMap
       endPosMaybe = findTile endTileVal tileMap
   in case (startPosMaybe, endPosMaybe) of
      (Nothing, _) -> Right "No starting position"
      (_, Nothing) -> Right "No end position"
      (Just startPos, Just endPos) -> findPath' (M.fromLists tileMap)
                                                startPos endPos
                                                startTileVal endTileVal
                                                emptyTileVal fullTileVal
                                                (allEmptyTileCoords tileMap emptyTileVal)

findPath' :: (Eq a) => TileMatrix a -> Coord -> Coord -> a -> a -> a -> a -> [Coord] -> Either [Coord] String
findPath' tileMap startPos endPos startVal endVal emptyVal fullVal emptyTileCoords = helper [endPos] 0 1
   where helper coords nextPosIndex currZ =
            if haveFinishCoord coords startPos
            then Left coords
            else if (allEmptyChecked coords emptyTileCoords) -- || (nextPosIndex > 2)
                 then Right "No path"
                 else
                     let (nextPos@(nextPosX, nextPosY, nextPosZ)) = (coords !! nextPosIndex)
                         adjacent = findAdjacent tileMap
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
                        then Right $ "No new coords found length coords: " ++ (show $ length goodCoords) ++ " old z: " ++
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
--pathStepList is a result from findPath. this contains every possible step

{-|
getShortestPath [] = []
getShortestPath pathStepList@(x:xs) =
   let maxStep@(_, _, maxStepZ) =
         foldr (\currStep@(x, y, currZ) accStep@(_, _, accZ) -> if currZ > accZ then currStep else accStep)
               x
               xs
   in maxStep : getShortestPath (filter (\(_, _, z_) -> z_ /= maxStepZ) xs)
-}

getShortestPath :: [Coord] -> [Coord]
getShortestPath pathStepList = getShortestPath' [] pathStepList

getShortestPath' [] [] = []
getShortestPath' [] pathStepList@(x:xs) =
   let maxStep@(_, _, maxStepZ) =
         foldr (\currStep@(x, y, currZ) accStep@(_, _, accZ) -> if currZ > accZ then currStep else accStep)
               x
               xs
   in getShortestPath' [maxStep] $ filter (\(_, _, z_) -> z_ /= maxStepZ) pathStepList

getShortestPath' accSteps@(accPrev:accRest) [] = accSteps
getShortestPath' accSteps@((accPrev@(prevX,prevY,prevZ)):accRest) pathStepList =
   let currStep@(_, _, currZ) =
         foldr (\foldStep@(foldX, foldY, foldZ) foldAcc ->
                  if foldZ+1 == prevZ && (abs $ foldX-prevX) <= 1 && (abs $ foldY-prevY) <= 1
                  then foldStep
                  else foldAcc)
               accPrev
               pathStepList
   in if currZ <= 0
      then accSteps
      else getShortestPath' (currStep : accSteps) $ filter (\(_,_,z_) -> z_ < currZ) pathStepList


------



--Matrix with character of chr'd numbers for paths (to get step number, use ord (M.getElem x y map) (ord z) - 300)
getMatrixMapChrPaths tileMap goodPath =
   let matrixMap = M.fromLists tileMap
   in helper' matrixMap goodPath
      where helper' mMap [] = mMap
            helper' mMap ((y,x,z):xs) = helper' (M.setElem (chr $ 300+z) (x+1, y+1) mMap) xs


kkDigit x = if x > 9 then '0' else intToDigit x

--Matrix with character digits of numbers for path
displayMatrixMapPaths tileMap goodPath =
   let matrixMap = M.fromLists tileMap
   in helper' matrixMap goodPath
      where helper' mMap [] = mMap
            helper' mMap ((y,x,z):xs) = helper' (M.setElem (kkDigit z) (x+1, y+1) mMap) xs

pathFinder :: (Eq a) => [[a]] -> a -> a -> a -> a -> Either [(Int, Int, Int)] String
pathFinder tMap startElement endElement emptyElement fullElement =
   let allPathsSteps = findPath tMap startElement endElement emptyElement fullElement
   in helper' allPathsSteps
      where helper' (Right x) = Right x
            helper' (Left x) = Left (getShortestPath x)

--list of all possible map paths
allPathsStepList :: Either [Coord] String
allPathsStepList = findPath getTileMap 's' 'o' '.' 'x'

shortestPathsStepList :: [Coord]
shortestPathsStepList = getShortestPath . extractEither $ allPathsStepList

allPathsStepMatrix = displayMatrixMapPaths getTileMap (extractEither allPathsStepList)
shortestPathsStepMatrix = displayMatrixMapPaths getTileMap shortestPathsStepList
originalMatrixMap = M.fromLists getTileMap --original map

main = do print "Path list:"
          print allPathsStepList
          print "all steps matrix:"
          print allPathsStepMatrix
          print "shortest path map:"
          print shortestPathsStepMatrix
          print "original map:"
          print originalMatrixMap
          return 0

--findAdjacent (M.fromLists getTileMap) (3, 8, 0) ['.', 's', 'o'] 'x'


extractEither (Left y) = y
extractEither (Right y) = error y

extractJust (Just y) = y

