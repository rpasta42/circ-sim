
import Utils

data TileMapFuncs a = TimeMapFuncs { isTileEmpty :: (TileMatrix a -> TileCoord2 -> Bool)
                                   , isTileStart :: (TileMatrix a -> TileCoord2 -> Bool)
                                   , isTileEnd   :: (TIleMatrix a -> TileCoord2 -> Bool)
                                   }

data TileMapInfo a =
   TileMapInfo { tileMap :: TileMap a
               , tileMatrix :: TileMatrix a
               , tileStartPos :: TileCoord2
               , tileEndPos :: TileCoord2
               , tileIsEmpty :: (TileMatrix a -> TileCoord2 -> Bool)
               }


-- # findAllPaths = returns a list of all path coords

findAllPaths :: (Eq a) => TileMapInfo a -> Either String [TileCoord3]
findAllPaths tileMapInfo@(TileMapInfo tMap tMatrix tStart tEnd tIsEmpty) =
   findAllPaths' tileMapInfo
                 (getEmptyTileCoords tMatrix tIsEmpty)
   where
      getEmptyTileCoords :: (Eq a) => TileMatrix a -> (TimeMatrix a -> TileCoord2 -> Bool) -> [TileCoord2]
      getEmptyTileCoords tileMatrix isEmpty = matrixFilter2 tileMatrix isEmpty


findAllPaths' :: (Eq a) => TileMapInfo a -> [TileCoord2] -> Either String [TileCoord3]
findAllPaths' (TileMapInfo tMap tMatrix tStartPos tEndPos tIsEmpty) emptyTileCoords =
   helper [endPos] 0 1
   where
      haveFinishTileCoord coords endCoord@(x,y,_) =
         foldr (\(x_,y_,_) acc -> if x_ == x && y_ == y then True else acc)
         False
         coords

      findAdjacent :: (Eq a) => TileMatrix a -> TileCoord3 -> [a] -> a -> [TileCoord3]
      findAdjacent tileMap adjacentTo@(x, y, z) =
         let midLeft = (x-1, y,   z+1)
             topMid  = (x,   y-1, z+1)
             botMid  = (x,   y+1, z+1)
             midRight= (x+1, y,   z+1)
         in filter (not . isBad) [midLeft, topMid, botMid, midRight]
            where isBad (x, y, _) =
                     let tileContents = M.getElem (y+1) (x+1) tileMap
                     in (x < 0 || y < 0 || x > (M.ncols tileMap - 1) || y > (M.ncols tileMap - 1) ||
                        (tileContents == fullTile) || (not $ tileContents `elem` nonFullTile))

      --can do this based on number of empty and checked coords, don't need to check each one
      --allEmptyChecked checkedTileCoord3s emptyTileCoord3s = length checkedTileCoord3s >= length emptyTileCoord3s
      allEmptyChecked checkedTileCoords emptyTileCoords =
         let isTileCoordInChecked coord@(x, y, _) =
               foldr (\(x_, y_) acc -> if x_ == x && y_ == y then True else acc)
                     False
                     checkedTileCoords
             leftOverTileCoords = filter (not . isTileCoordInChecked) emptyTileCoords
         in length leftOverTileCoords == 0

      helper coords nextPosIndex currZ =
         if haveFinishTileCoord coords tStartPos
         then Right coords
         else if (allEmptyChecked coords emptyTileCoords) -- || (nextPosIndex > 2)
              then Left "No path"
              else
                  let (nextPos@(nextPosX, nextPosY, nextPosZ)) = (coords !! nextPosIndex)
                      adjacent = findAdjacent tMatrix
                                              nextPos
                                              [emptyVal, startVal] --, endVal]
                                              fullVal
                      newTileCoord3s = coords ++ adjacent
                      isGoodWeight coord@(x, y, z) = --TODO: z>= z_ then 6th step in getTileMap1 is bad. if z>z_ then getTileMap3 fails
                        foldr (\(x_, y_, z_) acc -> if (x == x_ && y == y_ && z >= z_) then False else acc) --z >= z_
                              True
                              (delete coord coords) --newTileCoord3s) --coords) --newTileCoord3s)
                      goodTileCoord3s = (filter isGoodWeight newTileCoord3s)
                  --in helper goodTileCoord3s (nextPosIndex+1)
                  in if length goodTileCoord3s == length coords && (not $ haveFinishTileCoord3 goodTileCoord3s startPos) && nextPosZ > currZ
                     then Left $ "No new coords found length coords: " ++ (show $ length goodTileCoord3s) ++ " old z: " ++
                                  (show $ currZ) ++ " next z:" ++ (show $ nextPosZ)
                     else helper goodTileCoord3s (nextPosIndex+1) nextPosZ

                  --in if null goodTileCoord3s then Right "No adjacent movable" else helper goodTileCoord3s (nextPosIndex+1)



-- # getShortestPath = takes a list of all paths, and returns list for the shortest path
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

getShortestPath :: [TileCoord3] -> Either String [TileCoord3]
getShortestPath pathStepList = getShortestPath' [] pathStepList

getShortestPath' [] [] = Left "getShortestPath error" --before was []
getShortestPath' [] pathStepList@(x:xs) =
   let maxTileCoord3ByZ first rest =
         foldr (\currStep@(x,y,currZ) accStep@(_, _, accZ)
                  -> if currZ > accZ then currStep else accStep)
               first
               rest
       maxStep@(_, _, maxStepZ) = maxTileCoord3ByZ x xs
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



