import Data.List
import qualified Data.Matrix as M
import Data.Char (intToDigit)

type TileMatrix a = M.Matrix a
type TileMap a = [[a]]
type Coord = (Int, Int, Int)

getTileMap :: TileMap Char
getTileMap = [   --y
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
findPath' tileMap startPos endPos startVal endVal emptyVal fullVal emptyTileCoords = helper [endPos] 0
   where helper coords nextPosIndex =
            if haveFinishCoord coords startPos
            then Left coords
            else if (allEmptyChecked coords emptyTileCoords) -- || (nextPosIndex > 2)
                 then Right "No path"
                 else
                     let adjacent = findAdjacent tileMap
                                                 (coords !! nextPosIndex)
                                                 [emptyVal , startVal] --, endVal]
                                                 fullVal
                         newCoords = coords ++ adjacent
                         isGoodWeight coord@(x, y, z) =
                           foldr (\(x_, y_, z_) acc -> if (x == x_ && y == y_ && z > z_) then False else acc) --z >= z_
                                 True
                                 (delete coord coords) --newCoords)
                         goodCoords = (filter isGoodWeight newCoords)
                     in helper goodCoords (nextPosIndex+1)


haveFinishCoord coords endCoord@(x,y,_) =
   foldr (\(x_,y_,_) acc -> if x_ == x && y_ == y then True else acc)
         False
         coords

--can do this based on number of empty and checked coords, don't need to check each one
allEmptyChecked checkedCoords emptyCoords = length checkedCoords >= length emptyCoords

allEmptyChecked' checkedCoords emptyCoords =
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


getMatrixMapPaths tileMap goodPath =
   let matrixMap = M.fromLists tileMap
   in displayPaths' matrixMap goodPath
      where displayPaths' mMap [] = mMap
            displayPaths' mMap ((y,x,z):xs) = displayPaths' (M.setElem (intToDigit z) (x+1, y+1) mMap) xs


x = findPath getTileMap 's' 'o' '.' 'x'

extractEither (Left y) = y
extractJust (Just y) = y

y = getMatrixMapPaths getTileMap (extractEither x)
z = M.fromLists getTileMap

main = do print x
          print y
          print z
          return 0

--findAdjacent (M.fromLists getTileMap) (3, 8, 0) ['.', 's', 'o'] 'x'

