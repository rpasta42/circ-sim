import Data.List
import qualified Data.Matrix as M


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
findPath tileMap startTile endTile emptyTile fullTile =
   let startPosMaybe = findTile startTile tileMap
       endPosMaybe = findTile endTile tileMap
   in case (startPosMaybe, endPosMaybe) of
      (Nothing, _) -> Right "No starting position"
      (_, Nothing) -> Right "No end position"
      (Just startPos, Just endPos) -> findPath' (M.fromLists tileMap) startPos endPos emptyTile fullTile

findPath' :: (Eq a) => TileMatrix a -> Coord -> Coord -> a -> a -> Either [Coord] String
findPath' tileMap startPos endPos emptyTile fullTile = Left $ helper [endPos] endPos
   where helper coords nextPos =
            let adjacent = findAdjacent tileMap nextPos emptyTile fullTile
                newCoords = coords ++ adjacent
                isGoodWeight coord@(x, y, z) =
                  foldr (\(x_, y_, z_) acc -> if (x == x_ && y == y_ && z <= z_) then False else acc)
                        True
                        (delete coord newCoords)
            in filter isGoodWeight newCoords



findAdjacent :: (Eq a) => TileMatrix a -> Coord -> a -> a -> [Coord]
findAdjacent tileMap adjacentTo@(x, y, z) emptyTile fullTile =
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
                  (tileContents == fullTile) || (tileContents /= emptyTile))


x = findPath getTileMap 's' 'o' '.' 'x'


