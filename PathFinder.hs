import Data.List
import qualified Data.Matrix as M


type TileMap a = [[a]]
type Coord = (Int, Int, Int)

getTileMap :: TileMap Char
getTileMap = [
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


findTile :: (Eq a) => a -> TileMap a -> Maybe Coord
findTile destChar tileMap = findTile' destChar tileMap 0

findTile' :: (Eq a) => a -> TileMap a -> Int -> Maybe Coord
findTile' destChar [] _         = Nothing
findTile' destChar (row:rows) y =
   let indexMaybe = elemIndex destChar row
   in case indexMaybe of
      Nothing -> findTile' destChar rows (y+1)
      Just x -> Just (x, y, 0)


--startChar = starting position, endChar = ending position,
--emptyChar = empty path, fullChar = occupied
findPath :: (Eq a) => TileMap a -> a -> a -> a -> Either [Coord] String
findPath tileMap startChar endChar emptyChar fullChar =
   let startPos = findTile startChar tileMap
       endPos = findTile endChar tileMap
   in case (startPos, endPos) of
      (Nothing, _) => Right "No starting position"
      (_, Nothing) => Right "No end position"
      _ => findPath' tileMap startChar endChar emptyChar fullChar

findPath' :: (Eq a) => TileMap a -> a -> a -> a -> Either [Coord] String
findPath' tileMap startChar endChar emptyChar fullChar =






