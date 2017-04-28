import Data.List

type TileMap a = [[a]]

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

findTileMap :: (Eq a) => a -> TileMap a -> Maybe (Int, Int, Int)
findTileMap destChar tileMap = findTileMap' destChar tileMap 0


findTileMap' :: (Eq a) => a -> TileMap a -> Int -> Maybe (Int, Int, Int)
findTileMap' destChar [] _         = Nothing
findTileMap' destChar (row:rows) y =
   let indexMaybe = elemIndex destChar row
   in case indexMaybe of
      Nothing -> findTileMap' destChar rows (y+1)
      Just x -> Just (x, y, 0)

--findDest :: (Eq a) => TileMap -> a -> a -> a -> [Coord]
--findDest tileMap startChar endChar emptyChar fullChar


