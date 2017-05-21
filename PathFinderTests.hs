import PathFinder
import Utils

import qualified Data.Matrix as M


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

-- #test code

getTileMap = getTileMap1

tileMapInfo :: Either String (TileMapInfo Char)
tileMapInfo = tileMapToInfo getTileMap 's' 'o' '.' 'x'

--list of all possible map paths
allPathsStepList :: Either String [Coord]
allPathsStepList = tileMapInfo >>= findAllPaths

shortestPathsStepList :: Either String [Coord]
shortestPathsStepList = allPathsStepList >>= getShortestPath
--shortestPathsStepList = getShortestPath . extractEither
--                           $ allPathsStepList

allPathsStepMatrix = displayMatrixMapPaths getTileMap
                                           (extractEither allPathsStepList)

shortestPathsStepMatrix = displayMatrixMapPaths getTileMap
                                                (extractEither shortestPathsStepList)
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


