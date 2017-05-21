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



-- #test code

getTileMap = getTileMap1

tileMapInfo :: Either String (TileMapInfo Char)
tileMapInfo = tileMapToInfo getTileMap 's' 'o' '.' 'x'

--list of all possible map paths
allPathsStepList :: Either String [TileCoord3]
allPathsStepList = tileMapInfo >>= findAllPaths

--list of shortest paths
shortestPathsStepList :: Either String [TileCoord3]
shortestPathsStepList = allPathsStepList >>= getShortestPath

--display matrix with all paths
allPathsStepMatrix = allPathsStepList >>= displayMatrixMapPaths getTileMap

--display matrix with shortest path
shortestPathsStepMatrix = shortestPathsStepList
                        >>= displayMatrixMapPaths getTileMap


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


