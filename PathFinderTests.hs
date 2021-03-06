
import qualified Data.Matrix as M
import qualified Data.List as L

--import qualified System.CPUTime as CPUT

--import qualified PathFinder as PF1
import qualified PathFinder3 as PF3
import qualified PathFinder2 as PF2

import PathFinder
import Utils

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

getTileMap4 :: TileMap Char --SHOULD FAIL
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

getTileMap7 = [
   "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx",
   "x                                           x",
   "x                                           x",
   "x                                           x",
   "x                                           x",
   "x    xxxxxsxxxxx     xxxxxx     xxxxxo      x",
   "x    xxxxxxxxxxx                            x",
   "x    xxxxxxxxxxx                            x",
   "x                                           x",
   "x                                           x",
   "x                                           x",
   "x                                           x",
   "x                                           x",
   "x                                           x",
   "x                                           x",
   "x    xxxxx                                  x",
   "x    xxxxx                                  x",
   "x    xxxxx                                  x",
   "x    xxxxx                                  x",
   "x    xxxxx                                  x",
   "x                                           x",
   "x                                           x",
   "x                                           x",
   "x                                           x",
   "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
   ]



-- #test code PF2

tileMatrixFuncs1 = PF2.TileMatrixFuncs
      { PF2.isTileEmpty = \m (x,y) -> M.getElem y x m == '.'
      , PF2.isTileStart = \m (x,y) -> M.getElem y x m == 's'
      , PF2.isTileEnd   = \m (x,y) -> M.getElem y x m == 'o'
      }

tileMatrixFuncs2 = PF2.TileMatrixFuncs
   { PF2.isTileEmpty = \m (x,y) -> M.getElem y x m == ' '
   , PF2.isTileStart = \m (x,y) -> M.getElem y x m == 's'
   , PF2.isTileEnd = \m (x,y) -> M.getElem y x m == 'o'
   }

tileMatrixFuncs = tileMatrixFuncs2 --2
getTileMap = getTileMap7 --7

tileMapData = PF2.tileMapInitFromMap getTileMap tileMatrixFuncs
tileMapPaths = tileMapData >>= PF2.findAllPaths
shortestPath = tileMapPaths >>= PF2.getShortestPath

--debug:
isEmpty = PF2.isTileEmpty tileMatrixFuncs
(Right tileMapData') = tileMapData
tMatrix = PF2.tileMatrix tileMapData'

--tileMapData
--tileMapPaths
--isEmpty tMatrix (3, 8) --returns True???
--PF2.findAdjacent tileMapData' (4, 9, 0)
--PF2.findAdjacent tileMapData' (3, 9, 0)
--    (3,8) shouldn't be here

allPathsMatrixPF2 = do --PF2.displayMatrixMapPaths <$> tileMapData <*> tileMapPaths
   tMapData <- tileMapData
   tMapPaths <- tileMapPaths
   PF2.displayPaths tMapData tMapPaths

(Right allPathsMatrixPF2') = allPathsMatrixPF2

shortestPathMatrixPF2 = do
   tMapData <- tileMapData
   shortestPath' <- shortestPath
   PF2.displayPaths tMapData shortestPath'

(Right shortestPathMatrixPF2') = shortestPathMatrixPF2

picoToMilli n = n / 1000 / 1000
picoToSeconds n = picoToMilli n / 1000

--use :set +s instead
--PF2: 1,203,333 1,266,666
--PF3: 1,513,332 1,233,333

x = do
      --t1 <- CPUT.getCPUTime
      putStr "\n\n"
      putStr . L.intercalate "\n" . M.toLists $ shortestPathMatrixPF2'
      putStr "\n\n"
      --t2 <- CPUT.getCPUTime

      --milliseconds
      --putStrLn . show . round . picoToMilli $ fromIntegral (t2 - t1)

(Left shortestPathError') = shortestPathMatrixPF2
y = do putStr shortestPathError'

--with foldr:
--fmap head tileMapPaths --1.31 secs

--with foldl:
--fmap head tileMapPaths --1.43 secs


-- #test code PF1


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


