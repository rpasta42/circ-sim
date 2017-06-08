module DrawerHelper
( getElAscii
, elToAsciiGrid
, elToPathGrid
, drawGridToShapeData
, ShapeData(ShapeData, getShapeCoord, getShapeTerminals)
, getShapeWidth, getShapeHeight -- for tests
, getConnectedElements
, circuitToLayout
, arrangeShapeData
--, CircuitLayout(CircuitLayout, getLayoutElements, getConnectedPointCoords, getConnectedPointNames)
--, newCircuitLayout
, cLayoutGetWireCoords --Old, removeme
, cLayoutToGrid, cLayoutToGridWithWires
, ShapeConnectionData(getShapeData) --testing
, getConnectionPathCoords, getConnectionCoords --testing
, generatePathGrid
) where

import Utils
import DrawGrid
import Circuit
import qualified Data.Matrix as M
import qualified Data.List as L
import qualified Data.Map as Map
import qualified PathFinder2 as PF2

import Debug.Trace

debug = True

------ ### path finder setup stuff

pathFullChar = 'x'
pathEmptyChar = ' '
pathStartChar = 's'
pathEndChar = 'o'

tileMatrixFuncs = PF2.TileMatrixFuncs
   { PF2.isTileEmpty = \m (x, y) -> M.getElem y x m == pathEmptyChar
   , PF2.isTileStart = \m (x, y) -> M.getElem y x m == pathStartChar
   , PF2.isTileEnd   = \m (x, y) -> M.getElem y x m == pathEndChar
   }

------ ### end path finder setup stuff


------ ### data structures

{- battery:

--getShapeData $ elToPathGrid $ element battery'

ShapeData { getShapeCoord = (0,0,5,5)
          , getShapeEndPoints = [(1,3), (3,1), (3,5), (5,3)]
          }
-}

data ShapeData = ShapeData { getShapeCoord :: ShapeCoord
                           , getShapeTerminals :: [TileCoord2]
                           , getShapeGrid :: DrawGrid
                           } deriving (Show)

getShapeWidth :: ShapeData -> Int
getShapeWidth s =
   let (x1, _, x2, _) = getShapeCoord s
   in x2 - x1

getShapeHeight :: ShapeData -> Int
getShapeHeight s =
   let (_, y1, _, y2) = getShapeCoord s
   in y2 - y1

type ConnectedTerm = (String, Int)

data ShapeConnectionData a =
   ShapeConnectionData { getShapeData :: ShapeData
                       , getCircuitElement :: CircuitElement a
                       , getShapeName :: String
                       , getConnectedShapeNamesT1 :: [ConnectedTerm]
                       , getConnectedShapeNamesT2 :: [ConnectedTerm]
                       , arrangedCoord :: Maybe TileCoord2
                       } deriving (Show)

type CircuitLayout a = [ShapeConnectionData a] --TODO: dimensions should be stored here

------ ### end data structures


------ ### Drawing circuits without connectors

--OLD draws shapes in a grid (old version, new version is split into getAbsoluteCoords and cLayoutToGrid)
cLayoutGetWireCoords :: DrawGridInfo -> CircuitLayout a -> Either CircError DrawGrid
cLayoutGetWireCoords drawGridInfo cLayout =
   let dimensions@(gridWidth, gridHeight) = getDrawGridDimensions drawGridInfo
       padding@(paddingX, paddingY) = getDrawGridPadding drawGridInfo
       helper' :: CircuitLayout a -> DrawGrid -> DrawGrid
       helper' [] grid = grid
       helper' (x:xs) grid =
         let (ShapeConnectionData shapeData circuitElement shapeName conT1 conT2 (Just arrangedCoord)) = x
             (ShapeData shapeCoord shapeTerminals shapeGrid) = shapeData
             (x1, y1, x2, y2) = shapeCoord --dimensions of empty
             (offsetX, offsetY) = arrangedCoord
             --newShapeCoord = (x1+offsetX, y1+offsetY, x2+offsetX-1, y2+offsetY-1) --layout on actual grid
             --newShapeCoord = (1+offsetX, 1+offsetY, x2+offsetX, y2+offsetY)
             newShapeCoord = (1+offsetX+x1, 1+offsetY+y1, x2+offsetX, y2+offsetY)
         in --instead of passing shapeGrid, we can generate x by x square grid with full characters
            helper' xs $ overwriteGrid grid shapeGrid newShapeCoord
       drawnLines = helper' cLayout $ newDrawGrid gridWidth gridHeight
   in Right $ drawnLines


--ShapeData stores relative shapeCoords, this function returns absolute shape coords relative to draw grid
getAbsoluteCoords :: CircuitLayout a -> Either CircError [ShapeCoord]
getAbsoluteCoords cLayout = Right $
   foldr (\ x acc ->
            let (ShapeConnectionData sData cElem sName conT1 conT2 (Just arrangedCoord)) = x
                (ShapeData sCoord sTerms sGrid) = sData
                (x1, y1, x2, y2) = sCoord
                (offsetX, offsetY) = arrangedCoord
                newShapeCoord = (x1+offsetX+1, y1+offsetY+1, x2+offsetX, y2+offsetY)
            in newShapeCoord:acc)
         []
         cLayout

--returns DrawGrid, without wire connectors drawn
cLayoutToGrid :: DrawGridInfo -> CircuitLayout a -> Either CircError DrawGrid
cLayoutToGrid gridInfo cLayout =
   let dimensions@(gridWidth, gridHeight) = getDrawGridDimensions gridInfo
       (Right absoluteCoords) = getAbsoluteCoords cLayout
       zippedLayout = zip absoluteCoords cLayout
   in Right $
      foldr (\ (sCoord, shapeConnData) acc ->
               let (ShapeConnectionData sData cElem sName _ _ _) = shapeConnData
                   (ShapeData sRelativeCoord sTerms sGrid) = sData
               in overwriteGrid acc sGrid sCoord)
            (newDrawGrid gridWidth gridHeight)
            zippedLayout

------ ### end Drawing circuits without connectors

------ ### generating path finder grids

generateShapePathGrid :: Int -> Int -> Char -> DrawGrid
generateShapePathGrid width height elem = DrawGridChar $ M.matrix height width (\(x, y) -> elem)

newBorderGrid :: Int -> Int -> DrawGrid
newBorderGrid gridWidth gridHeight =
   let grid = newDrawGrid gridWidth gridHeight
       m1 = getCharGrid grid
       replaceWithFull = (\_ _ -> pathFullChar)
       m2 = M.mapRow replaceWithFull 1 m1 --drawing border for pathfinder
       m3 = M.mapRow replaceWithFull gridHeight m2
       m4 = M.mapCol replaceWithFull 1 m3
       m5 = M.mapCol replaceWithFull gridWidth m4
   in DrawGridChar m5

--generates 'x' filled grid without drawing actual Circuit Elemens
--TODO: add 'x' border around the whole grid
generatePathGrid :: DrawGridInfo -> CircuitLayout a -> DrawGrid
generatePathGrid gridInfo cLayout =
   let shapes = map (\ shapeConnData ->
                        let shapeSize@(x1, y1, x2, y2) = getShapeCoord . getShapeData $ shapeConnData
                            shapeGrid = generateShapePathGrid x2 y2 pathFullChar
                        in shapeGrid)
                    cLayout
       (Right absoluteCoords) = getAbsoluteCoords cLayout
       (gridWidth, gridHeight) = getDrawGridDimensions gridInfo
       zipped = zip absoluteCoords shapes
       goodDrawGrid = newBorderGrid gridWidth gridHeight
   in foldr (\ (shapeCoord, shapeGrid) acc ->
                  overwriteGrid acc shapeGrid shapeCoord)
            goodDrawGrid
            zipped

------ ### end generating path finder grids

------ ### getting coordinates for path finder

--return ShapeConnectionData from a name
findConnDataByName :: CircuitLayout a -> String -> Either CircError (ShapeConnectionData a)
findConnDataByName [] name = Left $ "findConnDataByName: couldn't find by name: " ++ name
findConnDataByName (x:xs) name =
   if getShapeName x == name
   then Right x
   else findConnDataByName xs name

--getShape(Relative/Absolute/Data)ConnectionCoords
--return (Negative Connections, Positive Connections)
getShapeRelativeConnectionCoords :: ShapeData -> ([TileCoord2], [TileCoord2])
getShapeRelativeConnectionCoords shapeData =
   let shapeMatrix = getCharGrid . getShapeGrid $ shapeData
       negConn = matrixFilter1 shapeMatrix (\x -> x == '-')
       posConn = matrixFilter1 shapeMatrix (\x -> x == '+')
   in (negConn, posConn)

getShapeAbsoluteConnectionCoords :: TileCoord2 -> ShapeData -> ([TileCoord2], [TileCoord2])
getShapeAbsoluteConnectionCoords offset@(offsetX, offsetY) shapeData =
   let relativeCons@(a,b) = getShapeRelativeConnectionCoords shapeData
       relToAbsFunc (x, y) = (x+offsetX, y+offsetY)
       a' = map relToAbsFunc a
       b' = map relToAbsFunc b
   in (a', b')

getShapeDataConnectionCoords :: ShapeConnectionData a -> ([TileCoord2], [TileCoord2])
getShapeDataConnectionCoords sConnData =
   let shapeData = getShapeData sConnData
       Just(thisOffset) = arrangedCoord sConnData
   in getShapeAbsoluteConnectionCoords thisOffset shapeData


{- getConnectionPathCoords: returns a list of start/end connections to draw
getConnectionPathCoords :: CircuitLayout a -> Either CircError [(TileCoord2, TileCoord2)]
-}
getConnectionPathCoords :: CircuitLayout a
                        -> Either CircError [(TileCoord2, TileCoord2)]
getConnectionPathCoords cLayout = getConnPathCoords' cLayout cLayout (Right [])

getConnPathCoords' :: CircuitLayout a -> CircuitLayout a
                   -> Either CircError [(TileCoord2, TileCoord2)]
                   -> Either CircError [(TileCoord2, TileCoord2)]
getConnPathCoords' [] _ acc = acc
getConnPathCoords' (sConnData:xs) originalCircuit acc =
   let thisConns@(thisPosConns, thisNegConns) = getShapeDataConnectionCoords sConnData
       thisPosCon = head thisPosConns --first positive connection
       thisNegCon = head thisNegConns --first negative connection

       negTerms = getConnectedShapeNamesT1 sConnData --neg connected (terminal 1)
       posTerms = getConnectedShapeNamesT2 sConnData --pos connected (terminal 2)

       --negIndices = map snd negTerms
       --posIndices = map snd posTerms

       negNames = map fst negTerms
       posNames = map fst posTerms

       --TODO: remove
       negTermNums = map snd negTerms
       posTermNums = map snd posTerms

       --shapeConnectionData
       negConnDatas = listEitherToEitherList $ map (findConnDataByName originalCircuit) negNames
       posConnDatas = listEitherToEitherList $ map (findConnDataByName originalCircuit) posNames

       currAcc = do
         negConnDatas' <- negConnDatas
         posConnDatas' <- posConnDatas

         --TODO: this is gonna be a bug because we don't account
         --for terminals of negConnDatas' and posConnDatas'

         negConns <- return $ map (\(a, i) ->
                                    let x1 = getShapeDataConnectionCoords a
                                        x2 = trace ((show x1) ++ (show i))
                                                   $ x1
                                    in snd x2 !! i --head $ snd x2 --fst x2 !! i
                                       {-if i == 0
                                       then head . fst $ x2
                                       else head . snd $ x2-} )
                                  (zip negConnDatas' negTermNums)

         posConns <- return $ map (\(a, i) ->
                                    let x1 = getShapeDataConnectionCoords a
                                        x2 = trace ((show x1) ++ (show i))
                                                   $ x1
                                    in fst x2 !! i --head $ snd x2 --fst x2 !! i
                                       {-if i == 0
                                       then head . fst $ x2
                                       else head . snd $ x2-} )
                                  (zip posConnDatas' posTermNums)

         --negConns <- return $ map (head . fst . getShapeDataConnectionCoords) negConnDatas'
         --posConns <- return $ map (head . fst . getShapeDataConnectionCoords) posConnDatas'

         {-
         negConns <- return $ map (\(a, i) ->
                                       ((fst $ getShapeDataConnectionCoords a) !! (negIndices !! i)))
                                  (zip negConnDatas' [0..])
         posConns <- return $ map (\(a, i) ->
                                       ((fst $ getShapeDataConnectionCoords a) !! (posIndices !! i)))
                                  (zip posConnDatas' [0..])
         -}

         negConnCoords <- return $ map (\x -> (thisNegCon, x)) negConns
         posConnCoords <- return $ map (\x -> (thisPosCon, x)) posConns

         return $ (negConnCoords) ++ (posConnCoords)

       newAcc = do
         acc' <- acc
         currAcc' <- currAcc
         return $ acc' ++ currAcc'
   in getConnPathCoords' xs originalCircuit newAcc


getAllPathSteps :: DrawGrid -> TileCoord2 -> TileCoord2 -> Either CircError [TileCoord2]
getAllPathSteps grid startCoord@(x1,y1) endCoord@(x2,y2) =
   let startCoord' = (y1,x1)
       endCoord' = (y2, x2)
       matrix1 = getCharGrid grid
       matrix2 = M.setElem pathStartChar startCoord' matrix1
       matrix3 = M.setElem pathEndChar endCoord' matrix2
       matrix4 = M.transpose matrix3
       --tileMapData = PF2.tileMapInitFromMatrix matrix3 tileMatrixFuncs
       tileMapData = {-trace ("matrix2: \n" ++ (L.intercalate "\n" . M.toLists $ matrix2) ++
                            "\tmatrix3: \n" ++ (L.intercalate "\n" . M.toLists $ matrix3))
                           $-} PF2.tileMapInitFromMatrix matrix3 tileMatrixFuncs

       allPaths = tileMapData >>= PF2.findAllPaths
       shortestPath3 = allPaths >>= PF2.getShortestPath
       shortestPath2 = fmap (map (\(x,y,z) -> (x,y))) shortestPath3
   in shortestPath2

switchCoord2XY :: TileCoord2 -> TileCoord2
switchCoord2XY (x,y) = (y,x)

--returns pixel locations of connections to draw
getConnectionCoords :: DrawGridInfo -> CircuitLayout a -> Either CircError [TileCoord2]
getConnectionCoords gridInfo cLayout =
   let dimensions@(gWidth, gHeight) = getDrawGridDimensions gridInfo
       (Right absoluteCoords) = getAbsoluteCoords cLayout
       pathGrid = generatePathGrid gridInfo cLayout
       terminalEndpoints = getConnectionPathCoords cLayout
       allPaths = (map (\(start, end) -> getAllPathSteps pathGrid start end)) <$> terminalEndpoints
   --in map switchCoord2XY <$> concat . concat <$> listEitherToEitherList <$> allPaths
   in concat . concat <$> listEitherToEitherList <$> allPaths


------ ### end getting coordinates for path finder

------ ### drawing wires

gridReplacePixels :: DrawGrid -> [TileCoord2] -> Char -> Either CircError DrawGrid
gridReplacePixels drawGrid pixs newElem =
   let helper' m [] = (DrawGridChar m)
       helper' m (pix@(x,y):pixs) = helper' newM pixs
         where newM = M.setElem newElem (y,x) m
   in Right $ helper' (getCharGrid drawGrid) pixs

cLayoutToGridWithWires :: DrawGridInfo -> CircuitLayout a -> Either CircError DrawGrid
cLayoutToGridWithWires dGrid cLayout =
   do wireCoords <- getConnectionCoords dGrid cLayout
      normalGrid <- cLayoutToGrid dGrid cLayout
      newGrid <- gridReplacePixels normalGrid wireCoords '\''
      return newGrid

------ ### end drawing wires

------ ### generating layouts from Circuit

--Takes a Circuit and returns "Either CircError (CircuitLayout a)"
circuitToLayout :: Circuit a b -> DrawGridInfo -> Either CircError (CircuitLayout a)
circuitToLayout circ drawGridInfo =
   --do tmpCircLayout <- circuitToLayout' circ
   --   shapePaths <- fmap (map getShapeData) tmpCircLayout

   let dimensions = getDrawGridDimensions drawGridInfo
       padding = getDrawGridPadding
       tmpCircLayout = circuitToLayout' circ
       shapePaths = fmap (map getShapeData) tmpCircLayout
       layoutCoords = shapePaths >>= arrangeShapeData drawGridInfo
       zipped = L.zip <$> tmpCircLayout <*> layoutCoords
       newCircLayout = map (\(shapeConData, layoutCoord)
                              -> setShapeConnectionDataArrangedCoords shapeConData layoutCoord)
                        <$> zipped
   in newCircLayout

circuitToLayout' :: Circuit a b -> Either CircError (CircuitLayout a)
circuitToLayout' circ =
   let circElems = map fst $ elements circ
       connectedElems = getConnectedElements circ
   in foldr (\circEl acc
               -> let name = circuitElementName circEl
                      maybeConnections = Map.lookup name connectedElems
                  in if isJust maybeConnections
                     then let connections = extractJust maybeConnections
                          in fmap ((:) (newShapeConnectionData circEl connections Nothing)) acc
                     else Left "can't find name in connectedElems")
            (Right [])
            circElems


{- arrangeShapeData
--takes a list of shapeDatas, dimensions and padding
--and returns a list of each shape's x and y origins
--helper takes [[TileCoord2]] for already layed-out rows
--and uses [TileCoord2] too keep track of current row
-}
arrangeShapeData :: DrawGridInfo -> [ShapeData] -> Either CircError [TileCoord2]
arrangeShapeData gridInfo shapes =
   let dimensions@(gridWidth, gridHeight) = getDrawGridDimensions gridInfo
       padding@(paddingX, paddingY) = getDrawGridPadding gridInfo
       helper' :: [ShapeData] -> Int -> Int -> [[TileCoord2]] -> [TileCoord2] -> Int
                 -> Either CircError [TileCoord2]

       helper' [] currX currY accCoords currRow rowMaxHeight =
         let accCoordsFlat = (L.concat accCoords) ++ (reverse currRow)
             maxX = coord2MaxX accCoordsFlat
             maxY = coord2MaxY accCoordsFlat
             errMsg = "circuit doesn't fit in given grid dimensions:  "
                      ++ " maxX: " ++ (show maxX)
                      ++ " maxY: " ++ (show maxY)
                      ++ " gridWidth: " ++ (show gridWidth)
                      ++ " gridHeight: " ++ (show gridHeight)
         in if maxX >= gridWidth || maxY >= gridHeight
            then Left errMsg
            else Right accCoordsFlat

       helper' shapes@(s:xs) currX currY accCoords currRowCoords rowMaxHeight =
         let shapeWidth = getShapeWidth s
             shapeHeight = getShapeHeight s
             newX = paddingX + currX
             newXEnd = newX + shapeWidth
             newCurrRowMaxHeight = max shapeHeight rowMaxHeight
             tmpDebug = 0
             {-tmpDebug = trace ("new X: " ++ (show newX)
                                  ++ "\tnew X End:" ++ (show newXEnd)
                                  ++ "\tshapeWidth: " ++ (show shapeWidth)
                                  ++ "\tmaxWidth: " ++ (show gridWidth)
                                  ++ "\tpaddingX: " ++ (show paddingX)
                                  ++ "\tpaddingY: " ++ (show paddingY))
                              0-}
         in if newXEnd + 1 + tmpDebug >= gridWidth
            then helper' shapes
                         0
                         (currY + newCurrRowMaxHeight + paddingY)
                         ((reverse currRowCoords) : accCoords)
                         --(currRowCoords : accCoords)
                         []
                         0
            else helper' xs
                         (newX + shapeWidth) --newXEnd
                         currY
                         accCoords
                         ((newX, currY) : currRowCoords)
                         newCurrRowMaxHeight
   in helper' shapes 0 paddingY [] [] 0


--newShapeCoonnectiondata: takes circuitElement and
--tuple with terminals and creates ShapeConnectionData
newShapeConnectionData :: CircuitElement a -> ([ConnectedTerm], [ConnectedTerm]) -> Maybe TileCoord2
                       -> ShapeConnectionData a
newShapeConnectionData circEl (t1Connections, t2Connections) maybeArrangedCoord =
   let circElName = circuitElementName circEl
       el = element circEl
       shapeDrawGrid = elToPathGrid el
       shapeData = drawGridToShapeData shapeDrawGrid
   in ShapeConnectionData { getShapeData = shapeData
                          , getCircuitElement = circEl
                          , getShapeName = circElName
                          , getConnectedShapeNamesT1 = t1Connections
                          , getConnectedShapeNamesT2 = t2Connections
                          , arrangedCoord = maybeArrangedCoord
                          }

setShapeConnectionDataArrangedCoords :: ShapeConnectionData a -> TileCoord2 -> ShapeConnectionData a
setShapeConnectionDataArrangedCoords (ShapeConnectionData { getShapeData = sData
                                                          , getCircuitElement = cEl
                                                          , getShapeName = sName
                                                          , getConnectedShapeNamesT1 = cT1
                                                          , getConnectedShapeNamesT2 = cT2
                                                          })
                                     arrangedCoord =
   ShapeConnectionData { getShapeData = sData
                       , getCircuitElement = cEl
                       , getShapeName = sName
                       , getConnectedShapeNamesT1 = cT1
                       , getConnectedShapeNamesT2 = cT2
                       , arrangedCoord = Just arrangedCoord
                       }


------ ### end generating layouts from Circuit


{- getConnectedElements
getConnectedElements :: Circuit a b -> Map.Map String ([String], [String])
--basically goes through list and finds out which elements are connected
-}

getConnectedElements :: Circuit a b -> Map.Map String ([ConnectedTerm], [ConnectedTerm])
getConnectedElements circ =
   let circElems = map fst $ elements circ
       connectedElems = getConnectedElems' circElems Map.empty
   in connectedElems

getConnectedElems' :: [CircuitElement a] -> Map.Map String ([ConnectedTerm], [ConnectedTerm])
                   -> Map.Map String ([ConnectedTerm], [ConnectedTerm])
getConnectedElems' [] acc = acc --TODO: replace with a fold
getConnectedElems' circElems@(x:xs) nameMap =
   let elName = circuitElementName x
       t1 = terminal1 x
       t2 = terminal2 x
       --assumes we used connectElements, not connectElementsByName
       connectedToT1 = terminals t1
       connectedToT2 = terminals t2
       connectedToT1Names = map (\(a, b) -> (circuitElementName a, b)) $ zip connectedToT1 [0..]
       connectedToT2Names = map (\(a, b) -> (circuitElementName a, b)) $ zip connectedToT2 [0..]
       newMap = Map.insert elName
                           (connectedToT1Names, connectedToT2Names)
                           nameMap
   in getConnectedElems' xs newMap


--takes a drawing of an element and returns
--its dimensions and terminal coords
drawGridToShapeData :: DrawGrid -> ShapeData
drawGridToShapeData shape =
   ShapeData (0, 0, gridNumCols shape, gridNumRows shape)
             (matrixFilter1 (getCharGrid shape) (\x -> x == '+' || x == '-'))
             shape


------ ### translate element to ASCII code

getElAscii :: Element a -> String --z and y means it's endpoint

getElAscii (EnergySourceElement source) = "\
   \/===\\\n\
   \|===|\n\
   \+=b=-\n\
   \|===|\n\
   \\\===/"

getElAscii (ResistorElement resistanceElem) = "\
   \/=========\\\n\
   \-==/\\/\\/==+\n\
   \\\=========/"

{-
getElAscii (EnergySourceElement source) = "\
   \/=+=\\\n\
   \|===|\n\
   \+=b=-\n\
   \|===|\n\
   \\\=-=/"

getElAscii (ResistorElement resistanceElem) = "\
   \/====+====\\\n\
   \-==/\\/\\/==+\n\
   \\\====-====/"
-}

{-
   \+===+\n\ --default
   \|=+=|\n\
   \|===|\n\
   \|=-=|\n\
   \+===+"

   \+=========+\n\ --default
   \|=-/\\/\\/-=|\n\
   \+=========+"

   \+=z=+\n\ --with z/y as +/-
   \|=+=|\n\
   \z===y\n\
   \|=-=|\n\
   \+=y=+"

   \+====z====+\n\ --with z/y as +/-
   \|=-/\\/\\/-=|\n\
   \+====y====+"
-}

getElAscii (WireElement wireElem) = "+====-"


------ ### end translate element to ASCII code

--drawer for figuring out wire paths
--elToPathGrid :: (Num a) => Element a -> DrawGrid
elToPathGrid :: Element a -> DrawGrid
elToPathGrid = strToDrawGrid . getElAscii


--drawer for displaying item (replace +/- terminals)
elToAsciiGrid :: (Num a) => Element a -> DrawGrid
elToAsciiGrid el = DrawGridChar $ matrixMap (getCharGrid . strToDrawGrid . getElAscii $ el)
                                            fixForDisplay
                                            ' '
   where fixForDisplay = (\el (x,y) m newM
                           -> if (el == '-' || el == '+')
                              then if (x == 1 || x == M.ncols m)
                                   then '|'
                                   else if (y == 1 || y == M.nrows m)
                                        then '='
                                        else el
                              else el)



