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
, cLayoutToGrid
, ShapeConnectionData(getShapeData) --testing
, getConnectionPathCoords
) where

import Utils
import DrawGrid
import Circuit
import qualified Data.Matrix as M
import qualified Data.List as L
import qualified Data.Map as Map
import Debug.Trace

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

data ShapeConnectionData a =
   ShapeConnectionData { getShapeData :: ShapeData
                       , getCircuitElement :: CircuitElement a
                       , getShapeName :: String
                       , getConnectedShapeNamesT1 :: [String]
                       , getConnectedShapeNamesT2 :: [String]
                       , arrangedCoord :: Maybe TileCoord2
                       } deriving (Show)

type CircuitLayout a = [ShapeConnectionData a] --TODO: dimensions should be stored here


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

--generates 'x' filled grid without drawing actual Circuit Elemens
--TODO: add 'x' border around the whole grid
generatePathGrid :: DrawGridInfo -> CircuitLayout a -> DrawGrid
generatePathGrid gridInfo cLayout =
   let shapes = map (\ shapeConnData ->
                        let shapeSize@(x1, y1, x2, y2) = getShapeCoord . getShapeData  $ shapeConnData
                            shapeGrid = generateShapePathGrid x2 y2 'x'
                        in shapeGrid)
                    cLayout
       (Right absoluteCoords) = getAbsoluteCoords cLayout
       (gridWidth, gridHeight) = getDrawGridDimensions gridInfo
       zipped = zip absoluteCoords shapes
   in foldr (\ (shapeCoord, shapeGrid) acc ->
                  overwriteGrid acc shapeGrid shapeCoord)
            (newDrawGrid gridWidth gridHeight)
            zipped

------ ### end generating path finder grids

------ ### getting coordinates for path finder

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
       thisPosCon = head thisPosConns
       thisNegCon = head thisNegConns
       negNames = getConnectedShapeNamesT1 sConnData
       posNames = getConnectedShapeNamesT2 sConnData
       negConnDatas = listEitherToEitherList $ map (findConnDataByName originalCircuit) negNames
       posConnDatas = listEitherToEitherList $ map (findConnDataByName originalCircuit) posNames
       currAcc = do
         negConnDatas' <- negConnDatas
         posConnDatas' <- posConnDatas

         --TODO: this is gonna be a bug because we don't account
         --for terminals of negConnDatas' and posConnDatas'
         negConns <- return $ map (head . fst . getShapeDataConnectionCoords) negConnDatas'
         posConns <- return $ map (head . fst . getShapeDataConnectionCoords) posConnDatas'

         negConnCoords <- return $ map (\x -> (thisNegCon, x)) negConns
         posConnCoords <- return $ map (\x -> (thisPosCon, x)) posConns

         return $ (negConnCoords) ++ (posConnCoords)

       newAcc = do
         acc' <- acc
         currAcc' <- currAcc
         return $ acc' ++ currAcc'
   in getConnPathCoords' xs originalCircuit newAcc



--returns pixel locations of connections to draw
getConnectionCoords :: DrawGridInfo -> CircuitLayout a -> [TileCoord2]
getConnectionCoords gridInfo cLayout =
   let dimensions@(gWidth, gHeight) = getDrawGridDimensions gridInfo
       (Right absoluteCoords) = getAbsoluteCoords cLayout
       pathGrid = generatePathGrid gridInfo cLayout

------ ### end getting coordinates for path finder



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
         let accCoordsFlat = (L.concat accCoords) ++ currRow
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
                         []
                         0
            else helper' xs
                         (newX + shapeWidth) --newXEnd
                         currY
                         accCoords
                         ((newX, currY) : currRowCoords)
                         newCurrRowMaxHeight
   in helper' shapes 0 paddingY [] [] 0

------ ### end generating layouts from Circuit


--newShapeCoonnectiondata: takes circuitElement and
--tuple with terminals and creates ShapeConnectionData
newShapeConnectionData :: CircuitElement a -> ([String], [String]) -> Maybe TileCoord2
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


{- getConnectedElements
getConnectedElements :: Circuit a b -> Map.Map String ([String], [String])
--basically goes through list and finds out which elements are connected
-}

getConnectedElements :: Circuit a b -> Map.Map String ([String], [String])
getConnectedElements circ =
   let circElems = map fst $ elements circ
       connectedElems = getConnectedElems' circElems Map.empty
   in connectedElems

getConnectedElems' :: [CircuitElement a] -> Map.Map String ([String], [String])
                   -> Map.Map String ([String], [String])
getConnectedElems' [] acc = acc --TODO: replace with a fold
getConnectedElems' circElems@(x:xs) nameMap =
   let elName = circuitElementName x
       t1 = terminal1 x
       t2 = terminal2 x
       --assumes we used connectElements, not connectElementsByName
       connectedToT1 = terminals t1
       connectedToT2 = terminals t2
       connectedToT1Names = map circuitElementName connectedToT1
       connectedToT2Names = map circuitElementName connectedToT2
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


-----------------

--translate element to ASCII code
getElAscii :: Element a -> String --z and y means it's endpoint

getElAscii (EnergySourceElement source) = "\
   \/=+=\\\n\
   \|===|\n\
   \+=b=-\n\
   \|===|\n\
   \\\=-=/"

getElAscii (ResistorElement resistanceElem) = "\
   \/====+====\\\n\
   \-=-/\\/\\/-=+\n\
   \\\====-====/"

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


-----------------

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



