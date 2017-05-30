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
, cLayoutGetWireCoords
, ShapeConnectionData(getShapeData) --testing
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

--dimensions: (100, 100) padding: (5, 5)
{-
cLayoutGetWireCoords :: CircuitLayout a -> TileCoord2 -> TileCoord2 -> [TileCoord2]
cLayoutGetWireCoords cLayout dimensions@(gridWidth, gridHeight) padding@(paddingX, paddingY) =
   let helper' :: CircuitLayout a -> DrawGrid -> DrawGrid
       helper' [] grid = grid
       helper' (x:xs) grid =
         let (ShapeConnectionData shapeData circuitElement shapeName conT1 conT2 (Just arrangedCoord)) = x
             (ShapeData shapeCoord shapeTerminals shapeGrid) = shapeData
             (x1, y1, x2, y2) = shapeCoord --dimensions of empty
             (offsetX, offsetY) = arrangedCoord
             newShapeCoord = (x1+offsetX, y1+offsetY, x2+offsetX, y2+offsetY) --layout on actual grid
         in --instead of passing shapeGrid, we can generate x by x square grid with full characters
            overwriteGrid grid shapeGrid newShapeCoord
       drawnLines = helper' cLayout $ newDrawGrid gridWidth gridHeight
   in matrixFilter1 (getCharGrid drawnLines) (\ x -> x == '*')


cLayoutToGrid :: CircuitLayout a -> Int -> Int -> DrawGrid
cLayoutToGrid cLayout width height = helper' cLayout
                                             (newDrawGrid width height)
   where helper :: CircuitLayout a -> DrawGrid -> DrawGrid
         helper [] = DrawGrid
         helper (x:xs) =
         --helper layout grid
-}

--draws shapes in a grid
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


--ShapeData stures relative shapeCoords, this function returns absolute shape coords relative to grid
getAbsoluteCoords :: DrawGridInfo -> CircuitLayout a -> Either CircError [TileCoord2]
getAbsoluteCoords gridInfo cLayout =
   let dimensions@(gridWidth, gridHeight) = getDrawGridDimensions drawGridInfo
       padding@(paddingX, paddingY) = getDrawGridPadding drawGridInfo
       helper :: CircuitLayout a -> [ShapeCoord] -> [ShapeCoord]
       helper [] acc = acc
       helper (x:xs) acc =
         let (ShapeConnectionData sData cElem sName conT1 conT2 (Just arrangedCoord)) = x
             (ShapeData sCoord sTerms sGrid) = sData
             (x1, y1, x2, y2) = sCoord
             (offsetX, offsetY) = arrangedCoord
             newShapeCoord = (x1+offsetX+1, y1+offsetY+1, x2+offsetX, y2+offsetY)
         in helper xs (newShapeCoord:acc)
   in helper cLayout []



drawElemsForPathFinder :: TileCoord2 -> TileCoord


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
                               ++ "\tpaddingY: " ++ (show paddingY))-}

                              0
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



