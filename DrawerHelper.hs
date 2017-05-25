module DrawerHelper
( getElAscii
, elToAsciiGrid
, elToPathGrid
, drawGridToShapeData
, ShapeData(ShapeData, getShapeCoord, getShapeTerminals)
, getConnectedElements
, circuitToLayout
--, CircuitLayout(CircuitLayout, getLayoutElements, getConnectedPointCoords, getConnectedPointNames)
--, newCircuitLayout
) where

import Utils
import DrawGrid
import Circuit
import qualified Data.Matrix as M
import qualified Data.List as L
import qualified Data.Map as Map

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
                       } deriving (Show)

type CircuitLayout a = [ShapeConnectionData a]


--arrangeShapeData :: [ShapeData]
--                 -> TileCoord2 (dimensions) -> TileCoord2 (padding)
--                 -> Either CircError [TileCoord2]
--takes a list of shapeDatas, dimensions and padding
--and returns a list of each shape's x and y origins
--helper takes [[TileCoord2]] for each row
arrangeShapeData :: [ShapeData]
                 -> TileCoord2 -> TileCoord2
                 -> Either CircError [TileCoord2]

arrangeShapeData shapes
                 dimensions@(gridWidth, gridHeight)
                 padding@(paddingX, paddingY) =
   let helper' :: [ShapeData] -> Int -> Int -> [[TileCoord2]] -> [TileCoord2]
                 -> Either CircError [TileCoord2]

       helper' [] currX currY accCoords currRow =
         let accCoordsFlat = (L.concat accCoords) ++ currRow
             maxX = coord2MaxX accCoordsFlat
             maxY = coord2MaxY accCoordsFlat
             errMsg = "circuit doesn't fit in given grid dimensions"
         in if maxX >= gridWidth || maxY >= gridHeight
            then Left errMsg
            else Right accCoordsFlat

       helper' shapes@(s:xs) currX currY accCoords currRowCoords =
         let shapeWidth = getShapeWidth s
             shapeHeight = getShapeHeight s
             newX = shapeWidth + paddingX + currX
             --newY = shapeHeight + paddingY + currY
             currRowMaxHeight = coord2MaxY currRowCoords
         in if newX > gridWidth
            then helper' shapes
                         paddingX
                         (currY + currRowMaxHeight + paddingY)
                         (currRowCoords : accCoords)
                         []
            else helper' xs
                         newX
                         currY
                         accCoords
                         ((newX, currY) : currRowCoords)
   in helper' shapes paddingX paddingY [] []


{-
circuitLayoutToGrid :: CircuitLayout a -> Int -> Int -> DrawGrid
circuitLayoutToGrid cLayout width height = helper' cLayout
                                                   (newDrawGrid width height)
   where helper :: CircuitLayout a -> DrawGrid -> DrawGrid
         helper [] = DrawGrid
         helper (x:xs) =
         --helper layout grid
-}

--takes a drawing of an element and returns it's dimensions and terminal coords
drawGridToShapeData :: DrawGrid -> ShapeData
drawGridToShapeData shape =
   ShapeData (0, 0, gridNumCols shape, gridNumRows shape)
             (matrixFilter1 (getCharGrid shape) (\x -> x == '+' || x == '-'))
             shape


newShapeConnectionData :: CircuitElement a -> ([String], [String])
                       -> ShapeConnectionData a
newShapeConnectionData circEl (t1Connections, t2Connections) =
   let circElName = circuitElementName circEl
       el = element circEl
       shapeDrawGrid = elToPathGrid el
       shapeData = drawGridToShapeData shapeDrawGrid
   in ShapeConnectionData { getShapeData = shapeData
                          , getCircuitElement = circEl
                          , getShapeName = circElName
                          , getConnectedShapeNamesT1 = t1Connections
                          , getConnectedShapeNamesT2 = t2Connections
                          }

circuitToLayout :: Circuit a b -> Either CircError (CircuitLayout a)
circuitToLayout circ =
   let circElems = map fst $ elements circ
       connectedElems = getConnectedElements circ
   in foldr (\circEl acc
               -> let name = circuitElementName circEl
                      maybeConnections = Map.lookup name connectedElems
                  in if isJust maybeConnections
                     then let connections = extractJust maybeConnections
                          in fmap ((:) (newShapeConnectionData circEl connections)) acc
                     else Left "can't find name in connectedElems")
            (Right [])
            circElems


--getConnectedElements :: Circuit a b -> Map.Map String ([String], [String])

--basically goes through list and finds out which elements are connected
--getConnectedElements :: Circuit a b -> [[String]]
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



--translate element to ASCII code

-----------------

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



