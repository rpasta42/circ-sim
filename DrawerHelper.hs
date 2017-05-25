module DrawerHelper
( getElAscii
, elToAsciiGrid
, elToPathGrid
, getShapeData
, ShapeData(ShapeData, getShapeCoord, getShapeEndPoints)
, CircuitLayout(CircuitLayout, getLayoutElements, getConnectedPointCoords, getConnectedPointNames)
, getConnectedElements
, newCircuitLayout
) where

import Utils
import DrawGrid
import Circuit
import qualified Data.Matrix as M
import qualified Data.Map as Map

{- battery:

--getShapeData $ elToPathGrid $ element battery'

ShapeData { getShapeCoord = (0,0,5,5)
          , getShapeEndPoints = [(1,3), (3,1), (3,5), (5,3)]
          }
-}

data ShapeData = ShapeData { getShapeCoord :: ShapeCoord
                           , getShapeEndPoints :: [TileCoord2]
                           } deriving (Show)

getShapeData :: DrawGrid -> ShapeData
getShapeData shape =
   ShapeData (0, 0, gridNumCols shape, gridNumRows shape)
             (matrixFilter1 (getCharGrid shape) (\x -> x == '+' || x == '-'))


data CircuitLayout a =
   CircuitLayout { getLayoutElements :: [(ShapeData, CircuitElement a)]
                 , getConnectedPointCoords :: [(TileCoord2, TileCoord2)]
                 , getConnectedPointNames :: [(String, String)]
                 }


--getConnectedElements

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


newCircuitLayout :: CircuitLayout a
newCircuitLayout = CircuitLayout { getLayoutElements = []
                                 , getConnectedPointCoords = []
                                 , getConnectedPointNames = []
                                 }


layoutCircuit :: Circuit a b -> CircuitLayout a
layoutCircuit (Circuit {elements=elems}) =
   let circElems = map fst elems
   in layoutCircuit' circElems newCircuitLayout

--layoutCircuit' elems circLayout
layoutCircuit' [] circLayout = circLayout
--layoutCircuit'






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
elToPathGrid :: (Num a) => Element a -> DrawGrid
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



