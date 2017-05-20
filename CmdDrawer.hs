module CmdDrawer (
  drawCircuit
, elToAscii --for testing
) where

-- Command Line Drawer - converts circuit to Matrix

import Circuit
import Utils
import DrawGrid
import qualified Data.Matrix as M
import qualified Data.Text as T



circuitWidth = 30 --100
circuitHeight = 30 --100

drawCircuit :: (Num a) => Circuit a b -> DrawGrid
drawCircuit (Circuit {elements=elems_draw_data}) =
   let circ_elems = map fst elems_draw_data
   in drawCircuit' circ_elems [] $ newDrawGrid circuitWidth circuitHeight

drawCircuit' :: (Num a)
             => [CircuitElement a] -> [ShapeCoord] -> DrawGrid
             -> DrawGrid

drawCircuit' [] _ drawGrid = drawGrid
drawCircuit' circElems@(elem:restElems) drawGridCoords drawGrid =
   let (newDrawGrid, newDrawGridCoords) = drawElement drawGrid drawGridCoords elem
   in drawCircuit' restElems newDrawGridCoords newDrawGrid


--adds an element to the supplied drawGrid
drawElement :: (Num a)
            => DrawGrid -> [ShapeCoord] -> CircuitElement a
            -> (DrawGrid, [ShapeCoord])
drawElement drawGrid
            drawGridCoords
            (CircuitElement { circuitElementName=el_name
                            , element=el
                            , terminal1=t1
                            , terminal2=t2}) =
   let newElement = elToAscii el
   in addElementToGrid newElement drawGrid drawGridCoords


addElementToGrid :: DrawGrid -> DrawGrid -> [ShapeCoord]
                 -> (DrawGrid, [ShapeCoord])
addElementToGrid drawElement drawGrid drawGridCoords =
   let drawGridNumCols = gridNumCols drawGrid
       drawGridNumRows = gridNumRows drawGrid
       elNumCols = gridNumCols drawElement
       elNumRows = gridNumRows drawElement
       maxCoord@(maxX, maxY) = foldr (\(_, _, x, y) (xAcc, yAcc) ->
                                          ( if x > xAcc then x else xAcc
                                          , if y > yAcc then y else yAcc))
                                     (0, 0)
                                     drawGridCoords
       newStartX = maxX + 1
       newStartY = maxY + 1
       newEndX = newStartX + elNumCols - 1
       newEndY = newStartY + elNumRows - 1
       newShapeCoords = (newStartX, newStartY, newEndX, newEndY)
       newGrid = overwriteGrid drawGrid drawElement newShapeCoords
   in (newGrid, newShapeCoords : drawGridCoords)



--translate element to ASCII code

elToAscii :: (Num a) => Element a -> DrawGrid

elToAscii (EnergySourceElement source) = strToDrawGrid "\
   \+===+\n\
   \|=+=|\n\
   \|===|\n\
   \|=-=|\n\
   \+===+"

elToAscii (ResistorElement resistanceElem) = strToDrawGrid "\
   \+=========+\n\
   \|=-/\\/\\/-=|\n\
   \+=========+"

elToAscii (WireElement wireElem) = strToDrawGrid "----"


strToDrawGrid :: String -> DrawGrid
strToDrawGrid str =
   let lines = map T.unpack $ T.split (\x -> x == '\n') (T.pack str)
   in DrawGridChar $ M.fromLists lines --newDrawing 10 10


