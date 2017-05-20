module CmdDrawer (
  drawCircuit
) where

-- Command Line Drawer - converts circuit to Matrix

import Circuit
import qualified Data.Matrix as M
import qualified Data.Text as T
import qualified Data.List as L

--data DrawGrid = DrawGridChar { drawGridMatrixCharGetter :: (M.Matrix Char) }
--              | DrawGridInt { drawGridMatrixIntGetter :: (M.Matrix Int) }
--   deriving (Show)
data DrawGrid = DrawGridChar (M.Matrix Char) | DrawGridInt (M.Matrix Int)
   deriving (Show)


--gridNumRows/gridNumCols
gridNumRows :: DrawGrid -> Int
gridNumRows (DrawGridChar g) = M.nrows g
gridNumRows (DrawGridInt g) = M.nrows g

gridNumCols :: DrawGrid -> Int
gridNumCols (DrawGridChar g) = M.ncols g
gridNumCols (DrawGridInt g) = M.ncols g


type ShapeCoord = (Int, Int, Int, Int) --top left and bottom right corners

newDrawing :: Int -> Int -> DrawGrid
--newDrawing x y = DrawGridInt . M.fromLists $ L.replicate x (map (\_->0) [1..y])
newDrawing x y = DrawGridChar . M.fromLists $ L.replicate x (map (\_->' ') [1..y])


--mReplace

mReplace :: M.Matrix a -> M.Matrix a -> ShapeCoord -> M.Matrix a
mReplace drawGrid drawElem drawCoordGrid =
   mReplace' drawGrid drawElem drawCoordGrid (0,0) (0,0)

--Matrix Replace: replace part of matrix with another
mReplace' :: M.Matrix a -> M.Matrix a -> ShapeCoord -> (Int, Int) -> (Int, Int) -> M.Matrix a
mReplace' drawGrid
          drawElem
          drawCoordGrid@(gridStartX, gridStartY, gridEndX, gridEndY)
          drawCoordElem@(elemX, elemY)
          tracker@(currX, currY)
   | currY+gridStartY >= gridEndY = drawGrid
   | currX+gridStartX >= gridEndX = mReplace' drawGrid drawElem
                                              drawCoordGrid drawCoordElem
                                              (0, currY+1)
   | otherwise =
      let currElem = M.getElem (currX+elemX) (currY+elemY) drawElem
          newGrid = M.setElem currElem (gridStartX+currX, gridStartY+currY) drawGrid
      in mReplace' newGrid drawElem drawCoordGrid drawCoordElem (currX+1, currY)



overwriteGrid :: DrawGrid -> DrawGrid -> ShapeCoord -> DrawGrid
--overwriteGrid drawGrid drawElem drawCoord
overwriteGrid (DrawGridChar drawGrid) (DrawGridChar drawElem) drawCoord =
   DrawGridChar (mReplace drawGrid drawElem drawCoord)


circuitWidth = 30 --100
circuitHeight = 30 --100

drawCircuit :: (Num a) => Circuit a b -> DrawGrid
drawCircuit (Circuit {elements=elems_draw_data}) =
   let circ_elems = map fst elems_draw_data
   in drawCircuit' circ_elems [] $ newDrawing circuitWidth circuitHeight


drawCircuit' :: (Num a) => [CircuitElement a] -> [ShapeCoord] -> DrawGrid -> DrawGrid
drawCircuit' _ [] drawGrid = drawGrid
drawCircuit' circElems@(elem:restElems) drawGridCoords drawGrid =
   let (newDrawGrid, newDrawGridCoords) = drawElement drawGrid drawGridCoords elem
   in drawCircuit' restElems newDrawGridCoords newDrawGrid



--adds an element to the supplied drawGrid
drawElement :: (Num a) => DrawGrid -> [ShapeCoord] -> CircuitElement a -> (DrawGrid, [ShapeCoord])
drawElement drawGrid
            drawGridCoords
            (CircuitElement { circuitElementName=el_name
                            , element=el
                            , terminal1=t1
                            , terminal2=t2}) =
   let newElement = elToAscii el
   in addElementToGrid newElement drawGrid drawGridCoords


addElementToGrid :: DrawGrid -> DrawGrid -> [ShapeCoord] -> (DrawGrid, [ShapeCoord])
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
       newStartX = elNumCols + 1
       newStartY = elNumRows + 1
       newEndX = newStartX + maxX
       newEndY = newStartY + maxY
       newShapeCoords = (newStartX, newStartY, newEndX, newEndY)
       newGrid = overwriteGrid drawGrid drawElement newShapeCoords
   in (newGrid, newShapeCoords : drawGridCoords)


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


