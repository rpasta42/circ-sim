module CmdDrawer (
  drawCircuit
) where

-- Command Line Drawer - converts circuit to Matrix

import Circuit
import qualified Data.Matrix as M
import qualified Data.Text as T
import qualified Data.List as L

type DrawGrid = (M.Matrix Int)

newDrawing :: Int -> Int -> DrawGrid
newDrawing x y = M.fromLists $ L.replicate x (map (\_->0) [1..y])


drawCircuit :: (Num a) => Circuit a b -> DrawGrid
drawCircuit (Circuit {elements=elems_draw_data}) =
   let circ_elems = map fst elems_draw_data
   in drawCircuit' circ_elems $ newDrawing 100 100


drawCircuit' :: (Num a) => [CircuitElement a] -> DrawGrid -> DrawGrid
drawCircuit' [] drawGrid = drawGrid
drawCircuit' circElems@(elem:restElems) drawGrid = drawCircuit' restElems newDrawGrid
   where newDrawGrid = drawElement elem drawGrid


drawElement :: (Num a) => CircuitElement a -> DrawGrid -> DrawGrid
drawElement (CircuitElement { circuitElementName=el_name
                            , element=el
                            , terminal1=t1
                            , terminal2=t2})
            drawGrid = drawElement' el

drawElement' :: (Num a) => Element a -> DrawGrid
drawElement' (EnergySourceElement source) = strToDrawGrid "\
   \+===+\n\
   \|=+=|\n\
   \|===|\n\
   \|=-=|\n\
   \+===+"


drawElement' (ResistorElement resistanceElem) = strToDrawGrid "\
   \+=========+\n\
   \|=-/\\/\\/-=|\n\
   \+=========+"


drawElement' (WireElement wireElem) = strToDrawGrid "----"


strToDrawGrid :: String -> DrawGrid
strToDrawGrid str =
   let lines = map T.unpack $ T.split (\x -> x == '\n') (T.pack str)
   in newDrawing 10 10 --M.fromLists lines --newDrawing 10 10


