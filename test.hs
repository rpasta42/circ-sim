import Circuit
import DrawGrid
import CmdDrawer
import DrawerHelper

import qualified Data.List as L
import qualified Data.Matrix as M


getTestCircuit1 :: (Num b, Num a) => Maybe (Circuit a b)
getTestCircuit1 = do
      circuit <- Just newCircuit
      battery <- Just $ newVoltageSource "battery" 10
      positiveWire <- Just $ newWire "wire+"
      negativeWire <- Just $ newWire "wire-"
      resistor <- Just $ newResistor "resistor" 2 --newResistorNamed

      (battery, positiveWire) <- Just $ connectElements battery positiveWire 1 1
      (battery, negativeWire) <- Just $ connectElements battery negativeWire 2 1

      (resistor, positiveWire) <- Just $ connectElements resistor positiveWire 1 2
      (resistor, negativeWire) <- Just $ connectElements resistor negativeWire 2 2

      cElements1 <- return [battery, positiveWire, negativeWire, resistor]
      cElements2 <- return [positiveWire, battery, negativeWire, resistor]
      cElements3 <- return [positiveWire, resistor, battery, resistor]

      circuit <- Just $ addCircuitElements circuit cElements1

      return circuit

getTestCircuit1' :: (Num b, Num a) => Maybe (Circuit a b)
getTestCircuit1' = do
   circuit <- Just newCircuit
   battery <- Just $ newVoltageSource "battery" 10
   resistor <- Just $ newResistor "resistor" 2

   (resistor, battery) <- Just $ connectElements battery resistor 1 2
   (resistor, battery) <- Just $ connectElements battery resistor 2 1

   cElems1 <- return [resistor, battery]
   cElems2 <- return [battery, resistor]
   circuit <- Just $ addCircuitElements circuit cElems1

   return circuit


getTestCircuit2 :: (Num b, Num a) => Maybe (Circuit a b)
getTestCircuit2 = do
   circuit <- getTestCircuit1
   let secondBattery = newVoltageSource "battery2" 10
   let thirdBattery = newVoltageSource "battery3" 5
   circuit <- return $ addCircuitElements circuit [secondBattery, thirdBattery]
   return circuit



-------test1

test1 = do
      circuit <- getTestCircuit1

      resultGridDrawing <- Just $ drawCircuit circuit
      Just $ drawGridToDisplayStr resultGridDrawing

(Just testRet1) = test1
result1 = do
   mapM_ putStrLn testRet1


-------test2

--testing layouts (DrawerHelper.hs)

test2 = do
   let circuitMaybe1 = getTestCircuit1
       circuitMaybe2 = getTestCircuit2
       (Just circuit) = circuitMaybe2 -- 1 or 2
       gridPadding = (4,4) --(3,2)
       gridDimensions = (40, 20) --for (35,20) for cicruitMaybe1
       gridSizeInfo = (DrawGridInfo gridDimensions gridPadding)
   gridLayout <- circuitToLayout circuit gridSizeInfo

   gridDrawing <- cLayoutToGrid gridInfo gridLayout
   --gridDrawing <- cLayoutGetWireCoords gridInfo gridLayout

   resultStr <- return $ drawGridToDisplayStr gridDrawing
   return resultStr

(Right testRet2) = test2
result2 = do
   mapM_ putStrLn testRet2
-------end test2

------- test 3 (testing circuitToLayout coords)
gridPadding = (5,5) --(2,2) --(3,5)
gridDimensions = (45,25) -- (40, 25) --(40, 20)
gridInfo = DrawGridInfo gridDimensions gridPadding

(Just circuitTest3) = getTestCircuit1 --getTestCircuit2

layout3 = circuitToLayout circuitTest3 gridInfo

--grid3 = layout3 >>= cLayoutGetWireCoords gridInfo
--grid3 = layout3 >>= cLayoutToGrid gridInfo
grid3 = layout3 >>= cLayoutToGridWithWires gridInfo
resultStr3 = drawGridToDisplayStr <$> grid3
(Right resultStr3') = resultStr3
resultIO3 = do
   mapM_ putStrLn resultStr3'

--(Left l) = layout3
--resultErr = do putStrLn (show l)

(Right layout3') = layout3
shapeData3 = map getShapeData layout3'
shapeCoords3 = map (getShapeCoord . getShapeData) layout3'

--((x,y), (x,y)) points to connect
connectionCoords3 = layout3 >>= getConnectionPathCoords

wireCoords3 = layout3 >>= getConnectionCoords gridInfo

--map getShapeWidth shapeData3

--getShapeData $ elToPathGrid $ element battery'

--show $ fmap (Data.List.intercalate "\n" . M.toLists . getCharGrid .generatePathGrid gridInfo) layout3
--putStr $ Data.List.intercalate "\n" . M.toLists . getCharGrid . generatePathGrid gridInfo $ layout3'

------- end test 3



-------Test

--let battery = EnergySourceElement (VoltageSource 5)
--let resistor = ResistorElement $ Resistor 5
--let wire_pos = WireElement {terminal1=battery, terminal2=resistor}


