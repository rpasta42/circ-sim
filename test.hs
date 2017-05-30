import Circuit
import DrawGrid
import CmdDrawer
import DrawerHelper


getTestCircuit1 :: (Num b, Num a) => Maybe (Circuit a b)
getTestCircuit1 = do
      circuit <- Just newCircuit
      battery <- Just $ newVoltageSource "battery" 10
      positiveWire <- Just $ newWire "wire+"
      negativeWire <- Just $ newWire "wire-"
      resistor <- Just $ newResistor "resistor" 2 --newResistorNamed
      (battery, positiveWire) <- Just $ connectElements battery positiveWire 1 1
      (resistor, positiveWire) <- Just $ connectElements resistor positiveWire 1 2
      (resistor, negativeWire) <- Just $ connectElements resistor negativeWire 2 1
      (battery, negativeWire) <- Just $ connectElements battery negativeWire 2 2

      cElements1 <- return [battery, positiveWire, negativeWire, resistor]
      cElements2 <- return [positiveWire, battery, negativeWire, resistor]

      circuit <- Just $ addCircuitElements circuit cElements2

      return circuit



test1 = do
      circuit <- getTestCircuit1

      resultGridDrawing <- Just $ drawCircuit circuit
      Just $ drawGridToDisplayStr resultGridDrawing

(Just testRet1) = test1
result1 = do
   mapM_ putStrLn testRet1

-------

--testing layouts (DrawerHelper.hs)

getTestCircuit2 :: (Num b, Num a) => Maybe (Circuit a b)
getTestCircuit2 = do
   circuit <- getTestCircuit1
   let secondBattery = newVoltageSource "battery2" 10
   let thirdBattery = newVoltageSource "battery3" 5
   circuit <- return $ addCircuitElements circuit [secondBattery, thirdBattery]
   return circuit



test2 = do
   let circuitMaybe1 = getTestCircuit1
       circuitMaybe2 = getTestCircuit2
       (Just circuit) = circuitMaybe2 -- 1 or 2
       gridPadding = (3,2)
       gridDimensions = (40, 20) --for (35,20) for cicruitMaybe1
   gridLayout <- circuitToLayout circuit gridDimensions gridPadding
   gridDrawing <- cLayoutGetWireCoords gridDimensions gridPadding gridLayout
   resultStr <- return $ drawGridToDisplayStr gridDrawing
   return resultStr

(Right testRet2) = test2
result2 = do
   mapM_ putStrLn testRet2


----test 3 (testing circuitToLayout coords)

gridPadding = (3,5)
gridDimensions = (40, 20)

(Just circuitTest3) = getTestCircuit2

layout3 = circuitToLayout circuitTest3 gridDimensions gridPadding
grid3 = layout3 >>= cLayoutGetWireCoords gridDimensions gridPadding
resultStr3 = drawGridToDisplayStr <$> grid3
(Right resultStr3') = resultStr3
resultIO3 = do
   mapM_ putStrLn resultStr3'


(Right layout3') = layout3
shapeData3 = map getShapeData layout3'
shapeCoords3 = map (getShapeCoord . getShapeData) layout3'

--getShapeData $ elToPathGrid $ element battery'


-------Test

--let battery = EnergySourceElement (VoltageSource 5)
--let resistor = ResistorElement $ Resistor 5
--let wire_pos = WireElement {terminal1=battery, terminal2=resistor}


