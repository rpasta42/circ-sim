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

      circuit <- Just $ addCircuitElements circuit [battery, positiveWire, negativeWire, resistor]

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
   let circuit = addCircuitElements circuit [secondBattery]
   return circuit



test2 = do
   let circuitMaybe = getTestCircuit2 --or getTestCircuit1
       (Just circuit) = circuitMaybe
       gridPadding = (3,5)
       gridDimensions = (40, 60) --(30,20), (30,40)
   gridLayout <- circuitToLayout circuit gridDimensions gridPadding
   gridDrawing <- cLayoutGetWireCoords gridDimensions gridPadding gridLayout
   resultStr <- return $ drawGridToDisplayStr gridDrawing
   return resultStr

--layout = circuitToLayout circuit2' gridDimensions gridPadding
--grid = layout >>= cLayoutGetWireCoords gridDimensions gridPadding
--resultStr2 = drawGridToDisplayStr <$> grid
--(Right resultStr2') = resultStr2
--resultIO2 = do
--   mapM_ putStrLn resultStr2'


--getShapeData $ elToPathGrid $ element battery'


-------Test

--let battery = EnergySourceElement (VoltageSource 5)
--let resistor = ResistorElement $ Resistor 5
--let wire_pos = WireElement {terminal1=battery, terminal2=resistor}


