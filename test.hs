import Circuit
import DrawGrid
import CmdDrawer
import DrawerHelper

circuit = newCircuit

battery = newVoltageSource "battery" 10

positiveWire = newWire "wire+"
negativeWire = newWire "wire-"

resistor = newResistorNamed 2

(battery2, positiveWire2) = connectElements battery positiveWire 1 1
(resistor2, positiveWire3) = connectElements resistor positiveWire2 1 2
(resistor3, negativeWire2) = connectElements resistor2 negativeWire 2 1
(battery3, negativeWire3) = connectElements battery2 negativeWire2 2 2

battery' = battery3
positiveWire' = positiveWire3
negativeWire' = negativeWire3
resistor' = resistor3

circuit' = addCircuitElements newCircuit [battery', positiveWire', negativeWire', resistor']


resultDrawing = drawCircuit circuit'

resultStr = drawGridToDisplayStr resultDrawing
resultIO = do
   mapM_ putStrLn resultStr

--getShapeData $ elToPathGrid $ element battery'



--let battery = EnergySourceElement (VoltageSource 5)
--let resistor = ResistorElement $ Resistor 5
--let wire_pos = WireElement {terminal1=battery, terminal2=resistor}

