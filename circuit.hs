import Types


data Terminal a = Terminal { terminals :: [Element a] } deriving (Show)


--later can add thickness/etc
data Wire a = Wire deriving (Show)

data EnergySource a = VoltageSource { voltage :: a }
                    | CurrentSource { current :: a }
                      deriving (Show)

data Resistor a = Resistor { resistance :: a } deriving (Show)


data Element a = EnergySourceElement { source :: EnergySource a }
               | ResistorElement { resistor :: Resistor a }
               | WireElement { wire :: Wire a }
                 deriving (Show)

data CircuitElement a = CircuitElement { element :: Element a
                                       , terminal1 :: Terminal a
                                       , terminal2 :: Terminal a
                                       } deriving (Show)


--data used for drawing. later can add description,
--color, size, rotation, etc
data DrawData a = DrawData { positions :: [Point a]
                           } deriving (Show)


--a: unit for electric data, b: unit for location
data Circuit a b = Circuit { elements :: [(CircuitElement a, DrawData b)] }


newCircuit :: Circuit a b
newCircuit = Circuit { elements = [] }

newDrawData :: (Num a) => DrawData a
newDrawData = DrawData {positions = [Point 0 0]}

addCircuitElement :: (Num b, Num a) => Circuit a b -> CircuitElement a -> Circuit a b
addCircuitElement (Circuit {elements=elems}) elem =
   Circuit $ (elem, newDrawData) : elems





{-|
let battery = EnergySourceElement (VoltageSource 5)
let resistor = ResistorElement $ Resistor 5

let wire_pos = WireElement {terminal1=battery, terminal2=resistor}

-}


