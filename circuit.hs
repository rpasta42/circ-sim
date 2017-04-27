import Types

data Terminal a = Terminal { terminals :: [Element a] }

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


--a: unit for resistance, b: unit for location
data ElementDrawData a b = DrawData { positions :: [Point b]
                                    , circuitElemnt :: CircuitElement a
                                    } deriving (Show)


data Circuit a b = Circuit { elements :: [(CircuitElement a, ElementDrawData b)] }

{-|
let battery = EnergySourceElement (VoltageSource 5)
let resistor = ResistorElement $ Resistor 5

let wire_pos = WireElement {terminal1=battery, terminal2=resistor}

-}


