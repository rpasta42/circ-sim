

data EnergySource a = VoltageSource a | CurrentSource a deriving (Show)

data Resistor a = Resistor { resistance :: a } deriving (Show)


data Element a = EnergySourceElement { source :: EnergySource a }
               | ResistorElement { resistor :: Resistor a }
               | WireElement { terminal1 :: Element a
                             , terminal2 :: Element a
                             }
               | ConnectorElement { terminals :: [Element a] }
                 deriving (Show)


data Point a = Point { posX :: a, posY :: a }


--a: unit for resistance, b: unit for location
data ElementDrawData a b = DrawData { positions :: [Point b]
                                    , circuitElemnt :: Element a
                                    }



