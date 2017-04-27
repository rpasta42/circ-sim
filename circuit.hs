

data EnergySource a = VoltageSource { voltage :: a }
                    | CurrentSource { current :: a }
                      deriving (Show)

data Resistor a = Resistor { resistance :: a } deriving (Show)


data Element a = EnergySourceElement { source :: EnergySource a }
               | ResistorElement { resistor :: Resistor a }
               | WireElement { terminal1 :: Element a
                             , terminal2 :: Element a
                             }
               | ConnectorElement { terminals :: [Element a] }
                 deriving (Show)


data Point a = Point { posX :: a, posY :: a }
               deriving (Show)

--a: unit for resistance, b: unit for location
data ElementDrawData a b = DrawData { positions :: [Point b]
                                    , circuitElemnt :: Element a
                                    } deriving (Show)


{-|
let battery = EnergySourceElement (VoltageSource 5)
let resistor = ResistorElement $ Resistor 5

let wire_pos = WireElement {terminal1=battery, terminal2=resistor}

-}


