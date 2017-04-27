import Types
import Data.List


data Terminal a = Terminal { terminals :: [CircuitElement a] }

instance Show (Terminal a) where
   show (Terminal []) = "none"
   show (Terminal terminals) = "none" -- intercalate "\n, " $ map circuitElementName terminals

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

data CircuitElement a = CircuitElement { circuitElementName :: String
                                       , element :: Element a
                                       , terminal1 :: Terminal a
                                       , terminal2 :: Terminal a
                                       } deriving (Show)


--data used for drawing. later can add description,
--color, size, rotation, etc
data DrawData a = DrawData { positions :: [Point a]
                           } deriving (Show)


--a: unit for electric data, b: unit for location
data Circuit a b = Circuit { elements :: [(CircuitElement a, DrawData b)]
                           } deriving (Show)


-- constructor functions

newTerminal :: Terminal a
newTerminal = Terminal { terminals = [] }

newCircuit :: Circuit a b
newCircuit = Circuit { elements = [] }

newDrawData :: (Num a) => DrawData a
newDrawData = DrawData {positions = [Point 0 0]}

newCircuitElement :: String -> Element a -> CircuitElement a
newCircuitElement name e = CircuitElement {element=e, terminal1=newTerminal, terminal2=newTerminal, circuitElementName=name}

newWire :: CircuitElement a
newWire = newCircuitElement "wire" $ WireElement $ Wire

newVoltageSource :: a -> CircuitElement a
newVoltageSource v = newCircuitElement "v-source" . EnergySourceElement . VoltageSource $ v

newCurrentSource :: a -> CircuitElement a
newCurrentSource i = newCircuitElement "c-source" $ EnergySourceElement $ CurrentSource i

newResistor :: a -> CircuitElement a
newResistor r = newCircuitElement "r-source" $ ResistorElement $ Resistor r

--new CircuitElement in Circuit
addCircuitElement :: (Num b, Num a) => Circuit a b -> CircuitElement a -> Circuit a b
addCircuitElement (Circuit {elements=elems}) elem =
   Circuit $ (elem, newDrawData) : elems


addTerminal :: Terminal a -> CircuitElement a -> Terminal a
addTerminal terminal element = Terminal (element : (terminals terminal))

--idea: circuit is a dictionary String -> CircuitElement


--connectElements a b aTerm bTerm
--aTerm/bTerm is Int which is either 1 or 2

connectElements :: CircuitElement a -> CircuitElement a -> Int -> Int -> (CircuitElement a, CircuitElement a)

--connectElements (CircuitElement {circuitElementName=nameA, element=elA, terminal1=t1A, terminal2=t2A})
--                (CircuitElement {circuitElementName=nameB, element=elB, terminal1=t1B, terminal2=t2B})

connectElements (CircuitElement nameA elA t1A t2A)
                (CircuitElement nameB elB t1B t2B)
                aTerm bTerm =
   connectElements' aTerm bTerm
      where connectElements' 1 1 =
               let a = (CircuitElement nameA elA (addTerminal t1A b) t2A)
                   b = (CircuitElement nameB elB (addTerminal t1B a) t2B)
               in (a, b)
            connectElements' 2 2 =
               let a = (CircuitElement nameA elA t1A (addTerminal t2A b))
                   b = (CircuitElement nameB elB t1B (addTerminal t2B a))
               in (a, b)
            connectElements' 1 2 =
               let a = (CircuitElement nameA elA (addTerminal t1A b) t2A)
                   b = (CircuitElement nameB elB t1B (addTerminal t2B a))
               in (a, b)
            connectElements' 2 1 =
               let a = (CircuitElement nameA elA t1A (addTerminal t2A b))
                   b = (CircuitElement nameB elB (addTerminal t1B a) t2B)
               in (a, b)



{-|

let circuit = newCircuit

let battery = newVoltageSource 10

let positiveWire = newWire
let negativeWire = newWire

let resistor = newResistor 2

let (battery, positiveWire) = connectElements battery positiveWire 1 1
let (resistor, positiveWire) = connectElements resistor positiveWire 1 2
let (resistor, negativeWire) = connectElements resistor negativeWire 2 1
let (battery, negativeWire) = connectElements battery negativeWire 2 2


--let battery = EnergySourceElement (VoltageSource 5)
--let resistor = ResistorElement $ Resistor 5
--let wire_pos = WireElement {terminal1=battery, terminal2=resistor}
-}


