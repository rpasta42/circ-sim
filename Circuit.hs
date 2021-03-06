module Circuit
( Terminal(Terminal, TerminalByName, terminals, terminalNames)
, CircuitElement(CircuitElement, circuitElementName, element, terminal1, terminal2)
, Element(EnergySourceElement, ResistorElement, WireElement)
, DrawData(DrawData, positions)
, Circuit(Circuit, elements)
, newTerminal, newCircuit, newDrawData, newCircuitElement
, newWire, newVoltageSource, newCurrentSource, newResistor
, newWireNamed, newVoltageSourceNamed, newCurrentSourceNamed, newResistorNamed
, addCircuitElement, addCircuitElements, addTerminal, addTerminalByName
, connectElements, connectElementsByName
) where

import Types
import Data.List


data Terminal a = Terminal { terminals :: [CircuitElement a] }
                | TerminalByName { terminalNames :: [String] }

instance Show (Terminal a) where
   show (Terminal []) = "none"
   show (Terminal terminals) = intercalate "\n, " $ map circuitElementName terminals

   show (TerminalByName []) = "none"
   show (TerminalByName terminals) = intercalate "\n, " $ terminals


--later can add thickness/etc
data Wire a = Wire deriving (Show)

data EnergySource a = VoltageSource { voltage :: a }
                    | CurrentSource { current :: a }
                      deriving (Show)

data Resistor a = Resistor { resistance :: a } deriving (Show)

data Splitter = Splitter deriving (Show)

data Element a = EnergySourceElement { source :: EnergySource a }
               | ResistorElement { resistor :: Resistor a }
               | WireElement { wire :: Wire a }
               | SplitterElement { splitter :: Splitter }
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


newWire :: String -> CircuitElement a
newWire name = newCircuitElement name $ WireElement $ Wire
newWireNamed = newWire "wire"

newVoltageSource :: String -> a -> CircuitElement a
newVoltageSource name v = newCircuitElement name . EnergySourceElement . VoltageSource $ v
newVoltageSourceNamed = newVoltageSource "v-source"

newCurrentSource :: String -> a -> CircuitElement a
newCurrentSource name i = newCircuitElement name $ EnergySourceElement $ CurrentSource i
newCurrentSourceNamed = newCurrentSource "c-source"

newResistor :: String -> a -> CircuitElement a
newResistor name r = newCircuitElement name $ ResistorElement $ Resistor r
newResistorNamed = newResistor "resistor"

--new CircuitElement in Circuit
addCircuitElement :: (Num b, Num a) => Circuit a b -> CircuitElement a -> Circuit a b
addCircuitElement (Circuit {elements=elems}) elem =
   Circuit $ (elem, newDrawData) : elems

addCircuitElements :: (Num b, Num a) => Circuit a b -> [CircuitElement a] -> Circuit a b
addCircuitElements c [] = c
addCircuitElements c (x:xs) = addCircuitElements (addCircuitElement c x) xs

addTerminal :: Terminal a -> CircuitElement a -> Terminal a
addTerminal terminal element = Terminal (element : (terminals terminal))

addTerminalByName :: Terminal a -> String -> Terminal a
addTerminalByName terminal elementName = TerminalByName $ elementName : (terminalNames terminal)

--idea: circuit is a dictionary String -> CircuitElement

--connectElements a b aTerm bTerm
--aTerm/bTerm is Int which is either 1 or 2

--connectElements (CircuitElement {circuitElementName=nameA, element=elA, terminal1=t1A, terminal2=t2A})
--                (CircuitElement {circuitElementName=nameB, element=elB, terminal1=t1B, terminal2=t2B})


connectElements :: CircuitElement a -> CircuitElement a -> Int -> Int -> (CircuitElement a, CircuitElement a)
connectElements cElA@(CircuitElement nameA elA t1A t2A)
                cElB@(CircuitElement nameB elB t1B t2B)
                aTerm bTerm =
   connectElements' aTerm bTerm
      where connectElements' 1 1 =
               let a = (CircuitElement nameA elA (addTerminal t1A b) t2A)
                   b = cElB --(CircuitElement nameB elB (addTerminal t1B a) t2B)
               in (a, b)
            connectElements' 2 2 =
               let a = (CircuitElement nameA elA t1A (addTerminal t2A b))
                   b = cElB --(CircuitElement nameB elB t1B (addTerminal t2B a))
               in (a, b)
            connectElements' 1 2 =
               let a = (CircuitElement nameA elA (addTerminal t1A b) t2A)
                   b = cElB --(CircuitElement nameB elB t1B (addTerminal t2B a))
               in (a, b)
            connectElements' 2 1 =
               let a = (CircuitElement nameA elA t1A (addTerminal t2A b))
                   b = cElB --(CircuitElement nameB elB (addTerminal t1B a) t2B)
               in (a, b)


{-
connectElements :: CircuitElement a -> CircuitElement a -> Int -> Int -> (CircuitElement a, CircuitElement a)
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
-}

connectElementsByName (CircuitElement nameA elA t1A t2A)
                      (CircuitElement nameB elB t1B t2B)
                      aTerm bTerm =
   connectElements' aTerm bTerm
      where connectElements' 1 1 =
               let a = (CircuitElement nameA elA (addTerminalByName t1A nameB) t2A)
                   b = (CircuitElement nameB elB (addTerminalByName t1B nameA) t2B)
               in (a, b)
            connectElements' 2 2 =
               let a = (CircuitElement nameA elA t1A (addTerminalByName t2A nameB))
                   b = (CircuitElement nameB elB t1B (addTerminalByName t2B nameA))
               in (a, b)
            connectElements' 1 2 =
               let a = (CircuitElement nameA elA (addTerminalByName t1A nameB) t2A)
                   b = (CircuitElement nameB elB t1B (addTerminalByName t2B nameA))
               in (a, b)
            connectElements' 2 1 =
               let a = (CircuitElement nameA elA t1A (addTerminalByName t2A nameB))
                   b = (CircuitElement nameB elB (addTerminalByName t1B nameA) t2B)
               in (a, b)




