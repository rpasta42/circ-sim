module Circuit
( newCircuit
, setElementId
, addCircuitElement
, connectCircuitElements
)

where

import Errors
import Types

newCircuit = Circuit [] Map.Empty 0

setElementId :: Element a -> Id -> Element a
setElementId (Element { getResistance = r
                      , getCurrent = i
                      , getVoltage = v
                      , getElementType = t
                      , getConnectorNames = c
                      )
             newId
   = Element newId r i v t c


addCircuitElement :: Circuit a -> Element a -> Circuit a
addCircuitElement (Circuit { getElements = elems
                           , getConnectors = conns
                           , getLatestId = id
                           })
                  el =
   let newId = succ id
       newEl = setElementId el newId
       newElems = Map.insert newId newEl elems
   in Circuit newElems conns newId


findCircuitElement


connectCircuitElements :: Circuit a -> Id -> Id -> Either Failures a
connectCircuitElements circ id1 id2 =





