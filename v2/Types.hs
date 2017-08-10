module Types
( Connector(..), ElementType(..), Element(..), Circuit(..)
) where

import Errors
import qualified Data.Map as Map


type Id = Int
type IdMap a = Map.Map Id a

type Connector a = IdMap [Element a]

data ElementType = VoltageSource | CurrentSource | Resistor | Capacitor
   deriving (Show)

data Element a = Element
   { getElementId :: Id
   , getResistance :: a
   , getCurrent :: a
   , getVoltage :: a
   , getElementType :: ElementType
   , getConnectorName :: String
   } deriving (Show)

data Circuit a = Circuit
   { getCircuitElements :: IdMap (Element a)
   , getConnectors :: [Connector a]
   , getLatestId :: Id
   } deriving (Show)



