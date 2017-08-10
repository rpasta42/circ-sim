module Errors
( Failure(..), Failures
, Err1AddCircuitElement(..)
)

where

data Err1AddCircuitElement = Err1CantFindElement | Err1AlreadyConnected

data Failure = Failure1AddCircuitElement Err1AddCircuitElement
               | OtherError String

type Failures = [Failure]


data Result a b = ResultOk a | ResultErr Failures
newtype Result1 b a = Result a b

instance Monoid Result1 where
   mempty = ResultErr []
   mappend (ResultErr a) (



