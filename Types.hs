module Types
( Point
) where

data Point a = Point { posX :: a
                     , posY :: a
                     } deriving (Show)

