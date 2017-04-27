module Types
( Point(Point, posX, posY)
, newPoint
) where

data Point a = Point { posX :: a
                     , posY :: a
                     } deriving (Show)

newPoint :: a -> a -> Point a
newPoint a b = Point a b
