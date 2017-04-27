module Types
( Point(Point, posX, posY)
) where

data Point a = Point { posX :: a
                     , posY :: a
                     } deriving (Show)

