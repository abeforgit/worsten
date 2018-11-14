module Util where

import Data.Ix

data IVec = IVec
    { x :: Integer
    , y :: Integer
    } deriving (Ord)

instance Num IVec where
    s + v = IVec (x s + x v) (y s + y v)
    s - v = IVec (x s - x v) (y s - y v)
    s * v = IVec (x s * x v) (y s * y v)
    abs v = IVec (abs $ x v) (abs $ y v)
    fromInteger x = IVec x x
    signum v = IVec (signum $ x v) (signum $ y v)

instance Eq IVec where
    s == v = (x s == x v) && (y s == y v)

instance Show IVec where
    show (IVec x y) = "<" ++ show x ++ ", " ++ show y ++ ">"

instance Ix IVec where
    range (s, v) = map (\(x, y) -> IVec y x) $ range ((y s, x s), (y v, x v))
    index (s, v) t = index ((y s, x s), (y v, x v)) (y t, x t)
    inRange (s, v) t = inRange ((y s, x s), (y v, x v)) (y t, x t)

type Direction = IVec

north :: Direction
north = IVec 0 (-1)

south :: Direction
south = IVec 0 1

east :: Direction
east = IVec 1 0

west :: Direction
west = IVec (-1) 0

dirToStr :: Direction -> String
dirToStr d
    | d == north = "N"
    | d == south = "S"
    | d == east = "E"
    | d == west = "W"
    | otherwise = ""

strToDir :: String -> Direction
strToDir "N" = north
strToDir "S" = south
strToDir "E" = east
strToDir "W" = west
