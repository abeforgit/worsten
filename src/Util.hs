module Util where

data IVec = IVec {
    x :: Integer,
    y :: Integer } deriving (Show, Ord)


instance Num IVec where
    s + v = IVec (x s + x v) (y s + y v)
    s - v = IVec (x s - x v) (y s - y v)
    s * v = IVec (x s * x v) (y s * y v)
    abs v = IVec (abs $ x v) (abs $ y v)
    fromInteger x = IVec x x
    signum v = IVec (signum $ x v) (signum $ y v)

instance Eq IVec where
    s == v = (x s == x v) && (y s == y v)


north :: IVec
north = IVec 0 1

south :: IVec
south = IVec 0 (-1)

east :: IVec
east = IVec (-1) 0

west :: IVec
west = IVec 1 0


