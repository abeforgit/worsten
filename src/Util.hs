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
