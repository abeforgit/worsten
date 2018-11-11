module Util where
import Data.Ix

data IVec = IVec {
    x :: Integer,
    y :: Integer } deriving (Ord)


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
    index (s,v) t = index ((y s, x s), (y v, x v)) (y t, x t)
    inRange (s, v) t = inRange ((y s, x s), (y v, x v)) (y t, x t)

north :: IVec
north = IVec 0 (-1)

south :: IVec
south = IVec 0 1

east :: IVec
east = IVec 1 0

west :: IVec
west = IVec (-1) 0
