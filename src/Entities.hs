module Entities where
import           Data.Map.Strict
import           Util


--class Entity a where
--    occupies :: a -> [IVec]

north :: IVec
north = IVec 0 1

south :: IVec
south = IVec 0 (-1)

east :: IVec
east = IVec (-1) 0

west :: IVec
west = IVec 1 0

sausageStrings :: Map (IVec, Doneness) String
sausageStrings = fromList [
    ((north, Raw), "∩"),
    ((north, Done), "∧"),
    ((north, Burnt), "⊓"),
    ((south, Raw), "∪"),
    ((south, Done), "∨"),
    ((south, Burnt), "⊔"),
    ((east, Raw), "⊃"),
    ((east, Done), ">"),
    ((east, Burnt), "⊐"),
    ((west, Raw), "⊂"),
    ((west, Done), "<"),
    ((west, Burnt), "⊏")
    ]





data Side = Up | Down
data Doneness = Raw | Done | Burnt deriving (Eq, Ord)

data Player = Player {
        playerLoc:: IVec,
        playerDir:: IVec
    }

data Sausage = Sausage {
        sausageLoc:: IVec,
        sausageDir:: IVec,
        side       :: Side,
        status     :: (Doneness, Doneness)
    }
data Entity =
    PlayerEnt Player
    | SausageEnt Sausage
    | Grass {
        entityLoc:: IVec
    }
    | Water {
        enitityLoc:: IVec
    }
    | Grill {
        entityLoc:: IVec
    }

data GridEntity =
    GridPlayerEnt IVec
    | SausagePiece IVec Doneness
    | GridGrass
    | GridWater
    | GridGrill
    | Fork

instance Show Player where
    show p
        | playerDir p == north = "N"
        | playerDir p == south = "S"
        | playerDir p == east = "E"
        | playerDir p == west = "W"
        | otherwise = ""

instance Show GridEntity where
    show (GridPlayerEnt d) = pDirShow d
        where pDirShow d
                | d == north = "N"
                | d == south = "S"
                | d == east = "E"
                | d == west = "W"
    show (SausagePiece dir don) = sausageStrings ! (dir, don)
    show GridGrass = "+"
    show GridWater = "~"
    show GridGrill = "#"
    show Fork = "x"


--instance Entity Player where
--    occupies p = [location p, forkLocation]
--        where forkLocation = location p + direction p
