module Entities where
import           Data.Array
import           Data.Map.Strict as M (Map, fromList, (!))
import           Util

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


data Side = Up | Down deriving (Show)

data Doneness = Raw | Done | Burnt deriving (Show, Eq, Ord)

newtype Player = Player {
        playerDir:: IVec
    }

data SausagePiece = SausagePiece {
        sausageDir :: IVec,
        side       :: Side,
        status     :: (Doneness, Doneness)
    } deriving (Show)
data Entity =
    PlayerEnt {
        location :: IVec,
        player   :: Player
    }
    | SausageEnt {
        location :: IVec,
        sausagePiece :: SausagePiece
    }
    | Grass {
        location:: IVec
    }
    | Water {
        location:: IVec
    }
    | Grill {
        location:: IVec
    } deriving (Show)
type Grid = Array (IVec, Integer) GridEntity

data GridEntity =
    GridPlayerEnt IVec
    | GridSausagePiece IVec Doneness
    | GridGrass
    | GridWater
    | GridGrill
    | Fork
    | Empty

instance Show Player where
    show p
        | playerDir p == north = "N"
        | playerDir p == south = "S"
        | playerDir p == east = "E"
        | playerDir p == west = "W"
        | otherwise = ""

instance Show GridEntity where
    show (GridPlayerEnt d)      = pDirShow d
    show (GridSausagePiece dir don) = sausageStrings M.! (dir, don)
    show GridGrass              = "+"
    show GridWater              = "~"
    show GridGrill              = "#"
    show Fork                   = "x"
    show Empty                  = " "
testGrid = entitiesToGrid 5 5 
    [Grass $ IVec 0 0, 
    Water $ IVec 0 1, 
    PlayerEnt{location = IVec 1 3, player = Player {playerDir = south}},
    SausageEnt{location = IVec 2 3, sausagePiece = 
        SausagePiece north Up (Raw, Done)}]
gridToString :: Grid -> String
gridToString g = init $ concatMap parse $ assocs g
    where parse ((v, i), e)
            | x v == x width && i == 1 = show e ++ "\n"
            | otherwise = show e
                where width = fst $ snd $ bounds g

pDirShow :: IVec -> String
pDirShow d
    | d == north = "N"
    | d == south = "S"
    | d == east = "E"
    | d == west = "W"

getForkPos :: Entity -> IVec
getForkPos PlayerEnt {location = l, player = p}
    = l + playerDir p

emptyGrid :: Integer -> Integer -> Grid
emptyGrid w h = listArray ((IVec 0 0, 0), (IVec (w-1) (h-1),1))
    [Empty | _ <- [0 .. ((w * h)*2 - 1)]]

convertEntities :: [Entity] -> [((IVec, Integer), GridEntity)]
convertEntities es = es >>= convert
        where convert e@PlayerEnt {location = l, player = p} =
                [((l, 1), GridPlayerEnt $ playerDir p), 
                ((getForkPos e, 1), Fork)]
              convert SausageEnt {location = l, sausagePiece = s} =
                [((l, 1), GridSausagePiece (sausageDir s) (getCurStat s))] 
              convert Grass {location = l} = 
                [((l, 0), GridGrass)]
              convert Grill {location = l} =
                [((l, 0), GridGrill)]
              convert Water {location = l} =
                [((l, 0), GridWater)]

getCurStat :: SausagePiece -> Doneness
getCurStat s@SausagePiece {side = Up} = fst $ status s
getCurStat s@SausagePiece {side = Down} = snd $ status s

entitiesToGrid :: Integer -> Integer -> [Entity] -> Grid
entitiesToGrid w h es = let g = emptyGrid w h
    in g // convertEntities es

tupToStr :: (GridEntity, GridEntity) -> String
tupToStr (a, b) = show a ++ show b

rowToStr :: [(GridEntity, GridEntity)] -> String
rowToStr =  concatMap tupToStr
