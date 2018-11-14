module Entities where

import Data.Array
import Data.Map.Strict as M (Map, (!), fromList)
import Player
import SausagePiece
import Util

data Entity
    = PlayerEnt { location :: IVec
                , player :: Player }
    | SausageEnt { location :: IVec
                 , sausagePiece :: SausagePiece }
    | Grass { location :: IVec }
    | Water { location :: IVec }
    | Grill { location :: IVec }
    deriving (Show)

data GridEntity
    = GridPlayerEnt Direction
    | GridSausagePiece Direction
                       Doneness
    | GridGrass
    | GridWater
    | GridGrill
    | Fork
    | Empty

instance Show GridEntity where
    show (GridPlayerEnt d) = dirToStr d
    show (GridSausagePiece dir don) = sausageStrings M.! (dir, don)
    show GridGrass = "+"
    show GridWater = "~"
    show GridGrill = "#"
    show Fork = "x"
    show Empty = " "

type Grid = Array (IVec, Integer) GridEntity

entitiesToString :: Integer -> Integer -> [Entity] -> String
entitiesToString w h es = gridToString $ entitiesToGrid w h es

entitiesToGrid :: Integer -> Integer -> [Entity] -> Grid
entitiesToGrid w h es =
    let g = emptyGrid w h
     in g // convertEntities es

gridToString :: Grid -> String
gridToString g = init $ concatMap parse $ assocs g
  where
    parse ((v, i), e)
        | x v == x width && i == 1 = show e ++ "\n"
        | otherwise = show e
      where
        width = fst $ snd $ bounds g

convertEntities :: [Entity] -> [((IVec, Integer), GridEntity)]
convertEntities es = es >>= convert
  where
    convert e@PlayerEnt {location = l, player = p} =
        [((l, 1), GridPlayerEnt $ playerDir p), ((getForkPos e, 1), Fork)]
    convert SausageEnt {location = l, sausagePiece = s} =
        [((l, 1), GridSausagePiece (sausageDir s) (getCurStat s))]
    convert Grass {location = l} = [((l, 0), GridGrass)]
    convert Grill {location = l} = [((l, 0), GridGrill)]
    convert Water {location = l} = [((l, 0), GridWater)]

getForkPos :: Entity -> IVec
getForkPos PlayerEnt {location = l, player = p} = l + playerDir p

emptyGrid :: Integer -> Integer -> Grid
emptyGrid w h =
    listArray
        ((IVec 0 0, 0), (IVec (w - 1) (h - 1), 1))
        [Empty | _ <- [0 .. ((w * h) * 2 - 1)]]

sausageStrings :: Map (Direction, Doneness) String
sausageStrings =
    fromList
        [ ((north, Raw), "∩")
        , ((north, Done), "∧")
        , ((north, Burnt), "⊓")
        , ((south, Raw), "∪")
        , ((south, Done), "∨")
        , ((south, Burnt), "⊔")
        , ((east, Raw), "⊃")
        , ((east, Done), ">")
        , ((east, Burnt), "⊐")
        , ((west, Raw), "⊂")
        , ((west, Done), "<")
        , ((west, Burnt), "⊏")
        ]
