module Parsing where

import Entities
import Player
import SausagePiece
import Util

testString =
    "~ ~ ~ ~ ~ ~ \n\
\~ + + + + ~ \n\
\~ +S# +∩+ ~ \n\
\~ +x# #∪+ ~ \n\
\~ + + + + ~ \n\
\~ ~ ~ ~ ~ ~ \n"

zippify :: String -> [(Integer, String)]
zippify s = zip [0 ..] $ lines s

convert :: [(Integer, String)] -> [(Integer, Integer, Char)]
convert ts = ts >>= doconvert
  where
    doconvert (x, s) = [(x, y, c) | (y, c) <- zip [0 ..] s]

parseToEnts :: String -> [Entity]
parseToEnts s = convert (zippify s) >>= doParse
  where
    doParse (x, y, c) = strToEnt (y `div` 2) x c

strToEnt :: Integer -> Integer -> Char -> [Entity]
strToEnt x y '+' = [Grass (IVec x y)]
strToEnt x y '~' = [Water (IVec x y)]
strToEnt x y '#' = [Grill (IVec x y)]
strToEnt x y 'N' = [PlayerEnt {location = IVec x y, player = Player north}]
strToEnt x y 'S' = [PlayerEnt {location = IVec x y, player = Player south}]
strToEnt x y 'E' = [PlayerEnt {location = IVec x y, player = Player east}]
strToEnt x y 'W' = [PlayerEnt {location = IVec x y, player = Player west}]
strToEnt x y '∩' =
    [ SausageEnt
          {location = IVec x y, sausagePiece = SausagePiece north Up (Raw, Raw)}
    ]
strToEnt x y '∪' =
    [ SausageEnt
          {location = IVec x y, sausagePiece = SausagePiece south Up (Raw, Raw)}
    ]
strToEnt x y '⊃' =
    [ SausageEnt
          {location = IVec x y, sausagePiece = SausagePiece east Up (Raw, Raw)}
    ]
strToEnt x y '⊂' =
    [ SausageEnt
          {location = IVec x y, sausagePiece = SausagePiece west Up (Raw, Raw)}
    ]
strToEnt x y _ = []
