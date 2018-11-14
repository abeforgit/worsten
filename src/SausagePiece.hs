module SausagePiece where

import Util

data Side
    = Up
    | Down
    deriving (Show)

data Doneness
    = Raw
    | Done
    | Burnt
    deriving (Show, Eq, Ord)

data SausagePiece = SausagePiece
    { sausageDir :: IVec
    , side :: Side
    , status :: (Doneness, Doneness)
    } deriving (Show)

getCurStat :: SausagePiece -> Doneness
getCurStat s@SausagePiece {side = Up} = fst $ status s
getCurStat s@SausagePiece {side = Down} = snd $ status s
