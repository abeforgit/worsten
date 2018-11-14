module Player where

import Util

newtype Player = Player
    { playerDir :: Direction
    }

instance Show Player where
    show Player {playerDir = d} = dirToStr d
