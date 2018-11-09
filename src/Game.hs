module Game where
import GameBoard

data Status = Alive | Dead | Paused

data Game = Game {
                 board :: Board,
                 status :: Status}
