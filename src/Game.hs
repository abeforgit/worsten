module Game where
import           Control.Monad.State.Lazy
import           GameBoard                (Board)
import Entities

data Status = Alive | Dead | Paused


data GameState = GameState {
    entities :: [Entity],
    status :: Status }
