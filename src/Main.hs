
import System.Environment (getArgs)

main :: IO ()
main = do (boardFile:steps) <- getArgs
          boardString       <- readFile boardFile
          if null steps
            then putStrLn "Spelletjes spelen en zo"
            else putStr . unlines . map printBoard $ scanl (flip walk) (parseBoard boardString) (map parseDir steps)

-- Hieronder volgen enkele definities om duidelijk te maken hoe bovenstaande code werkt.
-- Je wil deze waarschijnlijk gewoon wegsmijten en vervangen door iets nuttig.

type Board = ()
type Direction = ()

parseDir :: String -> Direction
parseDir = undefined

parseBoard :: String -> Board
parseBoard = undefined

printBoard :: Board -> String
printBoard = undefined

walk :: Direction -> Board -> Board
walk = undefined

