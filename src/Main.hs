import Entities
import Game
import Parsing
import System.Environment (getArgs)
import Util

main :: IO ()
main = do
    (boardFile:steps) <- getArgs
    boardString <- readFile boardFile
    let h = fromIntegral $ length $ lines boardString
        w = fromIntegral $ length $ head $ lines boardString
     in if null steps
            then putStrLn "Spelletjes spelen en zo"
            else putStr . unlines . map (entitiesToString w h) $
                 scanl
                     (flip walk)
                     (parseToEnts boardString)
                     (map strToDir steps)

-- Hieronder volgen enkele definities om duidelijk te maken hoe bovenstaande code werkt.
-- Je wil deze waarschijnlijk gewoon wegsmijten en vervangen door iets nuttig.
walk :: Direction -> [Entity] -> [Entity]
walk = undefined
