module GameBoard where


  data Board = Board {
  grid :: [[String]]

                     }
  r1 = ["a", "b", "c"]
  r2 = ["1", "2", "3"]
  g = [r1, r2]

  b = Board g

  printBoard :: Board -> IO()
  printBoard b = putStr $ boardToString b

  boardToString :: Board -> String
  boardToString b = unlines $ map printBoardRow (grid b) 

  printBoardRow :: [String] -> String
  printBoardRow chars = concat chars
