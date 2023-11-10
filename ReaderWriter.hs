import Boxes
import Data.List
import Data.List.Split

readGame :: String -> Game
readGame string = (boardFinal, boxesFinal, readPlayer player, read int)
    where   [boardy, boxesy, player ,int] = lines string
            board = splitOn ";" boardy
            boxes = splitOn ";" boxesy
            boardFinal = [readEdge x | x <- board]
            boxesFinal = [readBox x | x <- boxes]

readEdge :: String -> Edge
readEdge str = ((x',y'),dir')
    where   [x,y,dir] = words str
            x' = read x
            y' = read y
            dir' = readDirection dir

readBox :: String -> Box
readBox str = ((x',y'),play')
    where   [x,y, play] = words str
            x' = read x
            y' = read y
            play' = readPlayer play


readPlayer :: String -> Player
readPlayer "P1" = P1
readPlayer "P2" = P2

readInt :: String -> Int
readInt str = read str

readDirection :: String -> Direction
readDirection "R" = Rgt
readDirection "D" = Dwn



showGame :: Game -> String --takes a game and converts the game state into a string using the unlines function.
showGame (board, boxes, player, size) =
      unlines [intercalate ";" (map showEdge board), intercalate ";" (map showBox boxes), showPlayer player, show size]
--    unlines [unwords $ map showEdge board, unwords $ map showBox boxes, showPlayer player, show size]

-- Helper functions for converting individual components to strings 
showEdge :: Edge -> String
showEdge ((x, y), dir) = unwords [show x, show y, showDirection dir]

showBox :: Box -> String
showBox ((x, y), player) = unwords [show x, show y, showPlayer player]

showPlayer :: Player -> String -- I now realise we have a getPlayer
showPlayer P1 = "P1"
showPlayer P2 = "P2"

showDirection :: Direction -> String
showDirection Rgt = "R"
showDirection Dwn = "D"


-- IO action to write a game state to a file
writeGame :: Game -> FilePath -> IO () -- takes a game than converts it into a file, writeFile: An IO action that writes the content to a file. filePath: The file path where the game state will be stored.
writeGame game filePath = writeFile filePath (showGame game)

-- IO action to load a game state from a file
loadGame :: FilePath -> IO Game -- readFile: An IO action that reads the content of a file. readGame: Converts the string content from the file into a game state
loadGame filePath = do
    content <- readFile filePath
    return $ readGame content
    --let sampleGame2 = ([((1,2),Dwn),((2,2),Rgt)], [((1,1),P2)], P1, 3)
--loadedGame <- loadGame "testGame.txt"        writeGame sampleGame "testGame.txt"          let sampleGame = ([], [], P1, 3) 
{-
-- IO action to compute and print the best move along with the outcome it forces. Note: this is very untested the cmd should be "putBestMove loadedGame" with 
putBestMove :: Game -> IO () -- If you have a game state currentGame and you want to find and print the best move, you would call putBestMove currentGame.
putBestMove game = do
    let move = bestMove game
        outcome = findWinner (makeMove game move)
    putStrLn $ "Best Move: " ++ showEdge move
    putStrLn $ "Forces Outcome: " ++ show outcome

-- Main IO action
main :: IO ()
main = do main

-}
