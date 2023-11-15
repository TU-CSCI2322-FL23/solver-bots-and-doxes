import Boxes
import Data.List
import Data.List.Split
import Solver
import Data.Maybe
import System.Environment
import Debug.Trace
import Text.Read
import GHC.Generics (R, D)
import Distribution.Compat.Lens (_1)


readGame :: String -> Maybe Game
readGame string =
    case lines string of
        [boardy, boxesy, player, int] ->
            let board = splitOn ";" boardy
                boxes = splitOn ";" boxesy
                boardFinal = catMaybes [readEdge x | x <- board, not $ null x]
                boxesFinal = catMaybes [readBox x | x <- boxes, not $ null x]
    
            in (boardFinal, boxesFinal, readPlayer player, read int)
        _ -> error $ "Invalid file: " ++ show (lines string)
readEdge :: String -> Maybe Edge
readEdge str = aux ((x',y'),dir')
      where [x,y,dir] = words str
            x' = readMaybe x
            y' = readMaybe y
            dir' = maybeReadDirection dir
            aux :: ((Maybe Int,Maybe Int), Maybe Direction) -> Maybe Edge
            aux ((Just x, Just y),Just z) = Just ((x,y),z) 
            aux _ = Nothing

maybeReadDirection :: String -> Maybe Direction
maybeReadDirection x 
    |x == "R" = Just Rgt
    |x == "D" = Just Dwn 
    |otherwise = Nothing


readBox :: String ->Maybe Box
readBox str =    
    case words str of
        [x,y, play] -> 
            do  x' <- readMaybe x
                y' <- readMaybe y
                play' <- maybeReadPlayer play
                Just ((x',y'),play')
        lst -> traceShow lst Nothing


maybeReadPlayer :: String -> Maybe Player
maybeReadPlayer x 
    |x == "P1" = Just P1
    |x == "P2" = Just P2 
    |otherwise = Nothing

readPlayer :: String -> Maybe Player
readPlayer "P1" = P1
readPlayer "P2" = P2

readInt :: String -> Maybe Int
readInt str = read str

readDirection :: String -> Maybe Direction
readDirection "R" = Rgt
readDirection "D" = Dwn



showGame :: Maybe Game -> String --takes a game and converts the game state into a string using the unlines function.
showGame (board, boxes, player, size) =
      unlines [intercalate ";" (map showEdge board), intercalate ";" (map showBox boxes), showPlayer player, show size]
--    unlines [unwords $ map showEdge board, unwords $ map showBox boxes, showPlayer player, show size]

-- Helper functions for converting individual components to strings 
showEdge :: Maybe Edge -> String
showEdge ((x, y), dir) = unwords [show x, show y, showDirection dir]

showBox :: Maybe Box -> String
showBox ((x, y), player) = unwords [show x, show y, showPlayer player]

showPlayer :: Maybe Player -> String -- I now realise we have a getPlayer
showPlayer P1 = "P1"
showPlayer P2 = "P2"

showDirection :: Maybe Direction -> String
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
    --let sampleGame = ([((1,1),Rgt),((1,1),Dwn),((1,2),Dwn),((3,1),Dwn),((2,2),Rgt),((1,3),Rgt),((2,3),Rgt),((2,1),Dwn)],[],P1,3)
--loadedGame <- loadGame "testGame.txt"        writeGame sampleGame "testGame.txt"          let sampleGame = ([], [], P1, 3) 
--putBestMove loadedGame
--putBestMove (ANY GAME)
putBestMove :: Game -> IO ()
putBestMove game = do
    let move = bestMove game
        updatedGame = fromMaybe game (makeMove game move)
        outcome = findWinner updatedGame
    putStrLn $ "Best Move: " ++ showEdge move
    putStrLn $ "Forces Outcome: " ++ show outcome
{-
-- IO action to compute and print the best move along with the outcome it forces. Note: this is very untested the cmd should be "putBestMove loadedGame" with 
putBestMove :: Game -> IO () -- If you have a game state currentGame and you want to find and print the best move, you would call putBestMove currentGame.
putBestMove game = do
    let move = bestMove game
        outcome = findWinner (makeMove game move)
    putStrLn $ "Best Move: " ++ showEdge move
    putStrLn $ "Forces Outcome: " ++ show outcome
-}
-- :main testGame.txt
-- Main IO action
main :: IO ()
main = do
    args <- getArgs
    case args of
        [filePath] -> do
            loadedGame <- loadGame filePath
            putStrLn "Loaded Game State:"
            prettyPrint loadedGame
            putStrLn "Computing and Printing Best Move:"
            putBestMove loadedGame
        _ -> putStrLn "Please provide a file path as an argument."