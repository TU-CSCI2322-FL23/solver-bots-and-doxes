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
import System.Directory


readGame :: String -> Maybe Game
readGame string =
    case lines string of
        [boardy, boxesy, player, int] -> do
            let board = splitOn ";" boardy
                boxes = splitOn ";" boxesy
            boardFinal <- sequence [readEdge x | x <- board, not $ null x]
            boxesFinal <- sequence [readBox x | x <- boxes, not $ null x]
            player' <- maybeReadPlayer player
            size' <- readMaybe int
            return (boardFinal, boxesFinal, player', size')
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



readInt :: String -> Maybe Int
readInt = read

readDirection :: String -> Maybe Direction
readDirection x
    |x == "R" = Just Rgt
    |x == "L" = Just Dwn
    |otherwise = Nothing



showGame :: Game -> Maybe String
showGame (board, boxes, player, size) = do
    edgeStrings <- mapM showEdge board
    boxStrings <- mapM showBox boxes
    playerStr <- showPlayer player
    return $ unlines [intercalate ";" edgeStrings, intercalate ";" boxStrings, playerStr, show size]

showEdge :: Edge -> Maybe String
showEdge ((x, y), dir) = do
    xStr <- showMaybe x
    yStr <- showMaybe y
    dirStr <- showDirection dir
    return $ unwords [xStr, yStr, dirStr]

showBox :: Box -> Maybe String
showBox ((x, y), player) = do
    xStr <- showMaybe x
    yStr <- showMaybe y
    playerStr <- showPlayer player
    return $ unwords [xStr, yStr, playerStr]

-- Add a helper function to handle Maybe String conversion
showMaybe :: Show a => a -> Maybe String
showMaybe x = Just (show x)

showPlayer :: Player -> Maybe String
showPlayer x
    |x == P1 = Just "P1"
    |x == P2 = Just "P2"
    |otherwise = Nothing

showDirection :: Direction -> Maybe String
showDirection x
    |x == Rgt = Just "R"
    |x == Dwn = Just "D"
    |otherwise = Nothing

-- takes a game than converts it into a file, writeFile: An IO action that writes the content to a file. filePath: The file path where the game state will be stored.
-- IO action to write a game state to a file
writeGame :: Game -> FilePath -> IO ()
writeGame board file = do
    writeFile file (show board)
    return ()

-- IO action to load a game state from a file
-- readFile: An IO action that reads the content of a file. readGame: Converts the string content from the file into a game state
loadGame :: FilePath -> IO (Maybe Game) 
loadGame file = do
  b <- doesFileExist file
  if b 
      then do
        contents <- readFile file
        return $ readMaybe contents
      else return Nothing

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