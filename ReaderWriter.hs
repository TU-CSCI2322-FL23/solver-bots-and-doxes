module ReaderWriter where
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

--loads game in from a file location
loadGame :: FilePath -> IO (Maybe Game) 
loadGame file = do
  valid_file <- doesFileExist file
  if valid_file
      then do
        contents <- readFile file
        return $ readGame contents
      else return Nothing


--takes in a string and outputs a game if it's a valid game state 
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

--helper functions for load/read game below:
readEdge :: String -> Maybe Edge
readEdge str = 
    case words str of
       [xStr,yStr,dirStr] -> 
          do x <- readMaybe xStr
             y <- readMaybe yStr
             dir <- maybeReadDirection dirStr
             Just ((x,y),dir)
       _ -> Nothing



maybeReadDirection :: String -> Maybe Direction
maybeReadDirection x
    |x == "R" = Just Rgt
    |x == "D" = Just Dwn
    |otherwise = Nothing


readBox :: String -> Maybe Box
readBox str = case words str of
    [x, y, play] -> do
        x' <- readMaybe x
        y' <- readMaybe y
        play' <- maybeReadPlayer play
        Just ((x', y'), play')
    _ -> Nothing


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



showGame :: Game -> String --takes a game and converts the game state into a string using the unlines function.
showGame (board, boxes, player, size) =
      unlines [intercalate ";" (map showEdge board), intercalate ";" (map showBox boxes), showPlayer player, show size]


showEdge :: Edge -> String
showEdge ((x, y), dir) = unwords [show x, show y, showDirection dir]

showBox :: Box -> String
showBox ((x, y), player) = unwords [show x, show y, showPlayer player]


showPlayer :: Player -> String
showPlayer P1 = "P1"
showPlayer P2 = "P2"

showDirection :: Direction -> String
showDirection Rgt = "R"
showDirection Dwn = "D"

-- takes a game than converts it into a file, writeFile: An IO action that writes the content to a file. filePath: The file path where the game state will be stored.
-- IO action to write a game state to a file
writeGame :: Game -> FilePath -> IO ()
writeGame board file = do
    writeFile file (showGame board)
    return ()

putBestMove :: Game -> IO ()
putBestMove game = do
    case (bestMove game) of 
        Just move -> 
               do case (makeMove game move) of
                    Just updatedGame ->
                        do let outcome = whoWillWin updatedGame
                           putStrLn $ "Best Move: " ++ showEdge move
                           putStrLn $ "Forces Outcome: " ++ show outcome
                    Nothing -> putStrLn "Something went wrong :("
        Nothing -> putStrLn "No move can win or tie, sorry :("



