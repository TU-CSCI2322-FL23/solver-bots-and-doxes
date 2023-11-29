module Main where
import ReaderWriter
import Boxes
import Data.List
import Data.List.Split
import Solver
import Data.Maybe
import Text.Read (readMaybe)
import System.Environment
import System.Console.GetOpt
--import Debug.Trace
--import Text.Read
--import GHC.Generics (R, D)
--import Distribution.Compat.Lens (_1)
--import System.Directory


data Flag = Help | Winner | Depth String | Move String | Verbose | Interactive  deriving (Eq, Show)

options :: [OptDescr Flag]
options = [ Option ['h'] ["help"] (NoArg Help) "Print usage information and exit."
          , Option ['w'] ["winner"] (NoArg Winner ) "Print best move using exhaustive search."
          , Option ['d'] ["depth"] (ReqArg Depth "<dep>") "Print best move using bounded search of depth <dep>."
          , Option ['m'] ["move"] (ReqArg Move "<move>") "Print resulting board of making move <move>"
          , Option ['v'] ["verbose"] (NoArg Verbose) "Prints bestMove and how good it is (win, tie, loss, rating, etc...)"
          , Option ['i'] ["interactive"] (NoArg Interactive) "Allows user to play against a bot"
          ]


main :: IO ()
main = 
  do args <- getArgs
     let (flags, inputs, errors) =  getOpt Permute options args
     {-let fname = case args of
                   [] -> "fortunes.txt"
                   (x:xs) -> x-}
     let fname = if null inputs then "blankGame.txt" else head inputs
     if Help `elem` flags
     then putStrLn $ usageInfo "Bots and Doxes [options] [file]" options
     else 
        do  contents <- (readFile fname)
            gamer <- readGame contents --Story five stuff hereish 
            case gamer of 
                Just game -> do if Winner `elem` flags
                                then case bestMove game of
                                    Just edge -> putStrLn (showEdge edge)
                                    Nothing -> putStrLn "No way to force a win or tie, better luck next time!"
                                else putStrLn "poo"
                Nothing -> putStrLn "Error: Game Input wrong :("









--Neil purgatory 
--main = do
--    args <- getArgs
--    case args of
--        [filePath] -> 
--            do  game <- loadGame filePath
--                case game of
--                    Just loadedGame ->
--                        do putStrLn "Loaded Game State:"
--                           prettyPrint loadedGame
--                           putStrLn "Computing and Printing Best Move:"
--                           putBestMove loadedGame
--                    Nothing -> putStrLn "Invalid file path."