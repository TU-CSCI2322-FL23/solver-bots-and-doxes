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


data Flag = Help | Winner | Depth String | Move String | Verbose | Interactive | deriving (Eq, Show)

options :: [OptDescr Flag]
options = [ Option ['h'] ["help"] (NoArg Help) "Print usage information and exit."
          , Option ['w'] ["winner"] (NoArg Winner ) "Print best move using exhaustive search."
          , Option ['d'] ["depth"] (ReqArg Depth "<dep>") "Print best move using bounded search of depth <dep>."
          , Option ['m'] ["move"] (ReqArg Mpve "<move>") "Print resulting board of making move <move>"
          , Option ['v'] ["verbose"] (NoArg Verbose) "Prints bestMove and how good it is (win, tie, loss, rating, etc...)"
          ]


main :: IO ()
main = do
    args <- getArgs
    case args of
        [filePath] -> 
            do  game <- loadGame filePath
                case game of
                    Just loadedGame -> 
                        do putStrLn "Loaded Game State:"
                           prettyPrint loadedGame
                           putStrLn "Computing and Printing Best Move:"
                           putBestMove loadedGame
                    Nothing -> putStrLn "Invalid file path."