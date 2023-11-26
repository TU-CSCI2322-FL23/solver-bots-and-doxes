module Main where
import ReaderWriter
import Boxes
import Data.List
import Data.List.Split
import Solver
import Data.Maybe
import System.Environment
--import Debug.Trace
--import Text.Read
--import GHC.Generics (R, D)
--import Distribution.Compat.Lens (_1)
--import System.Directory

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