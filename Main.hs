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
        do  gamer <- loadGame fname --Story five stuff hereish 
            case gamer of 
                Just game -> do if Winner `elem` flags
                                then case bestMove game of
                                    Just edge -> putStrLn (showEdge edge)
                                    Nothing -> putStrLn "No way to force a win or tie, better luck next time!"
                                else case moveInFlags flags of
                                        Just str -> case readEdge str of
                                            Just edge -> case makeMove game edge of
                                                Just game' -> do if Verbose `elem` flags
                                                                 then prettyPrint game'
                                                                 else putStrLn $ showGame game'
                                                Nothing -> putStrLn "Invalid move."
                                            Nothing -> putStrLn "Invalid input format."
                                        Nothing -> case snd (whoMightWin game (depthInFlags flags)) of
                                                        Just edge -> putStrLn $ showEdge edge
                                                        Nothing -> putStrLn "No way to force a win or tie, better luck next time!"
                                --if Interactive `elem` flags
                                --    then playGame gamer 
                                        
                Nothing -> putStrLn "Error: Game Input wrong :("

moveInFlags :: [Flag] -> Maybe String
moveInFlags [] = Nothing
moveInFlags (Move x:_) = Just x
moveInFlags (f:fs) = moveInFlags fs

depthInFlags :: [Flag] -> Int
depthInFlags [] = 5
depthInFlags (Depth x:_) = read x
depthInFlags (f:fs) = depthInFlags fs



 --makeMoveTestUserInput :: IO ()
-- makeMoveTestUserInput = do
--   let initialGame = ([], [], P1, 3)
 --  playGame initialGame

--  playGame :: Game -> IO ()
--  playGame game = do
--    move <- getUserMove
--    let updatedGame = makeMove game move
--    putStrLn "Current game state:"
--    print (findWinner updatedGame)
--    prettyPrint updatedGame
--    if isGameOver updatedGame
--      then putStrLn "Game over!"
--      else playGame updatedGame








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