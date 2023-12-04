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
import System.IO

data Flag = Help | Winner | Depth String | Move String | Verbose | Interactive  deriving (Eq, Show)

options :: [OptDescr Flag]
options = [ Option ['h'] ["help"] (NoArg Help) "Print usage information and exit."
          , Option ['w'] ["winner"] (NoArg Winner ) "Print best move using exhaustive search."
          , Option ['d'] ["depth"] (ReqArg Depth "<dep>") "Print best move using bounded search of depth <dep>."
          , Option ['m'] ["move"] (ReqArg Move "<move>") "Print resulting board of making move <move>"
          , Option ['v'] ["verbose"] (NoArg Verbose) "Prints bestMove and rates it on a scale of -100 to 100. "
          , Option ['i'] ["interactive"] (NoArg Interactive) "Allows user to play against Dot, the dots and boxes bot."
          ]


main :: IO ()
main = 
  do args <- getArgs
     let (flags, inputs, errors) =  getOpt Permute options args
     let fname = if null inputs then "blankGame.txt" else head inputs
     if Help `elem` flags
     then putStrLn $ usageInfo "Bots and Doxes [options] [file]" options
     else
        do  gamer <- loadGame fname 
            case gamer of 
                Just game -> do if Interactive `elem` flags
                                then playGame game (depthInFlags flags)
                                else if Winner `elem` flags
                                            then case bestMove game of
                                                Just edge -> putStrLn (showEdge edge)
                                                Nothing -> putStrLn "No way to force a win or tie, better luck next time!"
                                            else case moveInFlags flags of
                                                    Just str -> case readEdge str of
                                                        Just edge -> case makeMove game edge of
                                                            Just game' -> do if Verbose `elem` flags
                                                                             then do prettyPrint game'
                                                                                     putStrLn "Game rating of:" 
                                                                                     putStrLn (show $ rateGame game')
                                                                             else putStrLn $ showGame game'
                                                            Nothing -> putStrLn "Invalid move."
                                                        Nothing -> putStrLn "Invalid input format."
                                                    Nothing -> case snd (whoMightWin game (depthInFlags flags)) of
                                                                    Just edge -> putStrLn $ showEdge edge
                                                                    Nothing -> putStrLn "No way to force a win or tie, better luck next time!"                       
                Nothing -> putStrLn "Error: Game Input wrong :("


--helper functions 
moveInFlags :: [Flag] -> Maybe String
moveInFlags [] = Nothing
moveInFlags (Move x:_) = Just x
moveInFlags (f:fs) = moveInFlags fs

--default value is 5 if no depth specified (also functions as difficulty for the bot)
depthInFlags :: [Flag] -> Int
depthInFlags [] = 5
depthInFlags (Depth x:_) = read x
depthInFlags (f:fs) = depthInFlags fs

showOutcome :: Outcome -> String
showOutcome Tie = "It's a tie!"
showOutcome (Players P1) = "User wins!!!"
showOutcome (Players P2) = "Dot wins!!!"

--P1 is the player, P2 is the bot
playGame :: Game -> Int -> IO()
playGame game@(_,_,P2,_) d =  
    if isGameOver game
    then do case findWinner game of   
                Just p -> putStrLn (showOutcome p)                           
                Nothing -> putStrLn "Error: Please restart the module"
    else do case snd $ whoMightWin game d of 
                        Just edge -> case makeMove game edge of
                            Just game -> do putStrLn "Dot's move:"
                                            prettyPrint game
                                            playGame game d
                            Nothing -> putStrLn "Error: Please restart the module"
                        Nothing -> case makeMove game (head $ validMoves game) of
                                        Just game -> do putStrLn "Dot's move:"
                                                        prettyPrint game
                                                        playGame game d
                                        Nothing -> putStrLn "Error: Please restart the module"
playGame game@(_,_,P1,_) d =  
    if isGameOver game
    then do case findWinner game of   
                Just p -> putStrLn (showOutcome p)   
                Nothing -> putStrLn "Error: Please restart the module"
    else do move <- prompt "Enter your move:"
            case (readEdge move) of 
                Nothing -> do putStrLn "Invalid input format, please try again."
                              playGame game d
                Just move -> case makeMove game move of
                    Nothing -> do putStrLn "Invalid move, please try again."
                                  playGame game d
                    Just game -> do putStrLn "Your move:"
                                    prettyPrint game
                                    playGame game d

--takes in the users move 
prompt :: String -> IO String
prompt question = 
  do putStr (question++" ")
     hFlush stdout
     resp <- getLine
     return resp
