import Boxes 
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe
import Data.List

whoWillWin :: Game -> Outcome
whoWillWin game@(_,_,player,size) = case (findWinner game) of 
                            Just guy -> guy
                            Nothing -> pickOutcome player (map whoWillWin lst)
                                   where  moves = allPossibleEdges size
                                          lst = catMaybes [makeMove game x | x <- moves]


pickOutcome :: Player -> [Outcome] -> Outcome
pickOutcome player lst 
       | Players player `elem` lst = (Players player)
       | Tie `elem` lst = Tie
       | otherwise = Players (opponent player)


allPossibleEdges :: Int -> [Edge]
allPossibleEdges size = rgts ++ dwns 
  where  rgts = [((x,y), Rgt) | x <- [1..size-1], y <- [1..size]]
         dwns = [((x,y), Dwn) | x <- [1..size], y<-[1..size-1]]

validMoves :: Game -> [Edge]  
validMoves (board,_,_,size) = allPossibleEdges size \\ board