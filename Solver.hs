module Solver where
import Boxes 
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe
import Data.List

bestMove :: Game -> Edge --an edge is a move :)
bestMove game@(_,_,player,_) = pickMove player moveOutcomes'
      where  moves = validMoves game 
             moveOutcomes = catMaybes $ map liftMaybe [(makeMove game x, x) | x <- moves]
             moveOutcomes' = [(whoWillWin x, y) | (x,y) <- moveOutcomes]

pickMove :: Player -> [(Outcome, Edge)] -> Edge
pickMove player lst = case winningTuple of
                    Just (_,winningMove) -> winningMove
                    Nothing -> case findTie of 
                           Just (_, tieMove) -> tieMove
                           Nothing -> snd $ head lst --booo nooo dont use head WHO CARES
                    where  winningTuple = find (\(x,y) -> x==(Players player)) lst
                           findTie = find (\(x,y) -> x==Tie) lst

liftMaybe :: (Maybe a, b) -> Maybe(a,b)
liftMaybe (Just a,b) = Just (a,b)
liftMaybe (Nothing, b) = Nothing

whoWillWin :: Game -> Outcome
whoWillWin game@(_,_,player,size) = case (findWinner game) of 
                            Just guy -> guy
                            Nothing -> pickOutcome player (map whoWillWin lst)
                                   where  lst = catMaybes [makeMove game x | x <- validMoves game]


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