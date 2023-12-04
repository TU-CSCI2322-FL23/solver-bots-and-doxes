module Solver where
import Boxes 
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe
import Data.List


--outputs best possible move from game state if it leads to a win/tie 
bestMove :: Game -> Maybe Edge 
bestMove game@(_,_,player,_) = pickMove player moveOutcomes'
      where  moves = validMoves game 
             moveOutcomes = catMaybes $ map liftMaybe [(makeMove game x, x) | x <- moves]
             moveOutcomes' = [(whoWillWin x, y) | (x,y) <- moveOutcomes]

pickMove :: Player -> [(Outcome, Edge)] -> Maybe Edge
pickMove player lst = case winningTuple of
                    Just (_,winningMove) -> Just winningMove
                    Nothing -> case findTie of 
                           Just (_, tieMove) -> Just tieMove
                           Nothing -> Nothing
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



-------Last Sprint --------
--returns number between -100 and 100, based on the amount of boxes each player has made
rateGame :: Game -> Int
rateGame game@(board, boxes, player, size)
    | isGameOver game = evaluateOutcome (findWinner game) player
    | otherwise = evaluatePosition game player


evaluateOutcome :: Maybe Outcome -> Player -> Int
evaluateOutcome (Just (Players p)) currentPlayer
    | p == P1 = 100  -- Winning the game is highly favorable
    | otherwise = -100  -- Losing the game is highly unfavorable
evaluateOutcome (Just Tie) _ = 0  -- A tie is neutral

evaluatePosition :: Game -> Player -> Int
evaluatePosition (board, boxes, player, size) currentPlayer 
    |(player1Boxes + boxesLeft > player2Boxes)&& (currentPlayer == P1) = (player1Boxes+boxesLeft)*fracConversion
    |(player2Boxes + boxesLeft > player1Boxes) && (currentPlayer == P2) =(player2Boxes+boxesLeft)*fracConversion*(-1)
    |(player1Boxes + boxesLeft > player2Boxes) && (currentPlayer == P2) =(player2Boxes+boxesLeft)*fracConversion*(-1)
    |(player2Boxes + boxesLeft > player1Boxes) && (currentPlayer == P1) =(player1Boxes+boxesLeft)*fracConversion
    |otherwise = 0
  where
    player1Boxes = length $ filter (\(_, p) -> p == P1) boxes
    player2Boxes = length $ filter (\(_, p) -> p == P2) boxes
    totalBoxes = (size-1)*(size-1)
    fracConversion = round (100/fromIntegral totalBoxes)
    boxesLeft = totalBoxes - (length boxes)

whoMightWin :: Game -> Int -> (Int, Maybe Edge)
whoMightWin game depth =
    if currentPlayer == P1
        then maximize depth game
        else minimize depth game
    where (_, _, currentPlayer, _) = game

-- The maximizing player (Player One) so if (100,Just ((1,2),Rgt)) that means P1 is being favored
maximize :: Int -> Game -> (Int, Maybe Edge)
maximize depth game
    | depth == 0 || isGameOver game = (rateGame game, Nothing)
    | otherwise =
        foldl (\acc@(maxRating, _) move ->
            let nextState = fromMaybe game (makeMove game move)
                (rating, _) = minimize (depth - 1) nextState
            in if rating > maxRating then (rating, Just move) else acc
        ) (-1000, Nothing) (validMoves game)
--The minimizing player (two)
minimize :: Int -> Game -> (Int, Maybe Edge)
minimize depth game
    | depth == 0 || isGameOver game = (rateGame game, Nothing)
    | otherwise =
        foldl (\acc@(minRating, _) move ->
            let nextState = fromMaybe game (makeMove game move)
                (rating, _) = maximize (depth - 1) nextState
            in if rating < minRating then (rating, Just move) else acc
        ) (1000, Nothing) (validMoves game)

