module Solver where
import Boxes 
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe
import Data.List
import Debug.Trace
import Data.Ord (comparing)

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
    | (player1Boxes + boxesLeft > player2Boxes) && (currentPlayer == P1) = (player1Boxes + boxesLeft) * fracConversion
    | (player2Boxes + boxesLeft > player1Boxes) && (currentPlayer == P2) = (player2Boxes + boxesLeft) * fracConversion * (-1)
    | (player1Boxes + boxesLeft > player2Boxes) && (currentPlayer == P2) = (player2Boxes + boxesLeft) * fracConversion * (-1)
    | (player2Boxes + boxesLeft > player1Boxes) && (currentPlayer == P1) = (player1Boxes + boxesLeft) * fracConversion
    | otherwise = 0
  where
    player1Boxes = length $ filter (\(_, p) -> p == P1) boxes
    player2Boxes = length $ filter (\(_, p) -> p == P2) boxes
    totalBoxes = (size-1)*(size-1)
    fracConversion = round (100/fromIntegral totalBoxes)
    boxesLeft = totalBoxes - length boxes

--Checks to see which player might win and gives back the reccomended move base on depth given
whoMightWin :: Game -> Int -> (Int, Maybe Edge)
whoMightWin game depth 
    | currentPlayer == P1 = maximize depth game
    | otherwise = minimize depth game
    where (_, _, currentPlayer, _) = game

--checks to see if box would go to player
boxClaimingMove :: Edge -> Game -> Bool
boxClaimingMove move game =
    let nextState = fromMaybe game (makeMove game move)
        (_, _, currentPlayer, _) = nextState
        claimedBoxes = filter (\(_, owner) -> owner == currentPlayer) (getBoxes nextState)
    in length claimedBoxes > length (getBoxes game)
--get boxes out of game because Im not doing a where ([],[],_,_) = game again
getBoxes :: Game -> [(Point, Player)]
getBoxes (_, boxes, _, _) = nub boxes
-- The maximizing player (Player One) so if (100,Just ((1,2),Rgt)) that means P1 is being favored
maximize :: Int -> Game -> (Int, Maybe Edge)
maximize depth game
    | depth == 0 || isGameOver game = {-trace ("Maximized: " ++ show (rateGame game))-} (rateGame game, Nothing)
    | otherwise =
        let moves = validMoves game
            ratingsAndMoves = map 
                (\move ->
                    let nextState = fromMaybe game (makeMove game move)
                        (rating, _) = minimize (depth - 1) nextState
                    in {-trace ("Maximizing: " ++ show move ++ " with rating " ++ show rating)-} (rating, move)
                ) moves
            boxClaimingMoves = filter (\(_, move) -> boxClaimingMove move game) ratingsAndMoves
        in case boxClaimingMoves of
            [] -> --trace ("No box-claiming moves. Choosing highest rated move.") $
                foldl (\acc@(maxRating, _) (rating, move) ->
                    if rating > maxRating then (rating, Just move) else acc
                ) (-1000, Nothing) ratingsAndMoves
            _  -> --trace ("Choosing box-claiming move with the highest rating.") $
                maximumBy (comparing fst) (map (\(rating, move) -> (rating, Just move)) boxClaimingMoves)

--The minimizing player (two)
minimize :: Int -> Game -> (Int, Maybe Edge)
minimize depth game
    | depth == 0 || isGameOver game = {-trace ("Minimized: " ++ show (rateGame game))-} (rateGame game, Nothing)
    | otherwise =
        let moves = validMoves game
            ratingsAndMoves = map 
                (\move ->
                    let nextState = fromMaybe game (makeMove game move)
                        (rating, _) = maximize (depth - 1) nextState
                    in {-trace ("Minimizing: " ++ show move ++ " with rating " ++ show rating)-} (rating, move)
                ) moves
            boxClaimingMoves = filter (\(_, move) -> boxClaimingMove move game) ratingsAndMoves
        in case boxClaimingMoves of
            [] -> --trace ("No box-claiming moves. Choosing lowest rated move.") $
                foldl (\acc@(minRating, _) (rating, move) ->
                    if rating < minRating then (rating, Just move) else acc
                ) (1000, Nothing) ratingsAndMoves
            _  -> --trace ("Choosing box-claiming move with the lowest rating.") $
                minimumBy (comparing fst) (map (\(rating, move) -> (rating, Just move)) boxClaimingMoves)
--Finally done