module Solver where
import Boxes 
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe
import Data.List

bestMove :: Game -> Maybe Edge --an edge is a move :)
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
--{-
rateGame :: Game -> Int
rateGame game@(board, boxes, player, size)
    | isGameOver game = evaluateOutcome (findWinner game) player
    | otherwise = evaluatePosition game player

evaluateOutcome :: Maybe Outcome -> Player -> Int
evaluateOutcome (Just (Players p)) currentPlayer
    | p == currentPlayer = 100  -- Winning the game is highly favorable
    | otherwise = -100  -- Losing the game is highly unfavorable
evaluateOutcome (Just Tie) _ = 0  -- A tie is neutral
evaluateOutcome Nothing _ = 0  -- Game is ongoing, no outcome yet
--so if P1 is favorable than -100 else P2 for 100
evaluatePosition :: Game -> Player -> Int
evaluatePosition (board, boxes, player, size) currentPlayer =
    currentPlayerBoxes - opponentBoxes
  where
    currentPlayerBoxes = length $ filter (\(_, p) -> p == currentPlayer) boxes
    opponentBoxes = length $ filter (\(_, p) -> p == opponent currentPlayer) boxes
--Note this uses isGameOver from boxes.hs

--}
--let game = ([], [], P1, 3)
--let game1 = ([(1,1) R], [((1, 1), P1)], P1, 3)
--let game2 = ([], [((1, 1), P1), ((1, 2), P2), ((3, 2), P2), ((1, 3), P2)], P2, 3)
--rateGame game1  -- Expected output: 100
--rateGame game2  -- Expected output: -100
{-
rateGame :: Game -> Int
rateGame (_, boxes, _, _) =
    let player1Boxes = length $ filter (\(_, player) -> player == P1) boxes
        player2Boxes = length $ filter (\(_, player) -> player == P2) boxes
    in player1Boxes - player2Boxes
--in this version the evualtation is based on the number of boxes owned by P1 and P2, positive is good for P1 and negative is good for P2
-}
whoMightWin :: Game -> Int -> (Int, Maybe Edge)
whoMightWin game depth =
    maximize depth game

-- The maximizing player (Player One)
maximize :: Int -> Game -> (Int, Maybe Edge)
maximize depth game
    | depth == 0 || isGameOver game = (rateGame game, Nothing)
    | otherwise =
        foldl (\acc@(maxRating, _) move ->
            let nextState = fromMaybe game (makeMove game move)
                (rating, _) = minimize (depth - 1) nextState
            in if rating > maxRating then (rating, Just move) else acc
        ) (-100, Nothing) (validMoves game)
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
--whoMightWin game1 3  -- Adjust the depth as needed
--let sampleGame = ([((1,1),Rgt),((1,1),Dwn),((1,2),Dwn),((3,1),Dwn),((2,2),Rgt),((1,3),Rgt),((2,3),Rgt),((2,1),Dwn),((2,1),Rgt)],[((2,1), P1)],P1,3)

--Note: this uses fromMaybe which fogarty might not like



