module Boxes where
import Data.List
--LETS GET THIS SHIT DONE
--list of letters for the horizontal axis, list of numbers for vertical axis (numbers start at top, then go down, top left is A1)
--was getting mad for something like Y_axis = 1|2|3|4|5
type Point = (Int, Int) --making x_axis/y_axis points lets us control size of grid through constructors for each type
data Direction = Right1 | Down1  deriving (Show, Eq) --Right1 cuz Right conflicts w/ special word        --added (Eq)
data Player = P1 | P2 deriving (Show, Eq)
type Edge =(Point, Direction)
type Box= (Point, Player)
type Board = [Edge]
type Score = (Int, Int) --CHANGED FROM [BOXES] TO (player1_score, player2_score), findScore function below 
type Boxes = [Box] --Need to keep track of boxes because player that closes box matters
type Game = (Board, Boxes, Player, Integer)

--DONT LET THEM MAKE A GAME OF SIZE 1 IT WILL NEVER END
--data Game = Game { board :: Board, boxes :: Boxes, turn :: Player, x :: Int, y :: Int }
--boooooopooppboppoa hfdlhg kjg
--pretty pretty print
opponent:: Player -> Player
opponent P1 = P2
opponent P2 = P1

amIDumb :: Point -> Point -> Point --ok 75% of the constructors weren't needed 
amIDumb p1 p2 = ((fst p1 + fst p2) ,(snd p1 + snd p2))

makePoint:: Int -> Int -> Point
makePoint x y = (x, y)

makeDirection :: String -> Direction
makeDirection "Right" = Right1 --conflicts with prelude def Right, so it's Right1. Could do lePlayer ft instead if weird naming scheme is an issue 
 
makeDirection "Down" = Down1
makeDirection _ = error "invalid direction, only Right or Down permitted"

makePlayer :: String -> Player
makePlayer "P1" = P1
makePlayer "P2" = P2

makeEdge :: Point -> Direction -> Edge --should cover cases for out of bounds moves (for 5x5 grid)
makeEdge p d = (p, d)

makeBox :: Point -> Player -> Box
makeBox point player = (point, player)

makeBoard :: [Edge] -> Board
makeBoard edges = edges

findScore :: Boxes -> Score --still takes [Box], called Boxes for consistency with gamestate tracking
findScore boxes = (length p1, length p2)
                where (p1, p2) =partition (\(_, player) -> player == P1) boxes --splits into p1's/p2's boxes 


--MOVEMENT
{-
moveHorizontal :: Point -> Maybe Edge
moveHorizontal point@(x, y)
    | x == 5 = Nothing  -- Rightmost node, can't move right there should be an error msg from above
    | otherwise = Just (makeEdge point Right1) --right1 cause thats what it was abovelist of moves
    | otherwise = Just (makeEdge point Right1) --right1 cause thats what it was above

moveVertical :: Point -> Maybe Edge
moveVertical point@(x, y)
    | y == 5 = Nothing  -- Bottom node, can't move down there should be an error msg from above
    | otherwise = Just (makeEdge point Down1) 
-}
--makeMove :: Board -> Edge -> Player -> Maybe Board
--makeMove board edge player
--    | validMove edge board = Just (edge : board) --checks if move is valid if so the just edge on board -- type Board = [Edge]
--    | otherwise = Nothing

validMove :: Edge -> Board -> Bool
validMove edge board = notElem edge board

-- What moves are legal for a game state (Game -> [Move] ). in this case Board is game and edge is move

--Noah comment: Yo this is wrong. All possible edges includes some non-legal ones. (right edges on the right end, down edges on the bottom)
legalMoves :: Board -> [Edge]
legalMoves board = filter (\edge -> validMove edge board) allPossibleEdges --aPE will have to change is we change the size of board
    where
        allPossibleEdges = [makeEdge (x, y) dir | x <- [1..4], y <- [1..4], dir <- [Right1, Down1]] --hardcoded for 1..4 board

--Board Checks

isAvailable :: Board -> Point -> Direction -> Bool
isAvailable board point direction = notElem (makeEdge point direction) board

isHorizontal :: Edge -> Bool
isHorizontal (_, direction) = direction == Right1 --right1 cause thats what it was above

isVertical :: Edge -> Bool
isVertical (_, direction) = direction == Down1

isValid :: Board -> Edge -> Bool
isValid board edge@(point, direction) =
    isWithinBounds point && isAvailable board point direction
    where
        isWithinBounds (x, y) = x /= 5 && y /= 5

updateBoard :: Board -> Edge -> Player -> Maybe Board -- maybe it should be put edge in board and if that edge meakes a square set a boolean to true?
updateBoard board edge player
    | isValid board edge = Just (edge : board)
    | otherwise = Nothing

--BUILD BOARD

--no longer necessary and also wrong
--buildBoard :: [Edge]
--buildBoard = concatMap (\point -> [makeEdge point Right1, makeEdge point Down1]) allPoints

--WINNER LOGIC

--assuming game inputed is a finished game
findWinner :: Game -> Maybe Player
findWinner (board, boxes, _, _) = if p1_score > p2_score then Just P1 else if p1_score < p2_score then Just P2 else Nothing
    where   scored = findScore boxes
            p1_score = fst scored
            p2_score = snd scored



--type Point = (Int, Int) --making x_axis/y_axis points lets us control size of grid through constructors for each type
--type Edge =(Point, Direction)


--will also probably reassign all boxes to whichever player gets passed in
--will need another helper function to deal with it

--might go outside the board
--will take in move board and 
makeBoxes :: Edge -> Game -> [Box] --this needs to be tested asap 
makeBoxes move@(point@(x, y), direc) game@(board, boxes, player, _) =
            if(checkBox point board) then
                if((y-1 > 0) && checkBox (x,y-1) board)
                    then  [(point, player), ((x,y-1), player)]
                else [(point, player)]
            else []



--Returns boolean if there's a box originating (top left point of the box) from a given point. 
checkBox :: Point -> [Edge] -> Bool 
checkBox (x, y) edge_list = (e1 `elem` edge_list) && (e2 `elem` edge_list) && (e3 `elem` edge_list) && (e4 `elem` edge_list)
    where e1 = makeEdge (makePoint x y) (makeDirection "Right")
          e2 = makeEdge (makePoint x y) (makeDirection "Down")
          e3 = makeEdge (makePoint (x+1) y) (makeDirection "Down") 
          e4 = makeEdge (makePoint x (y+1)) (makeDirection "Right")
--Build a row of edges starting at a given point

--Noah note: valid move might need to only work on (n-1) points. Should be able to get any edge u want. 
--Noah note 2: would it need to determine winner at some point?
--Takes a Game state and an edge (move) and returns the new game state. 
makeMove :: Game -> Edge -> Game
makeMove game@(board, boxes, player, int) move
  | not (validMove move board) = error "Invalid Move" --if not a valid more, throw error 
  | null newBoxes = (move:board, boxes, opponent player, int) --if list of new boxes is empty, return new game w/move added to board, same boxes, switch player
  | otherwise = (move:board, newBoxes++boxes, player, int) --if new boxes, return new game w/move added to board, the nex boxes made added to existing boxes, same player
  where
      newBoxes = makeBoxes move game

-- prettyShow :: Game -> String
-- prettyShow ([],_,_)=".   .   .   .   .\n\n"

-- prettyPrint :: Game -> IO ()
-- prettyPrint


-- Build a row of edges starting at a given point

--buildRow :: Point -> [Edge]
--buildRow startPoint = [makeEdge startPoint Right1 | x <- [startPoint..(E, y)]]
--  where (_, y) = startPoint

-- Build a column of edges starting at a given point
--buildColumn :: Point -> [Edge]
--buildColumn startPoint = [makeEdge startPoint Down | y <- [startPoint..(x, Five)]]
--  where (x, _) = startPoint

-- Build a list of all available moves (edges) on the game board
--buildAvailables :: Board -> [Edge]
--buildAvailables board = filter (\edge -> isAvailable board edge) allEdges
--  where
--    allEdges = [(point, direction) | point <- allPoints, direction <- [Right1, Down]]





-- boardToString :: Board -> String
-- boardToString b = x ++ "\n" ++ fst (foldl printBoxes ("",(0,0)) b)
--   where x = fst $ foldl printHorizontal ("*",(0,0)) $ head b

-- printBoxes :: (String,Point) -> [Box] -> (String,Point)
-- printBoxes (s,(p1,p2)) lb = (s++x++"\n"++y++"\n",(p1+1,p2))
--   where z = if ((p1,p2),(p1+1,p2)) `elem` fst (head lb) then "-" else " "
--         x = fst $ foldl printVertical (z,(p1,p2)) lb
--         y = fst $ foldl printHorizontal ("*",(p1+1,p2)) lb

-- printHorizontal :: (String,Point) -> Box -> (String,Point)
-- printHorizontal (s,(p1,p2)) (tlpoint, player) = if ((p1,p2),(p1,p2+1)) `elem` le then (s++" - *",(p1,p2+1)) else (s++"   *",(p1,p2+1))

-- printVertical :: (String,Point) -> Box -> (String,Point)
-- printVertical (s,(p1,p2)) (le,n) = if ((p1,p2+1),(p1+1,p2+1)) `elem` le then (s++" "++show n++" -",(p1,p2+1)) else (s++" "++show n++"  ",(p1,p2+1))

-- printBoard :: Board -> IO ()
-- printBoard b = do putStrLn ""
--                   putStrLn $ boardToString b
--                   putStrLn ""






-- point is a tuple of (letter location, number location)
--an edge is a (point, direction)

--Direction= Right or Down 
--Player = P1 | P2
--Turn is type alias for player. 
--Box type? : (Player, Point) --assume top right point 
--Board = [edges]
 -- score = [boxes]
 --game is won once the length of score = (board length-1)^2
 --Box math : (x, y) (x+1, y)   idk what this is check picture 
            -- (x, y+1) (x+1, y+1) 

    --priorities for sprint 1:
    --get data types done (Noah) 
    --make test inputs (Daisy)
    --make a move function(Neil) 
    --pretty show function (hard, multi-person task, get Fogarty help)
    --check winner function (Melissa) 
