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
type Game = (Board, Boxes)
--boooooopooppboppoa hfdlhg kjg
--pretty pretty print

makePoint:: Int -> Int -> Point
makePoint x y = (x, y)

makeDirection :: String -> Direction
makeDirection "Right" = Right1 --conflicts with prelude def Right, so it's Right1. 
makeDirection "Down" = Down1
makeDirection _ = error "invalid direction, only Right or Down permitted"

makePlayer :: String -> Player
makePlayer "P1" = P1
makePlayer "P2" = P2
makeEdge :: Point -> Direction -> Edge --should cover cases for out of bounds moves (for 5x5 grid)
makeEdge (5, _) Right1 = error "can't go right on rightmost node"
makeEdge (_, 5) Down1 = error "can't go down on bottom node"
makeEdge p d = (p, d) 

makeBox :: Point -> Player -> Box 
makeBox point player = (point, player)

makeBoard :: [Edge] -> Board
makeBoard edges = edges 

findScore :: Boxes -> Score --still takes [Box], called Boxes for consistency with gamestate tracking
findScore boxes = (length p1, length p2)
                where (p1, p2) =partition (\(_, player) -> player == P1) boxes --splits into p1's/p2's boxes 

allPoints :: [Point]
allPoints = [makePoint a one, makePoint a two, makePoint a three , makePoint a four, makePoint a five,
             makePoint b one, makePoint b two, makePoint b three , makePoint b four, makePoint b five,
             makePoint c one, makePoint c two, makePoint c three , makePoint c four, makePoint c five,
             makePoint d one, makePoint d two, makePoint d three , makePoint d four, makePoint d five,
             makePoint e one, makePoint e two, makePoint e three , makePoint e four, makePoint e five]
           where a = 1
                 b = 2
                 c = 3
                 d = 4
                 e = 5
                 one = 1
                 two=2
                 three = 3
                 four = 4
                 five = 5

allBoxPoints :: [Point]
allBoxPoints = [makePoint a one, makePoint a two, makePoint a three , makePoint a four,
             makePoint b one, makePoint b two, makePoint b three , makePoint b four,
             makePoint c one, makePoint c two, makePoint c three , makePoint c four, 
             makePoint d one, makePoint d two, makePoint d three , makePoint d four]
           
           where a = 1
                 b = 2
                 c = 3
                 d = 4
                 one = 1
                 two=2
                 three = 3
                 four = 4
              
 
--MOVEMENT
moveHorizontal :: Point -> Maybe Edge
moveHorizontal point@(x, y)
    | x == 5 = Nothing  -- Rightmost node, can't move right there should be an error msg from above
    | otherwise = Just (makeEdge point Right1) --right1 cause thats what it was above

moveVertical :: Point -> Maybe Edge
moveVertical point@(x, y)
    | y == 5 = Nothing  -- Bottom node, can't move down there should be an error msg from above
    | otherwise = Just (makeEdge point Down1) 

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

buildBoard :: [Edge]
buildBoard = concatMap (\point -> [makeEdge point Right1, makeEdge point Down1]) allPoints

--WINNER LOGIC

--assuming game inputed is a finished game
findWinner :: Game -> Maybe Player
findWinner (board, boxes) = if p1_score > p2_score then Just P1 else if p1_score < p2_score then Just P2 else Nothing
    where   scored = findScore boxes
            p1_score = fst scored
            p2_score = snd scored

--type Point = (Int, Int) --making x_axis/y_axis points lets us control size of grid through constructors for each type
--type Edge =(Point, Direction)

--not working, logic is decent, but if one edge ends up not making box only returns nothing 
--will also probably reassign all boxes to whichever player gets passed in
--will need another helper function to deal with it
pointstoBoxes :: [Point] -> [Edge] -> Player -> [Maybe Box]
pointstoBoxes points edges player = [pointToBox p edges player|p<-allBoxPoints]

pointToBox :: Point -> [Edge] -> Player -> Maybe Box 
pointToBox (x, y) edge_list player = if ((e1 `elem` edge_list) && (e2 `elem` edge_list) && (e3 `elem` edge_list) && (e4 `elem` edge_list)) then Just $ makeBox (makePoint x y) player else Nothing
    where e1 = makeEdge (makePoint x y) (makeDirection "Right")
          e2 = makeEdge (makePoint x y) (makeDirection "Down")
          e3 = makeEdge (makePoint (x+1) y) (makeDirection "Down")
          e4 = makeEdge (makePoint x (y+1)) (makeDirection "Right")
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
