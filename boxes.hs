import Data.List 
--LETS GET THIS SHIT DONE
--list of letters for the horizontal axis, list of numbers for vertical axis (numbers start at top, then go down, top left is A1)
data X_axis = A | B | C | D | E deriving (Show, Eq) --hard coded for 5x5 grid for now       --added (Eq)
data Y_axis = One | Two | Three | Four | Five deriving (Show, Eq) --Note: can't use numbers as direct constructor (override show for nums? ex.5) --added (Eq)
--was getting mad for something like Y_axis = 1|2|3|4|5
type Point = (X_axis, Y_axis) --making x_axis/y_axis points lets us control size of grid through constructors for each type
data Direction = Right1 | Down  deriving (Show, Eq) --Right1 cuz Right conflicts w/ special word        --added (Eq)
data Player = P1 | P2 deriving (Show, Eq)
type Edge =(Point, Direction)
type Box= (Point, Player) 
type Board = [Edge]
type Score = (Int, Int) --CHANGED FROM [BOXES] TO (player1_score, player2_score), findScore function below 

toX :: Char -> X_axis --might need to add more if we extend the board at any point 
toX 'A' = A
toX 'B' = B
toX 'C' = C
toX 'D' = D
toX 'E' = E
toX _ = error "invalid X-axis coordinate"

toY :: Int-> Y_axis
toY 1 = One
toY 2 = Two
toY 3 = Three
toY 4 = Four
toY 5 = Five 
toY _ = error "invalid Y-axis coordinate"

makePoint:: X_axis -> Y_axis -> Point
makePoint x y = (x, y)

makeDirection :: String -> Direction
makeDirection "Right" = Right1 --conflicts with prelude def Right, so it's Right1. Could do left instead if weird naming scheme is an issue 
makeDirection "Down" = Down
makeDirection _ = error "invalid direction, only right or down permitted"

makePlayer :: String -> Player
makePlayer "P1" = P1
makePlayer "P2" = P2
makeEdge :: Point -> Direction -> Edge --should cover cases for out of bounds moves (for 5x5 grid)
makeEdge (E, _) Right1 = error "can't go right on rightmost node"
makeEdge (_, Five) Down = error "can't go down on bottom node"
makeEdge p d = (p, d) 

makeBox :: Point -> Player -> Box 
makeBox point player = (point, player)

makeBoard :: [Edge] -> Board
makeBoard edges = edges 

findScore :: [Box] -> Score 
findScore boxes = (length p1, length p2)
                where (p1, p2) =partition (\(_, player) -> player == P1) boxes --splits into p1's/p2's boxes 

allPoints :: [Point]
allPoints = [makePoint a one, makePoint a two, makePoint a three , makePoint a four, makePoint a five,
             makePoint b one, makePoint b two, makePoint b three , makePoint b four, makePoint b five,
             makePoint c one, makePoint c two, makePoint c three , makePoint c four, makePoint c five,
             makePoint d one, makePoint d two, makePoint d three , makePoint d four, makePoint d five,
             makePoint e one, makePoint e two, makePoint e three , makePoint e four, makePoint e five]
           where a = toX 'A'
                 b = toX 'B'
                 c = toX 'C'
                 d = toX 'D'
                 e = toX 'E'
                 one = toY 1
                 two=toY 2
                 three = toY 3
                 four = toY 4
                 five = toY 5

 
--MOVEMENT
moveHorizontal :: Point -> Maybe Edge
moveHorizontal point@(x, y)
    | x == E = Nothing  -- Rightmost node, can't move right there should be an error msg from above
    | otherwise = Just (makeEdge point Right1) --right1 cause thats what it was above

moveVertical :: Point -> Maybe Edge
moveVertical point@(x, y)
    | y == Five = Nothing  -- Bottom node, can't move down there should be an error msg from above
    | otherwise = Just (makeEdge point Down) 

--Board Checks

isAvailable :: Board -> Point -> Direction -> Bool
isAvailable board point direction = notElem (makeEdge point direction) board

isHorizontal :: Edge -> Bool
isHorizontal (_, direction) = direction == Right1 --right1 cause thats what it was above

isVertical :: Edge -> Bool
isVertical (_, direction) = direction == Down

isValid :: Board -> Edge -> Bool
isValid board edge@(point, direction) =
    isWithinBounds point && isAvailable board point direction
    where
        isWithinBounds (x, y) = x /= E && y /= Five

updateBoard :: Board -> Edge -> Player -> Maybe Board -- maybe it should be put edge in board and if that edge meakes a square set a boolean to true?
updateBoard board edge player
    | isValid board edge = Just (edge : board)
    | otherwise = Nothing

--BUILD BOARD

buildBoard :: [Edge]
buildBoard = concatMap (\point -> [makeEdge point Right1, makeEdge point Down]) allPoints

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





boardToString :: Board -> String
boardToString b = x ++ "\n" ++ fst (foldl printBoxes ("",(0,0)) b)
  where x = fst $ foldl printHorizontal ("*",(0,0)) $ head b

printBoxes :: (String,Point) -> [Box] -> (String,Point)
printBoxes (s,(p1,p2)) lb = (s++x++"\n"++y++"\n",(p1+1,p2))
  where z = if ((p1,p2),(p1+1,p2)) `elem` fst (head lb) then "-" else " "
        x = fst $ foldl printVertical (z,(p1,p2)) lb
        y = fst $ foldl printHorizontal ("*",(p1+1,p2)) lb

printHorizontal :: (String,Point) -> Box -> (String,Point)
printHorizontal (s,(p1,p2)) (le,_) = if ((p1,p2),(p1,p2+1)) `elem` le then (s++" - *",(p1,p2+1)) else (s++"   *",(p1,p2+1))

printVertical :: (String,Point) -> Box -> (String,Point)
printVertical (s,(p1,p2)) (le,n) = if ((p1,p2+1),(p1+1,p2+1)) `elem` le then (s++" "++show n++" -",(p1,p2+1)) else (s++" "++show n++"  ",(p1,p2+1))

printBoard :: Board -> IO ()
printBoard b = do putStrLn ""
                  putStrLn $ boardToString b
                  putStrLn ""






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
