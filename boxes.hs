import Data.List 
--LETS GET THIS SHIT 
--list of letters for the horizontal axis, list of numbers for vertical axis (numbers start at top, then go down, top left is A1)
data X_axis = A | B | C | D | E deriving Show --hard coded for 5x5 grid for now 
data Y_axis = One | Two | Three | Four | Five deriving Show --Note: can't use numbers as direct constructor (override show for nums? ex.5) 
--was getting mad for something like Y_axis = 1|2|3|4|5
type Point = (X_axis, Y_axis) --making x_axis/y_axis points lets us control size of grid through constructors for each type
data Direction = Right1 | Down  deriving Show --Right1 cuz Right conflicts w/ special word 
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
