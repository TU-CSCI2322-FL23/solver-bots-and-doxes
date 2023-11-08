module Boxes where
import Data.List 
import Data.Function(on)
import Debug.Trace

type Point = (Int, Int) --making x_axis/y_axis points lets us control size of grid through constructors for each type
data Direction = Rgt | Dwn  deriving (Show, Eq) --Right1 cuz Right conflicts w/ special word        --added (Eq)
data Player = P1 | P2 deriving (Show, Eq)
type Edge =(Point, Direction)
type Box= (Point, Player)
type Board = [Edge]
type Boxes = [Box] --Need to keep track of boxes because player that closes box matters
type Game = (Board, Boxes, Player, Int)
--DONT LET THEM MAKE A GAME OF SIZE 1 IT WILL NEVER END

--allPossibleEdges =[((x, y), dir) | x <- [1..size-1], y <- [1..size-1], dir <- [Rgt, Dwn]] 
opponent:: Player -> Player
opponent P1 = P2
opponent P2 = P1

validMove :: Edge -> Board -> Bool
validMove edge board = notElem edge board

isInBounds :: Game -> Edge -> Bool
isInBounds game@(_, _, _ ,size) ((x, y), Dwn) = (x<=size) && (x>=1) && (y<size) &&(y>=1)
isInBounds game@(_, _, _, size) ((x, y), Rgt) = (x<size) && (x>=1) && (y<=size) && (y>=1)

--assuming game inputed is a finished game
findWinner :: Game -> Maybe Player
findWinner (board, boxes, _, _) = if p1_score > p2_score then Just P1 else if p1_score < p2_score then Just P2 else Nothing
    where   scored = findScore boxes
            p1_score = fst scored
            p2_score = snd scored

--makes boxes
makeBoxes :: Edge -> Game -> [Box] 
makeBoxes move@(point@(x, y), Rgt) game@(board, boxes, player, _) = [(p, player) | p <- [(x,y-1), point], checkBox p (move:board)]
makeBoxes move@(point@(x, y), Dwn) game@(board, boxes, player, _) = [(p, player) | p <- [(x-1,y), point], checkBox p (move:board)]


--Returns boolean if there's a box originating (top left point of the box) from a given point. --AUDREY SEAL OF APPROVAL
checkBox :: Point -> [Edge] -> Bool
checkBox (x, y) edge_list = and [edge `elem` edge_list | edge <- boxEdges]
    where boxEdges = [((x, y), Rgt), ((x,y),Dwn), ((x+1,y),Dwn) , ((x,y+1),Rgt)]
--Build a row of edges starting at a given point

--Takes a Game state and an edge (move) and returns the new game state. 
makeMove :: Game -> Edge -> Game
makeMove game@(board, boxes, player, int) move
  | not (validMove move board) = error "Invalid Move, it already exists." --if not a valid more, throw error 
  | not (isInBounds game move) = error "Invalid Move, out of Bounds"
  | null newBoxes = (move:board, boxes, opponent player, int) --if list of new boxes is empty, return new game w/move added to board, same boxes, switch player
  | otherwise = (move:board, newBoxes++boxes, player, int) --if new boxes, return new game w/move added to board, the nex boxes made added to existing boxes, same player
  where
      newBoxes = makeBoxes move game


--PRETTY PRINT SECTION
combineRows :: [[(Point, String)]] -> [String]
combineRows rows = map concatRow rows
  where
    concatRow :: [(Point, String)] -> String
    concatRow = concatMap (\(_, s) -> s)

    
prettyShow :: Game -> [String]
prettyShow ([],[],_,n) = [intercalate ""(replicate n ".  ")|x<-[1..n]] 
prettyShow (board,boxes,_,n) =  
    let (horizontals,vertical) = partition(\(_,dir) -> dir == Rgt) board
        startingGrid = [((x,y),".")|x<-[1..n],y<-[1..n]]
        upHoriz = updateHorizontals horizontals startingGrid
        grouphoriz = orderPoints upHoriz
        upVerts = updateVerticals vertical grouphoriz
    in combineRows upVerts
    where updateHorizontals :: [Edge] -> [(Point,String)] -> [(Point,String)]
          updateHorizontals rights grid = 
                                    map (\x -> if elem (fst x) (map fst rights)
                                               then (fst x, snd x ++ "--")
                                               else (fst x, snd x ++ "  ")
                                               ) grid
          updateVerticals :: [Edge] ->[[(Point,String)]] -> [[(Point,String)]]
          updateVerticals downs horiz = 
                          let nGrid = [((x,y),"")|x<-[1..(n-1)],y<-[1..n]]
                              bars = map (\x -> if elem (fst x) (map fst downs)
                                                then (fst x, snd x ++ "|") --("|  ")
                                                else (fst x, snd x ++ " ") --(" ")
                                                ) nGrid
                              addPl = writesPlayer bars boxes
                              orderBars = orderPoints addPl--orderBars = orderPoints bars
                           in insertnewRows orderBars horiz
                             where insertnewRows :: [[(Point,String)]] -> [[(Point,String)]] -> [[(Point,String)]]
                                   insertnewRows [] []  = []
                                   insertnewRows [] [h] = [h]
                                   insertnewRows (v:vs) (h:hs) = h : [v] ++ insertnewRows vs hs 
{-
writesPlayer :: [(Point,String)] -> Boxes -> [(Point,String)]
writesPlayer grid boxes = 
                        map (\x -> if elem (fst x) (map fst boxes)
                                                 then (fst x, snd x ++ getPlayer (lookup (fst x) boxes))
                                                 else (fst x, snd x ++ "  ")
                                                 ) grid
                          where getPlayer :: Player -> String
                                getPlayer P1 = "P1"
                                getPlayer p2 = "P2"
-}
writesPlayer :: [(Point, String)] -> Boxes -> [(Point, String)]
writesPlayer grid boxes =
  map (\(p, s) -> case lookup p boxes of
    Just player -> (p, s ++ getPlayer player)
    Nothing -> (p, s ++ "  ")
  ) grid

-- Convert Player to String
getPlayer :: Player -> String
getPlayer P1 = "P1"
getPlayer P2 = "P2"



orderPoints :: [(Point,String)] -> [[(Point,String)]]
orderPoints points = groupBy (\x y -> fst(fst x) == fst (fst y)) points

{-
comparePoints :: Box -> Box -> Ordering
comparePoints ((x1, y1),_) ((x2, y2),_) =    
        case compare x1 x2 of
        EQ -> compare y1 y2
        other -> other


orderEdge :: Edge -> Edge -> Ordering
orderEdge ((x1,y1),_) ((x2,y2),_) =  
        case compare x1 x2 of
        EQ -> compare y1 y2
        other -> other
-}
-- sortBy comparePoints boxes
-- boxes will be in order
{-
.--.--.
|P1|P2|
.--.--.
|P2|P2|
.--.--.
-}


prettyPrint :: Game -> IO ()
prettyPrint game = do
        let strings = prettyShow game
        mapM_ putStrLn strings
      