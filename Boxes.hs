module Boxes where
import Data.List 
import Data.Function(on)
import Debug.Trace

type Point = (Int, Int) 
data Direction = Rgt | Dwn  deriving (Show, Eq) 
data Player = P1 | P2 deriving (Show, Eq)
data Outcome = Players Player | Tie deriving (Show, Eq)
type Edge =(Point, Direction)
type Box= (Point, Player)
type Board = [Edge]
type Boxes = [Box] 
type Game = (Board, Boxes, Player, Int)


--switches player for turn change 
opponent:: Player -> Player
opponent P1 = P2
opponent P2 = P1

--makes sure move doesn't exist
validMove :: Edge -> Board -> Bool
validMove edge board = notElem edge board

--makes sure move is in bounds 
isInBounds :: Game -> Edge -> Bool
isInBounds (_, _, _ ,size) ((x, y), Dwn) = (x<=size) && (x>=1) && (y<size) &&(y>=1)
isInBounds (_, _, _, size) ((x, y), Rgt) = (x<size) && (x>=1) && (y<=size) && (y>=1)

--returns Nothing if an unfinished game
findWinner :: Game -> Maybe Outcome
findWinner (board, boxes, _, size)
  | p1_score +p2_score< ((size-1) *(size-1)) = Nothing
  | p1_score == p2_score = Just Tie 
  | p1_score > p2_score = Just (Players P1) 
  | otherwise = Just (Players P2)
    where   (p1, p2) = partition (\(_, player) -> player == P1) boxes
            p1_score = length p1
            p2_score = length p2

--uses amount of boxes to determine if game is over 
isGameOver :: Game -> Bool
isGameOver (_, boxes, _, size) = length boxes == (size - 1) * (size - 1)

--makes boxes
makeBoxes :: Edge -> Game -> [Box] 
makeBoxes move@(point@(x, y), Rgt) (board, _, player, _) = 
  [(p, player) | p <- [(x,y-1), point], checkBox p (move:board)]
makeBoxes move@(point@(x, y), Dwn) (board, _, player, _) = 
  [(p, player) | p <- [(x-1,y), point], checkBox p (move:board)]


--Returns boolean if there's a box originating (top left point of the box) from a given point. 
checkBox :: Point -> [Edge] -> Bool
checkBox (x, y) edge_list = and [edge `elem` edge_list | edge <- boxEdges]
    where boxEdges = [((x, y), Rgt), ((x,y),Dwn), ((x+1,y),Dwn) , ((x,y+1),Rgt)]
--Build a row of edges starting at a given point


--returns a new game state with the edge and potential new boxes added, invalid move would return nothing 
makeMove :: Game -> Edge -> Maybe Game
makeMove game@(board, boxes, player, int) move
  | not (validMove move board) ||not (isInBounds game move) = Nothing 
  | null newBoxes = Just (move:board, boxes, opponent player, int) --if list of new boxes is empty, return new game w/move added to board, same boxes, switch player
  | otherwise = Just (move:board, newBoxes++boxes, player, int) --if new boxes, return new game w/move added to board, the nex boxes made added to existing boxes, same player
  where
      newBoxes = makeBoxes move game

--computes all valid moves from a game state 
validMoves :: Game -> [Edge]  
validMoves (board,_,_,size) = allPossibleEdges size \\ board

--computes all possible edges from a board size 
allPossibleEdges :: Int -> [Edge]
allPossibleEdges size = rgts ++ dwns 
  where  rgts = [((x,y), Rgt) | x <- [1..size-1], y <- [1..size]]
         dwns = [((x,y), Dwn) | x <- [1..size], y<-[1..size-1]]


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
        startingGrid = [[((x,y),".") |x<-[1..n]]|y<-[1..n]] --new one [[((a,b),String)]] 
        upHoriz = updateHorizontals horizontals startingGrid
        upVerts = updateVerticals vertical upHoriz--grouphoriz
    in combineRows upVerts
    where updateHorizontals :: [Edge] -> [[(Point,String)]] -> [[(Point,String)]]
          updateHorizontals rights grid =
                                   [ map (\x -> if elem (fst x) (map fst rights)
                                               then (fst x, snd x ++ "--")
                                               else (fst x, snd x ++ "  ")
                                               ) y|y<- grid]
          updateVerticals :: [Edge] ->[[(Point,String)]] -> [[(Point,String)]]
          updateVerticals downs horiz =
                          let nGrid = [[((x,y),"") |x<-[1..n]]|y<-[1..n]] --changed to x<-[1..(n-1)
                              bars = [map (\x -> if elem (fst x) (map fst downs)
                                                then (fst x, snd x ++ "|") --("|  ")
                                                else (fst x, snd x ++ " ") --(" ")
                                                ) y|y<- nGrid]
                              addPl = writesPlayer bars boxes
                           in insertnewRows addPl horiz--orderBars horiz
                             where insertnewRows :: [[(Point,String)]] -> [[(Point,String)]] -> [[(Point,String)]]
                                   insertnewRows [] []  = []
                                   insertnewRows [] [h] = [h]
                                   insertnewRows (v:vs) (h:hs) = h : [v] ++ insertnewRows vs hs

writesPlayer :: [[(Point, String)]] -> Boxes -> [[(Point, String)]]
writesPlayer grid boxes =
  [map (\(p, s) -> case lookup p boxes of
    Just player -> (p, s ++ getPlayer player)
    Nothing -> (p, s ++ "  ")
  ) y|y<- grid]

-- Convert Player to String
getPlayer :: Player -> String
getPlayer P1 = "P1"
getPlayer P2 = "P2"


orderPoints2 :: [(Point,String)] -> [[(Point,String)]]
orderPoints2 points = groupBy (\x y-> snd(fst x) == snd (fst x)) points

orderPoints :: [(Point,String)] -> [[(Point,String)]]
orderPoints points = groupBy (\x y -> fst(fst x) == fst (fst y)) points

prettyPrint :: Game -> IO ()
prettyPrint game = do
        let strings = prettyShow game
        mapM_ putStrLn strings
    
