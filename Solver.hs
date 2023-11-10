import Boxes 

whoWillWin :: Game -> Outcome
whoWillWin game = case winner of Just Outcome guy -> guy
                                  Nothing nothing -> pickOutcome (map whoWillWin lst)
                     where winner = findWinner game 
                            moves = allPossibleEdges
                            lst = [makeMove x game | x <- moves]


pickOutcome :: [Outcome] -> Outcome
pickOutcome  lst = undefined 


allPossibleEdges :: Int -> [Edge]
allPossibleEdges size = rgts ++ dwns 
  where  rgts = [((x,y), Rgt) | x <- [1..size-1], y <- [1..size]]
         dwns = [((x,y), Dwn) | x <- [1..size], y<-[1..size-1]]

validMoves :: Game -> [Edge]  
validMoves (board,_,_,size) = allPossibleEdges size \\ board