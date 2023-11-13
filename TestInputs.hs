module TestInputs where
 import System.IO.Unsafe
 import Data.List
 import Boxes



 whoWillWinTest1 :: Game
 whoWillWinTest1 = ([((1, 1), Rgt), ((1, 1), Dwn), ((1, 2), Dwn), ((3, 1),Dwn), ((2, 2), Rgt), ((1, 3), Rgt), ((2, 3), Rgt), ((2, 1), Dwn)], [], P1, 3) 

-- initialGame = ([], [], P1, 4)
-- move1 = ((1, 1), Rgt)
-- move2 = ((1, 1), Dwn)
-- move3 = ((2, 1), Rgt)
-- move4 = ((1, 2), Dwn)
-- move5 = ((1, 3), Dwn)
-- move6 = ((2, 2), Rgt)
-- move7 = ((1, 2), Dwn)
-- game1 = makeMove initialGame move1 --logic here is that every move copies the previous game that has all last moves than keep making moves
-- game2 = makeMove game1 move2
-- game3 = makeMove game2 move4

-- -- Test input for making moves in the game, to run this load testinputs and type makeMoveTest
-- makeMoveTest :: IO ()
-- makeMoveTest = do
--   let initialGame = ([], [], P1, 4) --intial game is empty, empty, player one, four by four
--       move1 = ((1, 1), Rgt)
--       move2 = ((1, 1), Dwn)
--       move3 = ((2, 1), Rgt)
--       move4 = ((1, 2), Rgt)
--       move5 = ((1, 3), Dwn)
--       move6 = ((2, 2), Rgt)
--       move7 = ((1, 2), Dwn) -- THIS MAKES TWO BOXES


--   let game1 = makeMove initialGame move1 --logic here is that every move copies the previous game that has all last moves than keep making moves
--   let game2 = makeMove game1 move2
--   let game3 = makeMove game2 move3
--   let game4 = makeMove game3 move4
--   let game5 = makeMove game4 move5
--   let game6 = makeMove game5 move6
--   let game7 = makeMove game6 move7


--   let game1A = makeMove game2 move4 --ignore
  
--   print game1 -- Expected: ([(1,1,Right1)],[],P2,3)
--   print game2 -- Expected: ([(1,1,Right1),(1,1,Down1)],[((1,1),P1)],P1,3)
--   print game3 -- Expected: ([(1,1,Right1),(1,1,Down1),(2,1,Right1)],[((1,1),P1)],P2,3)
--   print game4 -- Expected: ([(1,1,Right1),(1,1,Down1),(2,1,Right1),(1,2,Down1)],[((1,1),P1),((1,1),P2)],P1,3)

-- --  print initialGame
--   print game7 -- Expected: ([((1,2),Down1),((2,2),Right1),((1,3),Down1),((1,2),Right1),((2,1),Right1),((1,1),Down1),((1,1),Right1)],[],P2,4)
-- --  print game1A -- Expected: 

-- -- Function to take user input for a move
-- getUserMove :: IO Edge
-- getUserMove = do
--   putStrLn "Enter your move (e.g., '1 1 R' for right or '2 2 D' for down):"
--   input <- getLine
--   let [xStr, yStr, directionStr] = words input
--       x = read xStr
--       y = read yStr
--       direction = readDirection directionStr
--   return ((x, y), direction)
--   where
--     readDirection "R" = Rgt
--     readDirection "D" = Dwn
--     readDirection _ = error "Invalid direction. Use 'R' for right or 'D' for down."
-- -- so 1 1 D is ((1,1) Dwn)

-- -- Test input for making moves in the game with user input, so keep 
-- makeMoveTestUserInput :: IO ()
-- makeMoveTestUserInput = do
--   let initialGame = ([], [], P1, 3)
--   playGame initialGame

-- playGame :: Game -> IO ()
-- playGame game = do
--   move <- getUserMove
--   let updatedGame = makeMove game move
--   putStrLn "Current game state:"
--   print (findWinner updatedGame)
--   prettyPrint updatedGame
--   if isGameOver updatedGame
--     then putStrLn "Game over!"
--     else playGame updatedGame




-- {-
-- findWinnerTest :: IO ()
-- findWinnerTest = do
--   let game1 = ([(1,1,Rgt),(1,1,Dwn),(2,1,Rgt),(1,2,Dwn)],[((1,1),P1),((1,1),P2)],P1,3)
--   let game2 = ([(1,1,Rgt),(1,1,Dwn),(2,1,Rgt),(1,2,Dwn),(2,2,Dwn)],[((1,1),P1),((1,1),P2)],P1,3)

--   let winner1 = findWinner game1
--   let winner2 = findWinner game2

--   print winner1 -- Expected: Just P1
--   print winner2 -- Expected: Nothing
-- -}


-- --PREVIOUS CODE
-- {-
-- r= makeDirection "Right"
-- e1, e2, e3, e4:: Edge
-- d=makeDirection "Down"
-- e1= ((1,1), d)
-- e2= ((1,1), r) 
-- e3 = ((2, 1), d)
-- e4 = ((1, 2), r)
-- almostOneBoxGame :: Game
-- almostOneBoxGame = ([e1, e2, e3], [], (makePlayer "P1"), 3)

-- -}
-- --demo for bug (makeMove with the game and e4)
-- --will return empty list of boxes, checkBox called on point (1,2) in makeBoxes
-- --so will be false, return empty list, case 2 will be called 
-- --blankGame = ([],[], P1, 3)
-- -- .   .   .
-- --
-- -- .   .   .
-- --
-- -- .   .   .


-- -- . — .   .
-- -- | 1 |
-- -- . — .   .
-- --
-- -- .   .   .

-- --ANOTHER IDEA for how it could look
-- -- (1,1)——(2,1)——(3,1)
-- --   :      |  P1  |
-- -- (1,2)--(2,2)——(3,2)
-- --   :      :      :      
-- -- (1,3)--(2,3)——(3,3)
