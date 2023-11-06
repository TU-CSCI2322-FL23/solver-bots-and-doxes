module TestInputs where
import System.IO.Unsafe
import Data.List
import Boxes

r= makeDirection "Right"
d=makeDirection "Down"
e1=makeEdge (1,1) d
e2=makeEdge (1,1) r 
e3 = makeEdge (2, 1) d
e4 = makeEdge (1, 2) r
almostOneBoxGame = ([e1, e2, e3], [], (makePlayer "P1"), 3)
--demo for bug (makeMove with the game and e4)
--will return empty list of boxes, checkBox called on point (1,2) in makeBoxes
--so will be false, return empty list, case 2 will be called 
--blankGame = ([],[], P1, 3)
-- .   .   .
--
-- .   .   .
--
-- .   .   .


-- . — .   .
-- | 1 |
-- . — .   .
--
-- .   .   .