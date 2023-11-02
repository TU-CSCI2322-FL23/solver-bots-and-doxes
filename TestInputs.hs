module test Inputs(Game) where
import boxes

blankGame = ([],[])

--p1WinGame = Game :: ()

--box originating at 1, 1
1edge1 = makeEdge (makePoint 0 0 ) (makeDirection "right")
1edge2 = makeEdge (makePoint 0 0)(makeDirection "down")
1edge3 = makeEdge (makePoint 1 0)(makeDirection "down")
1edge4 = makeEdge (makePoint 0 1) (makeDirection "right") 

