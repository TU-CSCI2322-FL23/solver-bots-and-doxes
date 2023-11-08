import Boxes
import Data.List
import Data.List.Split

readGame :: String -> Game
readGame string = (boardFinal, boxesFinal, readPlayer player, read int)
    where   [boardy, boxesy, player ,int] = lines string
            board = splitOn ";" boardy
            boxes = splitOn ";" boxesy
            boardFinal = [readEdge x | x <- board]
            boxesFinal = [readBox x | x <- boxes]

readEdge :: String -> Edge
readEdge str = ((x',y'),dir')
    where   [x,y,dir] = words str
            x' = read x
            y' = read y
            dir' = readDirection dir

readBox :: String -> Box
readBox str = ((x',y'),play')
    where   [x,y, play] = words str
            x' = read x
            y' = read y
            play' = readPlayer play


readPlayer :: String -> Player
readPlayer "P1" = P1
readPlayer "P2" = P2

readInt :: String -> Int
readInt str = read str

readDirection :: String -> Direction
readDirection "R" = Rgt
readDirection "D" = Dwn

