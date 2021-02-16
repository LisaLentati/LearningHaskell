module LibGraph where

type Edge = (Char, Char, Integer)

type Graph = [Edge]

func :: Graph -> Edge -> Integer
func [] edge = 0
func graph edge = if head(graph) == edge
    then let (_, _, w) = edge in w
    else func (tail graph) edge


edge = ('a', 'f', 5)
graph = [('b', 'c', 3), ('a' , 'c', 1), ('a', 'b', 4)]


