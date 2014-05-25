import Data.Maybe
import Data.List

data Graph v e = Graph [(Int,v)] [(Int,Int,e)]

--find neighbor with min dist
nextNeighbor (a1,v1) (a2,v2) = if (v1 < v2) 
                                then (a1,v1)
                                else (a2,v2)

findEdge v1 v2 list = find (\(x,y,_) -> (v1 == x && v2 == y) || (v1 == y && v2 == x)) list

correctJust (a,b) = (a, Just b)

getThird (_,_,a) = a

dijkstra :: (Eq a, Ord a, Fractional a) => Graph v a -> Int -> [(Int, Maybe a)]                                    
dijkstra (Graph vs es) root = map correctJust $ algorithm (map (\(a,_) -> (a, if a == root then 0 else 1 / 0)) vs) []
    where algorithm [] visited = visited --all distances have been counted
          algorithm notVisited visited = algorithm (map refresh $ filter (/= nextNotVisited) notVisited) (nextNotVisited : visited)
                 where nextNotVisited = foldl nextNeighbor (head notVisited) (tail notVisited)
                       refresh (num, dist) = recounter $ findEdge num (fst nextNotVisited) es
                              where recounter Nothing = (num, dist)
                                    recounter (Just edge) = if ((getThird edge + snd nextNotVisited) < dist)
                                                            then (num, getThird edge + snd nextNotVisited)
                                                            else (num, dist)

   
test = Graph [(1, 3), (2, 4), (3, 1), (4, 5), (5, 1), (6, 3)] [(1, 2, 4), (1, 3, 3), (1, 4, 7), (2, 4, 4), (2, 5, 2), (3, 5, 3), (4, 6, 3), (5, 6, 2)]
main = putStrLn(show $ dijkstra test 1)