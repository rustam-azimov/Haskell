data Tree a = Nil
            | Node (Tree a) a (Tree a)

searchHelper :: [a] -> (a -> Bool) -> Tree a -> [a]
searchHelper l cond Nil = l
searchHelper l cond (Node left a right)  
                | cond a = searchHelper (a : (searchHelper l cond left)) cond right
                | otherwise = searchHelper (searchHelper l cond left) cond right
                
search :: (a -> Bool) -> Tree a -> [a]
search = searchHelper []