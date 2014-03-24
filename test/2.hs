data Tree a = Nil
            | Node (Tree a) a (Tree a)
search :: (a -> Bool) -> Tree a -> [a]
search = searchHelper []
          where searchHelper xs cond Nil = xs
                searchHelper xs cond (Node leftChild a rightChild)  
                                | cond a = searchHelper (a : (searchHelper xs cond leftChild)) cond rightChild
                                | otherwise = searchHelper (searchHelper xs cond leftChild) cond rightChild
								
--show $ search (<0) tree