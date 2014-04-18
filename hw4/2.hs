data Tree a = Null
            | Node (Tree a) a (Tree a)
            
fold func start Null = start
fold func start (Node left a right) = func (fold func a left) (fold func start right)