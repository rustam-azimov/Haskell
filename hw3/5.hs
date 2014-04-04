data Tree a = Leaf a | Branch (Tree a) (Tree a)

height :: Tree a -> Int
height (Leaf x) = 1
height (Branch left right) = if (h1 > h2)
                          then h1 + 1
                          else h2 + 1
                        where
                        h1 = height left
                        h2 = height right

minHeight :: Tree a -> Int
minHeight (Leaf x) = 1
minHeight (Branch left right) = if (h1 < h2)
                          then h1 + 1
                          else h2 + 1
                        where
                        h1 = minHeight left
                        h2 = minHeight right