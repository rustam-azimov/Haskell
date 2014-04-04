parse :: [Char] -> [Char] -> Bool
parse [] [] = True
parse [] _ = False

parse (x : xs) brList
                    | x == '(' = parse xs (x : brList)
                    | x == '[' = parse xs (x : brList)
                    | x == '{' = parse xs (x : brList)
                    | x == '<' = parse xs (x : brList)
                    | x == ')' = if ((brList /= []) && (head brList == '(')) then parse xs (tail brList)
                                else False
                    | x == ']' = if ((brList /= []) && (head brList == '[')) then parse xs (tail brList)
                                else False
                    | x == '}' = if ((brList /= []) && (head brList == '{')) then parse xs (tail brList)
                                else False
                    | x == '>' = if ((brList /= []) && (head brList == '<')) then parse xs (tail brList)
                                else False            
                    | otherwise = parse xs brList

bracketsCheck :: [Char] -> Bool
bracketsCheck s = parse s []