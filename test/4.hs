expr' [] s p = (s, p)
expr' (x:xs) s p = expr' xs (s + x) (p * (cos x))

expr xs = (fst result) / (snd result)
    where result = expr' xs 0 1