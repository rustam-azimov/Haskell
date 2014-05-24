data Monom = Sum (Int, Int) Monom
        | Null

showMonom :: Monom -> String
showMonom Null = ""
showMonom (Sum (coeff, degree) Null) = (show coeff) ++ "x^" ++ (show degree)
showMonom (Sum (coeff, degree) ms) = (show coeff) ++ "x^" ++ (show degree) ++ "+" ++ showMonom ms

add :: Monom -> Monom -> Monom
add ms1 Null = ms1
add Null ms2 = ms2
add (Sum (coeff1, degree1) ms1) (Sum (coeff2, degree2) ms2) =
	if (degree1 == degree2)
    then Sum (coeff1 + coeff2, degree1) (add ms1 ms2)
	else
		if (degree1 > degree2)
		then Sum (coeff1, degree1) (add ms1 (Sum (coeff2, degree2) ms2))
		else
            Sum (coeff2, degree2) (add ms2 (Sum (coeff1, degree1) ms1))
            
multiplier :: Monom -> Monom -> Monom -> Monom
multiplier ms1 ms2 Null = Null
multiplier Null ms2 ms3 = Null
multiplier (Sum (c1, d1) ms1) (Sum (c2, d2) ms2) ms3  =
    Sum (c1 * c2, d1 + d2) (multiplier (Sum (c1, d1) ms1) ms2 ms3)
multiplier (Sum (c1, d1) ms1) Null (Sum (c3, d3) ms3) = 
	multiplier ms1 (Sum (c3, d3) ms3) (Sum (c3, d3) ms3)

multiply :: Monom -> Monom -> Monom
multiply ms1 ms2 = multiplier ms1 ms2 ms2

main = do
        let p1 = Sum (2, 2) $ Sum (1, 1) $ Sum (5, 0) Null
        let p2 = multiply p1 p1
        putStrLn (showMonom p1)
        putStrLn (showMonom p2)