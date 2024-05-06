sacarBlancosRepetidos :: [Char] -> [Char]
sacarBlancosRepetidos [] = []
sacarBlancosRepetidos (x:[]) = [x]
sacarBlancosRepetidos (x:y:xs) | x ==' ' && y==' ' = sacarBlancosRepetidos (y:xs) 
                               | otherwise = x : sacarBlancosRepetidos (y:xs)