
absoluto :: Int -> Int
absoluto n  | n < 0 = -n
            | otherwise = n


maximoAbsoluto :: Int -> Int -> Int
maximoAbsoluto n m  | absoluto n > absoluto m = absoluto n
                    | otherwise = absoluto m

maximo3 :: Int -> Int -> Int -> Int
maximo3 a b c   | a > b && a > c = a
                | b > c = b
                | otherwise = c

algunoEs0 :: Float -> Float -> Bool
algunoEs0 q1 q2   | q1 == 0 || q2 == 0 = True
                | otherwise = False


ambosSon0 :: Float -> Float -> Bool
ambosSon0 q1 q2   | q1 == 0 && q2 == 0 = True
                | otherwise = False

mismoIntervalo :: Float -> Float -> Bool
mismoIntervalo x y  | 3 >= x && 3 >= y = True
                    | 3 < x && x <= 7  && 3 < y && y <= 7 = True
                    | x > 7 && y > 7 = True
                    | otherwise = False


sumaDistintos :: Int -> Int -> Int -> Int 
sumaDistintos a b c | a == b && a == c = 0
                    | a == b = c
                    | a == c = b 
                    | b == c = a
                    | otherwise = a + b + c 

esMultiploDe :: Int -> Int -> Bool
esMultiploDe n m    | mod n m == 0 = True
                    | otherwise = False

digitoUnidades :: Int -> Int
digitoUnidades a = mod a 10 
-- al mod le importa el resto.

digitoDecenas :: Int -> Int
digitoDecenas a = div a 10
-- al div le importa el cociente.


-- Ejercicio 3
estanRelacionados :: Int -> Int -> Bool
estanRelacionados a b = mod a b == 0 

-- Ejercicio 4
prodInt :: (Float, Float) -> (Float, Float) -> Float
prodInt (x1, x2) (y1, y2) = x1*y1 + x2*y2

todoMenor :: (Float, Float) -> (Float, Float) -> Bool
todoMenor(x1, x2) (y1,y2)   | x1 < y1 && x2 < y2 = True
                            | otherwise = False

distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float
distanciaPuntos (x1, x2) (y1, y2) = sqrt ((x1-y1)**2 + (x2-y2)**2)

sumaTerna :: (Int, Int, Int) -> Int
sumaTerna (x1, x2, x3) = x1 + x2 + x3

sumarSoloMultiplos :: (Int, Int, Int) -> Int -> Int
sumarSoloMultiplos (a, b, c) n  | mod a n == 0 && mod b n == 0 && mod c n == 0 = a + b + c
                                | mod a n == 0 && mod b n == 0 = a + b
                                | mod a n == 0 && mod c n == 0 = a + c
                                | mod b n == 0 && mod c n == 0 = b + c
                                | mod a n == 0 = a
                                | mod b n == 0 = b
                                | mod c n == 0 = c
                                | otherwise = 0


posPrimerPar :: (Int, Int, Int) -> Int
posPrimerPar (a, b, c)  | mod a 2 == 0 = 1
                        | mod b 2 == 0 = 2
                        | mod c 2 == 0 = 3
                        | otherwise = 4

crearPar :: a -> b -> (a, b)
crearPar a b = (a, b)

invertir :: (a, b) -> (b, a)
invertir (a, b) = (b, a)

todosMenores :: (Int, Int, Int) -> Bool
todosMenores (a, b, c)  | a > b && b > c = True
                        | otherwise = False

funf :: Int -> Int|
funf a  | a <= 7 = a*a
        | otherwise = 2*a - 1

fung :: Int -> Int
fung a  | mod a 2 == 0 = div a 2
        | otherwise = 3*a + 1


-- Ejercicio 6
bisiesto :: Int -> Bool
bisiesto a  | mod a 4 == 0 && mod a 10 /= 0 = True
            | otherwise = False

-- Ejercicio 7


absoluto2 :: Float -> Float
absoluto2 x     | x < 0 = -x
                | otherwise = x

distanciaManhattan :: (Float, Float, Float) -> (Float, Float, Float) -> Float
distanciaManhattan (x1, x2, x3) (y1, y2, y3) = absoluto2 ((x1 - y1)) + absoluto2 ((x2 - y2)) + absoluto2 ((x3 - y3))

-- Ejercicio 8
sumaUltimosDosDigitos :: Int -> Int
sumaUltimosDosDigitos a = ( mod (absoluto a) 10 ) + (mod (div a 10) 10)

comparar :: Int -> Int -> Int
comparar a b    | sumaUltimosDosDigitos a < sumaUltimosDosDigitos b = 1
                | sumaUltimosDosDigitos a > sumaUltimosDosDigitos b = 1
                | otherwise = 0




-- Se lo puede hacer llamando a funciones que cree previamente.