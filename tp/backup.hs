module PruebaTP where
import Data.Char

maximo :: Ord a => [a] -> a
maximo [x,y]    | x >= y = x
                | otherwise = y
maximo (x:y:xs) | x <= y = maximo (y:xs)
                | otherwise = maximo (x:xs)


posicion :: (Eq t, Num p) => [t] -> t -> p
posicion (x:xs) a | a == x = 0
                  | otherwise = 1 + posicion xs a
                  
valorDeLaPosicion :: (Eq t, Num t) => [p] -> t -> p
valorDeLaPosicion (x:xs) n | n == 0 = x
                           | otherwise = valorDeLaPosicion xs (n-1)


listaConMinusculas :: String -> Bool
listaConMinusculas [] = False
listaConMinusculas (x:xs) | esMinuscula x = True
                          | otherwise = listaConMinusculas xs
                          
-- Ejercicio 1
esMinuscula :: Char -> Bool
esMinuscula a = ord a >= 97 && ord a <= 122


-- Ejercicio 2
letraANatural :: Char -> Int
letraANatural a = ord a - 97


-- Ejercicio 3
desplazar :: Char -> Int -> Char
desplazar a n | not (esMinuscula a) = a
              | letraANatural a + n <= 25 && letraANatural a + n >= 0 = chr (97 + letraANatural a + n)
              | letraANatural a + n < 0 = chr (123 + letraANatural a + n)
              | otherwise = desplazar a (n-26)
              
-- Ejercicio 4
-- requiere n>=0
cifrar :: String -> Int -> String
cifrar [] _ = []
cifrar (x:xs) n = desplazar x n : cifrar xs n


-- Ejercicio 5
--requiere n>=0
descifrar :: String -> Int -> String
descifrar "" _ = ""
descifrar (x:xs) n = desplazar x (-n) : descifrar xs n

