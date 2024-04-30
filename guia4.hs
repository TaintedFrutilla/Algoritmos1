-- Ejericio 1
fibonacci :: Int -> Int
fibonacci n | n == 0 = 0
            | n == 1 = 1
            | otherwise = fibonacci(n-1) + fibonacci(n-2)
-- Ejercicio 2
parteEntera :: Float -> Int
parteEntera x   | x >= 0 && x < 1 = 0
                | x < 0 = parteEntera (x+1) - 1 
                | otherwise = 1 + parteEntera (x -1)

-- Ejercicio 3
esDivisible :: Int -> Int -> Bool
esDivisible a b | a == b = True
                | a < b = False
                | otherwise = esDivisible (a - b) b

-- Ejercicio 4}
-- Suma los primeos "n" impares

sumaImpares :: Int -> Int
sumaImpares a   | a == 1 = 1   
                | otherwise = 2*a-1 + sumaImpares(a-1)
-- Ejercicio 5

factorial :: Int -> Int 
factorial n | n == 0 = 1
            | otherwise = n*factorial(n-1)

medioFactorial :: Int -> Int
medioFactorial n    | n == 0 = 1
                    | otherwise = n*factorial(n-2)

-- Ejercicio 6
-- Primera resolucion sin llamar a funciones auxiliares
sumaDigitos :: Int -> Int
sumaDigitos n   | mod n 10 == n = n
                | otherwise = (mod n 10) + sumaDigitos(div n 10)

-- Llamamiento a funciones auxiliares
ultimoDigito :: Int -> Int 
ultimoDigito n = mod n 10

menosUltimoDigito :: Int -> Int 
menosUltimoDigito n = div n 10

sumaDigitos2 :: Int -> Int
sumaDigitos2 n  | ultimoDigito n == n = n
                | otherwise = ultimoDigito n + sumaDigitos2(menosUltimoDigito n)

-- Ejecicio 7
-- Me parecios bastante complicado, el tenes que llamar a funciones auxiliares lo hace mas facil.
-- Igual, el paso recursivo me marea un poquillo 

sacarUnidades :: Int -> Int
sacarUnidades n = n - (mod n 10)


todosDigitosIguales :: Int -> Bool
todosDigitosIguales n   | n < 10 = True
                        | otherwise = (ultimoDigito n == ultimoDigito (menosUltimoDigito n)) && todosDigitosIguales (menosUltimoDigito n)

-- Ejercicio 8 
cantidadDigitos :: Int -> Int
cantidadDigitos n   | n < 10 = 1
                    | otherwise = cantidadDigitos(menosUltimoDigito n) + 1 

iEsimoDigito :: Int -> Int -> Int 
iEsimoDigito n i    | i == cantidadDigitos n = ultimoDigito n
                    | otherwise = iEsimoDigito (menosUltimoDigito n) i 

-- Ejercicio 9
--esCapicua :: Int -> Bool+




-- Ejercicio 10
f1 :: Int -> Int
f1 n    | n == 0 = 1
        | otherwise = 2*n + f1(n-1)