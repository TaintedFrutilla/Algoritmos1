-- Parcial Haskell 2C2023

-- Ejercicio 1
cantVotos :: [Int] -> Int
cantVotos [] = 0
cantVotos (x:[]) = x
cantVotos (votos:votosRestantes) = votos + sumaDeVotos votosRestantes


votosEnBlanco2 :: [(String, String)] -> [Int] -> Int -> Int
votosEnBlanco2 formulas (v:vs) totalDeVotos | totalDeVotos > (cantVotos (v:vs)) = totalDeVotos - (cantVotos (v:vs))
                                            | otherwise = 0


-- Solucion de Matio, siento que se hizo mucho quilombo con los requiere.
--  Aca estamos poniendo la lista de los votos, y terminamos sumando la cantidad total de votos.
sumaDeVotos :: [Int] -> Int
sumaDeVotos [] = 0
sumaDeVotos (x:xs) = x + sumaDeVotos xs


-- Esto nos va a permitir comparar longitudes de listas.
longitud :: [t] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs


votosEnBlanco :: [(String,String)] -> [Int] -> Int -> Int
votosEnBlanco (x:xs) (y:ys) n | formulasValidas (x:xs) && longitud (x:xs) == longitud (y:ys) && sumaDeVotos (y:ys) <= n = n - sumaDeVotos (y:ys)


-- Ejercicio 2
-- Definimos la lista de "todosLosCandidatos", donde quedan expresados todos los candidatos pero no en tuplas.
-- Embeses es bueno escribir que representan las cosas y no hacerlo tan general.
todosLosCandidatos :: [(String,String)] -> [String]
todosLosCandidatos [] = []
todosLosCandidatos ((x1,y1):xs) = [x1,y1] ++ todosLosCandidatos xs


pertenece _ [] = False
pertenece a (x:xs) | a == x = True
                   | otherwise = pertenece a xs


formulasValidas :: [(String,String)] -> Bool
formulasValidas [] = False
formulasValidas [(x1,y1)] = x1 /= y1
formulasValidas ((x1,y1):xs) | pertenece x1 (y1:todosLosCandidatos xs) || pertenece y1 (x1:todosLosCandidatos xs) = False
                             | otherwise = formulasValidas xs

-- Ejercicio 3
division :: Int -> Int -> Float
division a b = (fromIntegral a) / (fromIntegral b)


porcentajeDeVotos1 :: String -> [(String, String)] -> [Int] -> Float
porcentajeDeVotos1 _ [] _ = 0
porcentajeDeVotos1 presidente ((x1,y1):xs) (v:vs)   | presidente == x1 = division (v*100) (cantVotos (v:vs))
                                                    | otherwise = porcentajeDeVotos1 presidente xs (vs ++ [v])


-- Solucion que hicimos con Mati, nuevamente nos la complicamos porque no tuvimos en cuenta lo que decia el enunciado
-- Porfiados
todosLosPresidentes :: [(String,String)] -> [String]
todosLosPresidentes [] = []
todosLosPresidentes ((x1,x2):xs) = x1:todosLosPresidentes xs


votoPositivo :: [Int] -> Bool
votoPositivo [] = False
votoPositivo (x:xs) | x > 0 = True
                    | otherwise = votoPositivo xs


posicionLista :: (Eq t) => t -> [t] -> Int
posicionLista _ [] = 0
posicionLista a (x:xs) | a == x = 1
                       | otherwise = 1 + posicionLista a xs


valorDeLaPosicion :: (Eq p) => Int -> [p] -> p
valorDeLaPosicion n (x:xs) | n == 1 = x
                           | otherwise = valorDeLaPosicion (n-1) xs


votoPresidente :: String -> [String] -> [Int] -> Int
votoPresidente a (x:xs) (y:ys) = valorDeLaPosicion (posicionLista a (x:xs)) (y:ys)

-- Nuevamente, se hace mucho quilombo con los requiere.
-- Los enuncia en las condiciones del codigo cuando eso no es necesario, porque sojn los parametros de entrada del codigo.
porcentajeDeVotos :: String -> [(String,String)] -> [Int] -> Float
porcentajeDeVotos _ [] _ = 0
porcentajeDeVotos a (x:xs) (y:ys) | pertenece a (todosLosPresidentes (x:xs)) && formulasValidas (x:xs) && longitud (x:xs) == longitud (y:ys) && votoPositivo (y:ys) = division (votoPresidente a (todosLosPresidentes (x:xs)) (y:ys)) (sumaDeVotos (y:ys)) * 100


-- Ejercicio 4
maximo :: Ord a => [a] -> a
maximo [x,y] | x <= y = y
             | otherwise = x
maximo (x:y:xs) | x <= y = maximo (y:xs)
                | otherwise = maximo (x:xs)


proximoPresidente :: [(String,String)] -> [Int] -> String
proximoPresidente (x:xs) (y:ys) = valorDeLaPosicion (posicionLista (maximo (y:ys)) (y:ys)) (todosLosPresidentes (x:xs))


