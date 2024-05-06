-- Ejercicio 1
componentesIguales :: (Eq t) => (t,t) -> Bool
componentesIguales (a,b) = a==b


inversa :: (a,b) -> (b,a)
inversa (a,b) = (b,a)


pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece a (x:xs) | a == x = True
                   | otherwise = pertenece a xs


relacionesValidas :: [(String, String)] -> Bool
relacionesValidas [] = True
relacionesValidas (x:xs) | componentesIguales x || pertenece x xs || pertenece (inversa x) xs = False
                         | otherwise = relacionesValidas xs


-- Ejercicio 2
todasLasPersonas :: [(String,String)] -> [String]
todasLasPersonas [] = []
todasLasPersonas ((x1,x2):xs) = [x1,x2] ++ todasLasPersonas xs


quitarTodo :: String -> [String] -> [String]
quitarTodo _ [] = []
quitarTodo a (x:xs) | a == x && pertenece a xs = quitarTodo a xs
                    | otherwise = x:quitarTodo a xs


ningunRepetido :: [String] -> [String]
ningunRepetido [] = []
ningunRepetido (x:xs) | pertenece x xs = ningunRepetido (quitarTodo x xs)
                      | otherwise = x:ningunRepetido xs

-- Ejercicio 3
personas :: [(String, String)] -> [String]
personas [] = []
personas (x:xs) = ningunRepetido (todasLasPersonas (x:xs))


perteneceTupla :: String -> (String,String) -> Bool
perteneceTupla a (x,y) = a==x || a==y


otraComponente :: String -> (String,String) -> String
otraComponente a (x,y) | a==x = y
                       | a==y = x


amigosDe :: String -> [(String, String)] -> [String]
amigosDe _ [] = []
amigosDe a (x:xs) | not (perteneceTupla a x) = amigosDe a xs
                  | otherwise = (otraComponente a x):amigosDe a xs


-- Ejercicio 4
--longitud :: [String] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs


listaDeAmigosDePersonas :: [String] -> [(String,String)] -> [[String]]
listaDeAmigosDePersonas [] _ = []
listaDeAmigosDePersonas (x:xs) (y:ys) = amigosDe x (y:ys):listaDeAmigosDePersonas xs (y:ys)


longitudEnLista :: [[String]] -> [Int]
longitudEnLista [] = []
longitudEnLista (xs:xss) = longitud xs:longitudEnLista xss


maximo :: [Int] -> Int
maximo [] = 0
maximo [x,y] | x<= y = y
             | otherwise = x
maximo (x:y:xs) | x <= y = maximo (y:xs)
                | otherwise = maximo (x:xs)

posicionMaximo :: Int -> [Int] -> Int
posicionMaximo _ [] = 0
posicionMaximo n (x:xs) | n == x = 1
                        | otherwise = 1 + posicionMaximo n xs

stringDeLaposicion :: Int -> [String] -> String
stringDeLaposicion n (x:xs) | n==1 = x
                            | otherwise = stringDeLaposicion (n-1) xs

personaConMasAmigos :: [(String, String)] -> String
personaConMasAmigos (x:xs) = stringDeLaposicion (posicionMaximo (maximo (magia)) magia) (personas (x:xs))
               where magia = longitudEnLista (listaDeAmigosDePersonas (personas (x:xs)) (x:xs))

